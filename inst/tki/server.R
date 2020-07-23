server <- function(input, output, session){

  #########################
  ###         UI        ###
  #########################

  output$covariates <- renderUI({

    createSelect <- function(name, descr, value){
      purrr::pmap(.l = list(.x = name, .y = descr,  .z = value),
                  .f = function(.x, .y , .z){
                    numericInput(inputId = .x, label = .y, value = .z)
                  })
    }

    cn <- re_model()$covariate_names
    cn <- cn[!cn%in%c("AOLA", "TOLA")]

    cd <- re_model()$covariate_description
    cd <- cd[re_model()$covariate_names %in%  cn]
    cd <- paste(cn, ':', cd)

    cv <- re_model()$covariate_ref_values
    cv <- cv[names(cv)%in%cn]

    createSelect(name  = cn, descr = cd, value = cv)

  })


  outputOptions(output, "covariates", suspendWhenHidden = FALSE)

  #########################
  ###  Reactive values  ###
  #########################

  re_model <- reactive({
    load_mapbay_model(model = input$MODEL)
  })

  re_interpretation <- reactive({
    interpretation_tki()[[(re_model()$drug)]]
  })

  re_covariates <- reactive({
    a <- re_model()$covariate_names
    names(a) <- a
    purrr::map(a, function(x){input[[x]]})
  })



  re_input_data <- reactive({
    adm <- adm_lines(re_model(),
                     addl = input$DUREE_TTT*(24/input$II),
                     ii = input$II,
                     amt = input$AMT)

    DV_vec <-  if(!is.na(input$DV_met2)){
      c(input$DV, input$DV_met2)
    } else{
      input$DV
    }

    obs <- obs_lines(re_model(),
                     time =  input$H_SAMPLE,
                     DV =  DV_vec
    )

    d <-  build_input_dataset(obs, adm) %>%
      add_rate_column(model = re_model()) %>%
      add_covariates(model = re_model(), covariates = re_covariates())

    return(d)
  })

  re_time_last_dose <- reactive({

    time_last_dose(re_input_data())
  })

  re_time_target <- reactive({
    time_target(re_input_data())
  })
  #########################
  ###   Event Reactive  ###
  ###     Estimation    ###
  #########################

  re_estimates <- eventReactive(input$GO_ESTIM, {

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Estimation en cours", value = 0.6)

    mapbay_estimation(data = re_input_data(),
                      model = re_model())

  })


  re_pred <- reactive({
   compute_prediction(data = isolate(re_input_data()),
                       estimates = re_estimates(),
                       model = isolate(re_model()),
                       time_target = isolate(re_time_target()))
  })

  #########################
  ###   Event Reactive  ###
  ###     Simulation    ###
  #########################

  re_sim <- eventReactive(input$GO_SIM, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Simulations en cours", value = 0.8)

    vector_of_amt_to_simulate <- re_interpretation()$sim_dose_regimens %>%
      dplyr::filter(choice %in% input$dose_regimens_to_simulate) %>%
      dplyr::pull(amt)

    vector_of_ii_to_simulate  <- re_interpretation()$sim_dose_regimens %>%
      dplyr::filter(choice %in% input$dose_regimens_to_simulate) %>%
      dplyr::pull(ii)

    purrr::map2_dfr(
      .x = vector_of_amt_to_simulate,
      .y = vector_of_ii_to_simulate,
      .f = compute_simulation,
      estimates = re_estimates(),
      model = re_model(),
      covariates = re_covariates()
    ) %>%
      dplyr::mutate_at("DOSING", as.factor)
  })



  #########################
  ###       Outputs     ###
  ###        Plots      ###
  #########################

  # plot 01 : Concentration vs time since the first administration

  output$pred_from_t0 <- renderPlot({

    plot_pred_mapbay(pred_data  = re_pred(),
                     input_data = isolate(re_input_data()),
                     model      = isolate(re_model()),
                     hline      = isolate(re_interpretation()$concentration_target)
    )
  })

  # plot 02 : Concentration vs time since the last administration

  output$pred_from_t_last_dose <- renderPlot({

    plot_pred_mapbay(pred_data  = re_pred(),
                     input_data = isolate(re_input_data()),
                     model = isolate(re_model()),
                     from = isolate(re_time_last_dose()),
                     hline = isolate(re_interpretation()$concentration_target)
    )
  })

  # plot 03 : Theoretical log-normal distribution of random effects

  output$plot_log_normal_distribution <- renderPlot({
    purrr::pmap(
      .l = list(
        PAR = isolate(re_model()$param_names),
        TV =  isolate(re_model()$param_typical_values),
        OM = diag(isolate(re_model()$param_omega_matrix)),
        EBE = re_estimates()[["final_eta"]],
        UNIT = isolate(re_model()$param_units)
      ),
      .f = plot_log_normal_distribution
    ) %>%
      ggpubr::ggarrange(plotlist = .)
  })

  # plot 04 : Simulation

  output$plot_sim <- renderPlot({

    plot_sim_tki(sim_data = re_sim(),
                 model = isolate(re_model()),
                 interpretation_tki = isolate(re_interpretation()),
                 zoom = input$ZOOM)
  })

  #########################
  ###       Outputs     ###
  ###        Text       ###
  #########################


  output$txt01 <- renderText({



    c(write_comment_fit_tki(pred_data   = re_pred(),
                            estimates = re_estimates(),
                          model     = isolate(re_model()),
                            time_last_dose = isolate(re_time_last_dose()),
                            time_target    = isolate(re_time_target())),

      "\n\n" ,
      write_comment_biology_tki(pred_data   = re_pred(),
                                model       = isolate(re_model()),
                                interpretation_tki = isolate(re_interpretation()),
                                time_target = isolate(re_time_target()))
    )

  })

  #########################
  ###       Outputs     ###
  ###        Table      ###
  #########################

  output$table01 <- DT::renderDataTable(
    print_data_tki(re_input_data()),
    options = list(scrollY = "350px",
                   searching = F,
                   scrollCollapse = TRUE,
                   paging = FALSE)
  )
  #########################
  ### Observe behaviour ###
  #########################


  observeEvent(input$MODEL, {
    updateTabsetPanel(session, "upper_box", selected = "Donnees")
  })

  observeEvent(input$GO_ESTIM, {
    updateTabsetPanel(session, "upper_box", selected = "Estimations")
  })

  observeEvent(input$GO_SIM, {
    updateTabsetPanel(session, "upper_box", selected = "Simulations")
  })

  observe({
    updateSelectInput (session, "MODEL"                    , label   = paste0("Modele ", re_model()$drug))
    updateNumericInput(session, "DV"                       , label   = paste0("Concent Parent (",re_model()$concentration_unit, ")"))
    updateNumericInput(session, "DV_met2"                  , label   = paste0("Concent Metab (",re_model()$concentration_unit, ")"))
    updateSelectInput (session, "dose_regimens_to_simulate", choices = (re_interpretation()$sim_dose_regimens) %>% dplyr::pull(choice))
  })

  session$onSessionEnded(function() {
    stopApp()
  })

  #end
}




