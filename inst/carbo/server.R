server <- function(input, output, session){

  #########################
  ###  Reactive values  ###
  #########################

  re_model <- reactive({
    load_mapbay_model(model = input$MODEL)
  })

  re_rate <- reactive({
    shiny::req(input$amt, input$h_debut_perf, input$h_fin_perf)

    rate_duration(input$amt, input$h_debut_perf, input$h_fin_perf)
  })

  re_input_data <- reactive({
    shiny::req(input$amt, input$h_debut_perf, input$h_fin_perf)
    shiny::req(input$h_prlvmt1, input$h_prlvmt2, input$h_prlvmt3)
    shiny::req(input$dv_1, input$dv_2, input$dv_3)

    adm <- adm_lines(model = re_model(),
                     amt = input$amt
    )

    obs <- obs_lines(model = re_model(),
                     time = hour_to_time(input$h_debut_perf, input$h_prlvmt1, input$h_prlvmt2, input$h_prlvmt3),
                     DV = c(input$dv_1, input$dv_2, input$dv_3),
                     mdv = as.numeric(c(input$mdv1,input$mdv2,input$mdv3))
    )

    build_input_dataset(obs, adm) %>%
      add_rate_column(model = re_model(), rate = re_rate()[["rate"]]) %>%
      add_covariates(model = re_model(), list(BSA = input$bsa))

  })


  #########################
  ###   Event Reactive  ###
  ###     Estimation    ###
  #########################

  re_estimates <- eventReactive(input$GO_ESTIM, {

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Estimation en cours", value = 0.6)

    mapbay_estimation(
      data = re_input_data(),
      model = re_model()
    )

  })

  re_adaptation <- reactive({
    req(re_estimates())
    adaptation_tice(
      data = isolate(re_input_data()),
      estimates = re_estimates(),
      model = isolate(re_model()),
      auc_cible = isolate(input$auc_cible)
    )}
  )

  re_figure_concentration_temps_fit <- reactive({
    req(re_estimates())
    plot_pred_mapbay(
      pred_data = re_adaptation()$pred_data,
      input_data = isolate(re_input_data()),
      model = isolate(re_model()),
      from = 0,
      to = max(isolate(re_input_data())$time)+1
    )
  })

  re_figure_concentration_temps_cycle <- reactive({
    req(re_estimates())
    plot_pred_mapbay(
      pred_data = re_adaptation()$pred_data,
      input_data = isolate(re_input_data()),
      model = isolate(re_model()),
      from = 0,
      to = 72
    )
  })

  re_figure_distribution_param <- reactive({
   browser()
     purrr::pmap(
      .l = list(
        PAR = isolate(re_model()$param_names),
        TV =  isolate(re_model()$param_typical_values) * c(1, isolate(input$bsa), 1),
        OM = diag(isolate(re_model()$param_omega_matrix)),
        EBE = re_estimates()[["final_eta"]],
        UNIT = isolate(re_model()$param_units)
      ),
      .f = plot_log_normal_distribution
    ) %>%
      ggpubr::ggarrange(plotlist = .)

  })

  re_text_resultat <- reactive(
    write_result_tice(
      re_adaptation(),
      n_cycle = isolate(input$n_cycle)
    )
  )

  re_text_commentaire <- reactive(
    write_comment_tice(
      re_adaptation(),
      n_cycle = isolate(input$n_cycle),
      auc_cible = isolate(input$auc_cible),
      date_adm = isolate(input$date_adm)
    )
  )

  re_table_nonmem <- reactive({
    if(input$GO_ESTIM==0){

      return(print_data_tice(re_input_data(), NA))

    } else{

      return(print_data_tice(re_input_data(), re_estimates()))
    }
  })

  re_tous_les_inputs <- reactive({
    reactiveValuesToList(input)
  })


  ######       OUTPUT
  output$text_perf <- renderText({

    paste0("Temps de perf = ", round(re_rate()$duration ,2), "h. Vitesse de perf = ", round(re_rate()$rate ,2) ,"mg/h")

  })

  output$text_resultat <- renderText( re_text_resultat() )

  output$text_commentaire <- renderText( re_text_commentaire() )

  output$table_nonmem <- renderTable( re_table_nonmem(), na = ".")

  output$figure_concentration_temps <- renderPlot(

    ggpubr::ggarrange(re_figure_concentration_temps_fit(),
                      re_figure_concentration_temps_cycle(),
                      ncol = 2, nrow = 1))

  output$figure_distribution_param <- renderPlot( re_figure_distribution_param() )

  output$copy1 <- renderUI({
    rclipButton("copy_result", "Copier Resultat", re_text_resultat(), icon("clipboard"))
  })

  output$copy2 <- renderUI({
    rclipButton("copy_comm", "Copier Commentaire", re_text_commentaire(), icon("clipboard"))
  })

  output$archiver <- downloadHandler(

    filename = function(){

      a <-  paste(stringr::str_sub(input$nom, 1, 3),
                  stringr::str_sub(input$prenom, 1, 2),
                  Sys.Date(),
                  ".doc",
                  sep = "_")
      a
    },

    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "rapport_tice.Rmd")
      file.copy("rapport_tice.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(table_nonmem = re_table_nonmem(),
                     tous_les_inputs = re_tous_les_inputs(),
                     biologiste = input$biologiste,
                     adaptation = re_adaptation(),
                     figure_concentration_temps_fit = re_figure_concentration_temps_fit(),
                     figure_concentration_temps_cycle = re_figure_concentration_temps_cycle(),
                     figure_distribution_param = re_figure_distribution_param(),
                     text_resultat = re_text_resultat(),
                     text_commentaire = re_text_commentaire()
      )

      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$download <- renderUI({
    if(!is.null(re_estimates())){
      downloadButton('archiver', 'ArchiveR')
    }
  })

  session$onSessionEnded(function() {
    stopApp()
  })

}

