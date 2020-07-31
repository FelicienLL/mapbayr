library(shiny)
library(shinydashboard)
library(magrittr)
library(mapbayr)


ui <- dashboardPage(
  dashboardHeader(
    title = "mapbayr-TKI", titleWidth = "400px"
  ),
  dashboardSidebar(collapsed = T),
  dashboardBody(
    fluidRow(
      column(width = 3,
             fluidRow(
               box(title = "Modele", width = 12,
                   selectInput("MODEL",label = "Modele :",
                               selected = 'pazopanib_yu',
                               choices =
                                 list(PAZOPANIB =
                                        c("Yu et al, Clin Pharmacokinet, 2016"    = "pazopanib_yu",
                                          "Complex (These DES PH Felicien, 2019)" = "pazopanib_complex",
                                          "Simple (These DES PH Felicien, 2019)"  = "pazopanib_simple"
                                        ),
                                      CABOZANTINIB =
                                        c("Lacy et al, Cancer Chemother Pharmacol, 2018" = "cabozantinib_lacy"
                                        ),
                                      IBRUTINIB =
                                        c("Gallais et al, Clin Pharmacokinet, 2020" = "ibrutinib_gallais",
                                          "Marostica et al, Cancer Chemother Pharmacol, 2015" = "ibrutinib_marostica"
                                        ),
                                      IMATINIB =
                                        c("Delbaldo et al, Clin Cancer Res, 2006" = "imatinib_delbaldo"),
                                      NILOTINIB =
                                        c("Larson et al, Eur J Clin Pharmacol, 2012"     = "nilotinib_larson",
                                          "Giles et al, Eur J Clin Pharmacol, 2013"      = "nilotinib_giles"
                                        )
                                 )
                   )
               ),
               tabBox(title = "", id = "input_box", width = 12,
                      tabPanel(title = "Dose & Concentrations", width = 12,
                               fluidRow(
                                 column(width = 4,
                                        numericInput("AMT", "Dose (mg)", 60),
                                        numericInput("H_SAMPLE", "Temps Prelevement H + (h)",15, step = 1)
                                 ),
                                 column(width = 4,
                                        numericInput("II", "Intervalle (h)", 24),
                                        numericInput("DV", "Concent Parent",32)
                                 ),
                                 column(width = 4,
                                        numericInput("DUREE_TTT", "Duree TTT (j)", 20),
                                        numericInput("DV_met2", 'Concent Metab', value = NA)
                                 )
                               )
                      ),
                      tabPanel(title = "Covariables", width = 12,
                               fluidRow(
                                 column(width = 12,
                                        uiOutput("covariates")
                                 )
                               )
                      )

               ),
               box(title = "", width = 12,
                   actionButton("GO_ESTIM", "Estimation"),
                   selectInput("dose_regimens_to_simulate", "Schemas",
                               choices = c("rien", "updatedwith", "listofmodels"),
                               multiple = T),
                   fluidRow(
                     column(width = 6, actionButton("GO_SIM", "Simulation")),
                     column(width = 6, checkboxInput("ZOOM", label = "ZOOM", value = F))
                   )

               )
             )

      ),
      column(width = 9,
             tabBox(title = "", id = "upper_box", width = 12,
                    tabPanel(title = "Donnees", width = 12,
                             DT::dataTableOutput("table01")
                             # tableOutput("table01")
                    ),
                    tabPanel(title = "Estimations", width = 12,
                             fluidRow(
                               column(width = 7, plotOutput("pred_from_t0", height = "350px")),
                               column(width = 2, plotOutput("pred_from_t_last_dose", height = "350px")),
                               column(width = 3, plotOutput("plot_log_normal_distribution", height = "350px"))
                             )
                    ),
                    tabPanel(title = "Simulations", width = 12,
                             fluidRow(
                               column(width = 12, plotOutput("plot_sim", height = "350px"))
                             )
                    )
             ),
             box(title = "Resultats MOLIS", width = 12,
                 verbatimTextOutput("txt01")
             )
      )
    )
  )
)
