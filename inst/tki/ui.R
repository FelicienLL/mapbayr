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
               box(title = "Modele", width = 12, uiOutput("MODEL"), actionButton("LOAD", "LOAD")
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
