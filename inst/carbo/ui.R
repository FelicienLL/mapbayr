library(shiny)
library(shinydashboard)
library(rclipboard)
library(mapbayr)
library(magrittr)

ui <- dashboardPage(

  dashboardHeader(
    title = "mapbayr-CARBO", titleWidth = "400px"
  ),
  dashboardSidebar(collapsed = T),
  dashboardBody(
    rclipboardSetup(),
    fluidRow(
      column(
        width = 4,
        box(
          title = "Patient", width = 12,
          column(width = 4,
                 textInput("nom", "Nom", "")),
          column(width = 4,
                 textInput("prenom", "Prenom", "")),
          column(width = 4,
                 numericInput("bsa", "Surf corp. (m2)",value = 1.73, min = 1, max = 2, step = 0.01))
        ),
        box(
          title = "Protocole et Dose", width = 12,
          column(
            width = 4,
            numericInput("n_cycle", "n° Cycle", value = NA),
            numericInput("auc_cible", "AUC cible (3j, mg/L.min)", value = NA)
          ),
          column(
            width = 4,
            dateInput("date_adm", "Date administration", format = "dd/mm/yyyy", weekstart = 1, language = "fr",value = lubridate::today()-lubridate::days(1)),
            numericInput("amt", "Dose administree (mg)", value = NA, min = 0, max = 2000, step = 10)
          ),
          column(
            width = 4,
            textInput("h_debut_perf", "Heure debut perfusion", "" ),
            textInput("h_fin_perf", "Heure fin perfusion", "")
          ),
          verbatimTextOutput("text_perf")
        ),
        box(
          title = "Concentrations", width = 12,
          column(
            width = 4,
            textInput("num_LA_1", "Numero LA 1",""),
            textInput("num_LA_2", "Numero LA 2",""),
            textInput("num_LA_3", "Numero LA 3","")
          ),
          column(
            width = 3,
            textInput("h_prlvmt1", "Heure 1",""),
            textInput("h_prlvmt2", "Heure 2",""),
            textInput("h_prlvmt3", "Heure 3","")
          ),
          column(
            width = 3,
            numericInput("dv_1", "Conc 1 (mg/L)",NA),
            numericInput("dv_2", "Conc 2 (mg/L)",NA),
            numericInput("dv_3", "Conc 3 (mg/L)",NA)
          ),
          column(
            width = 2,
            strong("Exclure"),
            br(),
            checkboxInput("mdv1", NULL, F),
            br(),
            checkboxInput("mdv2", NULL, F),
            br(),
            checkboxInput("mdv3", NULL, F)
          )
        ),
        box(
          title = "Analyse",  width  = 12,
          column(width = 6, selectInput("MODEL", label = "Modele",
                                        choices = list(
                                          CARBOPLATINE = c("Adulte, 188 patients" = "carboplatine_adulte_188")
                                          )
                                        )
                 ),
          column(width = 6, selectInput("biologiste", "Biologiste", choices = list("Etienne CHATELUT",
                                                                           "Marie LAMBERT",
                                                                           "Felicien LE LOUEDEC",
                                                                           "Fabienne THOMAS")
                                        )
                 ),
          column(width = 2, actionButton("GO_ESTIM", "ESTIMER")),
          column(width = 3, uiOutput("copy1")),
          column(width = 4, uiOutput("copy2")),
          column(width = 3, uiOutput("download"))
        )
      ),
      column(
        width = 8,
        box(
          title = "NM-TRAN like input", width = 7,
          tableOutput("table_nonmem")
        ),
        box(
          title = "Parametres", width = 5,
          plotOutput("figure_distribution_param",height = "175px")
        ),
        box(
          title = "Courbes", width = 12,
          plotOutput("figure_concentration_temps",height = "250px")
        ),
        box(
          title = "Resultats",width = 12,
          verbatimTextOutput("text_resultat"),
          verbatimTextOutput("text_commentaire")
        )
      )
    )
  )
)
