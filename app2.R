# global
library(shiny)
library(shinyjs)
library(sf)
library(tmap) # Package de carto
library(phacochr)
library(stringr)
library(tidyverse)
library(fuzzyjoin)
library(readxl)

options(shiny.maxRequestSize = 300 * 1024^2)

# ui
ui <- navbarPage(title = " ",
                 # PAGE IMPORTATION DES DONNÉES
                 tabPanel(title = "Importation des données",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("fileType_Input", "Type de fichier à géocoder",
                                             choices = list(".csv" = 1, ".xlsx" = 2), selected = 1, inline = TRUE),
                                fileInput("file1", "Fichier à géocoder (max 300 MB)", multiple = TRUE,
                                          accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.xlsx')),
                                checkboxInput("header", "Entête (header)", TRUE),
                                radioButtons("sep", "Séparateur de champs",
                                             choices = c(`,` = ",", `;` = ";", `|` = "|", Tab = "\t", `~` = "~"),
                                             selected = ";", inline = TRUE),
                                radioButtons("quote", "Guillements pour les champs texte",
                                             choices = c("Pas de guillemet" = "", `Doubles guillements` = '"', "Simples guillements" = "'"),
                                             selected = "")
                              ),
                              mainPanel(
                                h4("Aperçu des données à géocoder"),
                                tableOutput("head_data_to_geocode")
                              )
                            )
                          )),
                 # PAGE GEOCODAGE
                 tabPanel(title = "Géocodage",
                          fluidPage(
                            shinyjs::useShinyjs(), # Activer shinyjs
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(
                                  column(6,
                                         h4("Champs adresse"),
                                         h6("Sélectionnez les champs avec les adresses:"),
                                         selectInput("colonne_num", "Numéro", choices = NULL),
                                         selectInput("colonne_rue", "Rue", choices = NULL),
                                         selectInput("colonne_code_postal", "Code postal", choices = NULL),
                                         tags$hr(),
                                         selectInput("colonne_num_rue", "Numéro + Rue", choices = NULL),
                                         selectInput("colonne_num_rue_code_postal", "Numéro + Rue + Code postal", choices = NULL),
                                         selectInput("colonne_rue_code_postal", "Rue + Code postal", choices = NULL),
                                         h6("Notes :"),
                                         h6("- Toutes les options ne doivent pas être remplies."),
                                         h6("- Il est préférable d'avoir des champs séparés pour chaque information (numéro, rue, code postal)."),
                                         tags$hr(),
                                         actionButton("geocode", "Lancer le géocodage")
                                  ),
                                  column(6,
                                         actionLink("toggle_advanced", h4("Options Avancées")),
                                         shinyjs::hidden(
                                           div(id = "advanced_options",
                                               selectInput("method_stringdist", "Méthode pour la jointure inexacte",
                                                           choices = c("lcs (par défaut)" = "lcs", "osa", "lv", "dl", "hamming", "qgram", "cosine", "jaccard", "jw", "soundex"),
                                                           selected = "lcs"),
                                               numericInput("error_max", "Nombre d'erreurs max (2-4)", value = 4),
                                               numericInput("error_max_elarg", "Erreur max pour codes postaux adjacents (2-4)", value = 4),
                                               numericInput("approx_num", "Approximation max pour géolocalisation", value = 50),
                                               checkboxInput("mid_street", "Prendre le numéro du milieu si non trouvé", TRUE),
                                               tags$hr(),
                                               checkboxGroupInput("lang_encoded", "Langue des adresses",
                                                                  choices = c("FR", "NL", "DE"), selected = c("FR", "NL", "DE"), inline = TRUE),
                                               tags$hr(),
                                               checkboxInput("corrections_REGEX", "Correction orthographique", TRUE),
                                               checkboxInput("elargissement_com_adj", "Recherche élargie aux communes adjacentes", TRUE)

                                           )
                                         )
                                  )
                                )
                              ),
                              mainPanel(
                                h4("Géocodage"),
                                shinycssloaders::withSpinner(tableOutput("summary"), type = 3, color = "#636363", color.background = "#ffffff", size = 0.8)
                              )
                            )
                          )),
                 tabPanel(title = "Cartes",
                          fluidPage(
                            shinycssloaders::withSpinner(tmapOutput("tmapMap", width = "100%", height = 800), type = 3, color = "#636363", color.background = "#ffffff", size = 0.8)
                          )),
                 tabPanel(title = "Export",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("fileType_output", label = tagList("Extension du fichier",
                                                                                tags$small("Attention: l'export en format géographique (gpkg, kml, geojson) ne conserve que les points trouvés. Pour garder l'ensemble des données, dont les adresses non trouvées, il faut exporter en .csv .")),
                                             choices = list(".csv" = ".csv", ".gpkg" = ".gpkg", ".kml" = ".kml", ".geojson" = ".geojson"),
                                             selected = ".csv", inline = TRUE),
                                downloadButton("downloadData", "Télécharger le fichier géocodé")
                              ),
                              mainPanel(
                                h4("Aperçu des données géocodées"),
                                shinycssloaders::withSpinner(tableOutput("head_data_geocoded"), type = 3, color = "#636363", color.background = "#ffffff", size = 0.8)
                              )
                            )
                          ))
)

# Define server logic
server <- function(input, output, session) {
  # PAGE 1 : IMPORTATION ET AFFICHAGE
  data_to_geocode <- reactive({
    if (is.null(input$file1)) {
      data.frame(nom = c("Observatoire de la Santé et du Social", "ULB"),
                 rue = c("rue Beilliard", "avenue Antoine Depage"),
                 num = c("71", "30"),
                 code_postal = c("1040", "1000"))
    } else if (input$fileType_Input == 1) {
      read_delim(input$file1$datapath, delim = input$sep, quote = input$quote, trim_ws = TRUE)
    } else {
      read_excel(input$file1$datapath, col_types = "text")
    }
  })

  output$head_data_to_geocode <- renderTable({ head(data_to_geocode()) })

  # Mise à jour des choix pour les champs d'adresse
  observe({
    vars <- c("-", names(data_to_geocode()))
    lapply(c("colonne_num", "colonne_rue", "colonne_code_postal", "colonne_num_rue", "colonne_num_rue_code_postal", "colonne_rue_code_postal"),
           function(x) updateSelectInput(session, x, choices = vars))
  })

  # Toggle options avancées
  observeEvent(input$toggle_advanced, {
    shinyjs::toggle("advanced_options")
  })

  # Lancer le géocodage
  result <- eventReactive(input$geocode, {
    colonne_rue <- if (input$colonne_rue == "-") NULL else input$colonne_rue
    colonne_num <- if (input$colonne_num == "-") NULL else input$colonne_num
    colonne_code_postal <- if (input$colonne_code_postal == "-") NULL else input$colonne_code_postal
    colonne_num_rue <- if (input$colonne_num_rue == "-") NULL else input$colonne_num_rue
    colonne_num_rue_code_postal <- if (input$colonne_num_rue_code_postal == "-") NULL else input$colonne_num_rue_code_postal
    colonne_rue_code_postal <- if (input$colonne_rue_code_postal == "-") NULL else input$colonne_rue_code_postal

    phaco_geocode(data_to_geocode(), colonne_rue, colonne_num, colonne_code_postal, colonne_num_rue,
                  colonne_num_rue_code_postal, colonne_rue_code_postal, input$method_stringdist,
                  input$corrections_REGEX, input$error_max, input$approx_num, input$elargissement_com_adj,
                  input$lang_encoded, input$mid_street, path_data = "data_phacochr/")
  })

  output$summary <- renderTable({ req(result())$summary })
  output$head_data_geocoded <- renderTable({ head(result()$data_geocoded) })

  output$downloadData <- downloadHandler(
    filename = function() { paste0(input$file1$name, "_phacochr", input$fileType_output) },
    content = function(file) {
      if (input$fileType_output == ".csv") {
        write.csv(result()$data_geocoded, file, row.names = FALSE)
      } else {
        st_write(result()$data_geocoded_sf, file)
      }
    }
  )

  output$tmapMap <- renderTmap({
    req(result()$data_geocoded_sf)
    source("script/Carto des points géocodés - tmap interactif_6_14.R", local = TRUE)
    Carto_map
  })
}

# Create Shiny app ----
shinyApp(ui, server)
