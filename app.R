# global
library(shiny)
library(shinyjs)
library(sf)
library(tmap) # Package de carto
library(phacochrdev)
library(stringr)
library(tidyverse)
library(fuzzyjoin)
library(readxl)

#source("Script géocodage/Script de géocodage parallel.R")

options(shiny.maxRequestSize = 300*1024^2)

# ui
ui <- navbarPage(title = " ",
                 # PAGE IMPORTATION DES DONNÉES
                 tabPanel(title = "Importation des données",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("fileType_Input", "Type de fichier à géocoder",
                                             choices = list(".csv" = 1, ".xlsx" = 2),
                                             selected = 1, inline = TRUE),
                                fileInput("file1", "Fichier à géocoder (max 300 MB)",
                                          multiple = TRUE,
                                          accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.xlsx')),
                                checkboxInput("header", "Entête (header)", TRUE),
                                radioButtons("sep", "Separateur de champs",
                                             choices = c(`,` = ",", `;` = ";", `|` = "|", Tab = "\t", `~` = "~"),
                                             selected = ";", inline = TRUE),
                                radioButtons("quote", "Guillement pour les champs texte",
                                             choices = c("Pas de guillemet" = "", `Doubles guillements ( \"  \" )` = '"', "Simples guillements ( '  ' )" = "'"),
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
                            shinyjs::useShinyjs(), # Activer shinyjs
                          sidebarLayout(
                            sidebarPanel(
                              h4("Champs adresse"),
                              h6("Sélectionnez les champs avec les adresses:"),
                              fluidRow(
                                column(5,
                                       selectInput(inputId = "colonne_num", label = "Numéro", choices = NULL),
                                       selectInput(inputId = "colonne_rue", label = "Rue", choices = NULL),
                                       selectInput(inputId = "colonne_code_postal", label = "Code postal", choices = NULL)),
                                column(7,
                                       selectInput(inputId = "colonne_num_rue", label = "Numéro + Rue", choices = NULL),
                                       selectInput(inputId = "colonne_rue_code_postal", label = "Rue + Code postal", choices = NULL),
                                       selectInput(inputId = "colonne_num_rue_code_postal", label = "Numéro + Rue + Code postal", choices = NULL)
                                        )),
                              h6("Notes :"),
                              h6("- Toutes les options ne doivent pas être remplies."),
                              h6("- Il est préférable d'avoir des champs séparés pour chacune des informations (numéro, rue, code postal)."),
                              h6("- Les mêmes informations (numéro, rue, code postal) ne peuvent être reprises dans plusieurs champs."),
                              actionButton("geocode", "Lancer le géocodage",
                                           style = "
                                           font-size: 20px;
                                           padding: 10px 20px;"),
                              actionLink("toggle_advanced", h4("Options Avancées")),
                              shinyjs::hidden(
                                div(id = "advanced_options",
                                    fluidRow(

                                             selectInput("method_stringdist", "Méthode pour la jointure inexacte",
                                                         choices = c("lcs (par défaut)" = "lcs", "osa", "lv", "dl",
                                                                     "hamming", "qgram", "cosine", "jaccard", "jw", "soundex"),
                                                         selected = "lcs"),
                                             numericInput(inputId = "error_max",
                                                          label = "Nombre d'erreurs maximum acceptées par la jointure probabiliste (conseillé : 2-4)", value = 4),
                                             checkboxInput("elargissement_com_adj", "Elargir aux communes adjacentes", TRUE),
                                             numericInput(inputId = "error_max_elarg", label = "Nombre d'erreurs maximum pour les codes postaux étendus", value = 4),
                                             numericInput(inputId = "approx_num", label = "Approximation pour la géolocalisation (numéros voisins)", value = 50),
                                             checkboxInput("mid_street", "Prendre le numéro du milieu de la rue si le numéro n'est pas trouvé", TRUE),

                                             checkboxInput("corrections_REGEX", "Correction orthographique des adresses", TRUE),
                                             checkboxGroupInput("lang_encoded", "Langue des adresses", choices = c("FR", "NL", "DE"), selected = c("FR", "NL", "DE"), inline = TRUE)
                                    )
                                ))),
                            mainPanel(
                              h4("Géocodage"),
                              shinycssloaders::withSpinner(tableOutput("summary"), type = 3, color = "#636363", color.background = "#ffffff", size = 0.8)
                            ))),

                 tabPanel(title = "Cartes",
                          fluidPage(
                            shinycssloaders::withSpinner(tmapOutput(outputId = "tmapMap", width = "100%", height = 800),
                                                         type = 3, color = "#636363", color.background = "#ffffff", size = 0.8)
                          )),

                 tabPanel(title = "Export",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h4("Export"),
                                h6("Attention: seul le format csv garde toutes les données, y compris celles non géocodées."),
                                radioButtons("fileType_output", label = "Extension du fichier",
                                             choices = list(".csv" = ".csv", ".gpkg" = ".gpkg", ".kml" = ".kml", ".geojson" = ".geojson"),
                                             selected = ".csv", inline = TRUE),
                                downloadButton("downloadData", "Télécharger le fichier géocodé")
                              ),
                              mainPanel(
                                h4("Aperçu des données géocodées"),
                                shinycssloaders::withSpinner(tableOutput("head_data_geocoded"), type = 3, color = "#636363", color.background = "#ffffff", size = 0.8)
                              )))))

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

    phaco_geocode(
      data_to_geocode(),
      colonne_rue = colonne_rue,
      colonne_num = colonne_num,
      colonne_code_postal = colonne_code_postal,
      colonne_num_rue = colonne_num_rue,
      colonne_num_rue_code_postal = colonne_num_rue_code_postal,
      colonne_rue_code_postal = colonne_rue_code_postal,
      method_stringdist = input$method_stringdist,
      corrections_REGEX = input$corrections_REGEX,
      error_max = input$error_max,
      error_max_elag = input$error_max_elarg,
      approx_num_max = input$approx_num,
      elargissement_com_adj = input$elargissement_com_adj,
      mid_street = input$mid_street,
      lang_encoded = input$lang_encoded,
      anonymous = FALSE,
      path_data = "data_phacochr/"
    )
  })

  output$summary <- renderTable({ req(result())$summary })
  output$head_data_geocoded <- renderTable({ head(result()$data_geocoded) })

  output$downloadData <- downloadHandler(
    filename = function() { paste0(str_remove(input$file1$name,"\\.[^\\.]*$"), "_phacochr", input$fileType_output) },
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
    FULL_GEOCODING_sf<-  result()$data_geocoded_sf
    source("script/Carto des points géocodés - tmap interactif_6_14.R", local = TRUE)
    Carto_map
  })
}

# Create Shiny app ----
shinyApp(ui, server)


