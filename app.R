# global
library(shiny)
library(sf)
library(tmap) # Package de carto
library(phacochr)
library(stringr)
library(tidyverse)
library(fuzzyjoin)
library(readxl)


#source("Script géocodage/Script de géocodage parallel.R")

options (shiny.maxRequestSize=300*1024^2)
# ui
ui <- navbarPage(title = " ",
                 # PAGE IMPORTATION DES DONNÉES
                 tabPanel(title = "Importation des données",
                          fluidPage(
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(

                              # Sidebar panel for inputs ----
                              sidebarPanel(

                                # Choisir le format du fichier en entrée
                                radioButtons(
                                  "fileType_Input",
                                  label = "Type de fichier à géocoder",
                                  choices = list(".csv" = 1, ".xlsx" = 2),
                                  selected = 1,
                                  inline = TRUE
                                ),

                                # Input: Select a file ----
                                fileInput("file1", "Fichier à géocoder (max 300 MB)",
                                          multiple = TRUE,
                                          accept = c('text/csv',
                                                     'text/comma-separated-values,text/plain',
                                                     '.csv',
                                                     '.xlsx')),

                                # Horizontal line ----
                                #tags$hr(),

                                # Input: Checkbox if file has header ----
                                checkboxInput("header", "Entête (header)", TRUE),

                                # Input: Select separator ----
                                radioButtons("sep", "Separateur de champs",
                                             choices = c(`,` = ",",
                                                         `;` = ";",
                                                         `|` = "|",
                                                         Tab = "\t",
                                                         `~` = "~"),
                                             selected = ";",
                                             inline= T),

                                # Input: Select quotes ----
                                radioButtons("quote", "Guillement pour les champs texte",
                                             choices = c("Pas de guillemet" = "",
                                                         `Doubles guillements ( "  " )` = '"',
                                                         "Simples guillements ( '  ' )" = "'"),
                                             selected = "")
                              ),

                              # Main panel for displaying outputs ----
                              mainPanel(
                                h4("Aperçu des données à géocoder"),
                                # Output: Data file ----
                                tableOutput("head_data_to_geocode")

                              )

                            )
                          )),
                 # PAGE GEOCODAGE
                 tabPanel(title = "Géocodage",
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(

                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                fluidRow(
                                  column(6,

                                # Input: Select variables ----
                                h4("Champs adresse"),
                                h6("Sélectionnez les champs avec les adresses:"),
                                selectInput(inputId = "colonne_num", label = "Numéro", choices = NULL),
                                selectInput(inputId = "colonne_rue", label = "Rue", choices = NULL),
                                selectInput(inputId = "colonne_code_postal", label = "Code postal", choices = NULL),
                                # Horizontal line ----
                                tags$hr(),
                                selectInput(inputId = "colonne_num_rue", label = "Numéro + Rue", choices = NULL),
                                selectInput(inputId = "colonne_num_rue_code_postal", label = "Numéro + Rue + Code postal", choices = NULL),
                                selectInput(inputId = "colonne_rue_code_postal", label = "Rue + Code postal", choices = NULL),
                                h6("Notes :"),
                                h6("- Toutes les options ne doivent pas être remplie."),
                                h6("- Il est préférable d'avoir des champs séparés pour chacune des informations (numéro, rue, code postal)."),
                                h6("- Les mêmes informations (numéro, rue, code postal) ne peuvent être reprises dans plusieurs champs. "),
                                                                  ),
                                column(6,
                                h4("Options Avancées"),
                                # Input: Select méthode ----
                                selectInput("method_stringdist", "Méthode pour la jointure inexacte",
                                             choices = c("lcs (par défaut)"="lcs", "osa", "lv", "dl", "hamming", "qgram", "cosine", "jaccard", "jw","soundex"),
                                             selected = "lcs"),
                                # Input: Nombre d'erreur ----
                                numericInput(inputId = "error_max",
                                             label = "Nombre d'erreurs maximum acceptées par la jointure probabiliste (conseillé : 2-4)",
                                             value = 4),
                                # Input: Nombre d'erreur ----
                                numericInput(inputId = "error_max_elarg",
                                             label = "Nombre d'erreurs maximum acceptées par la jointure probabiliste lors de l'élargissement aux codes postaux de la commune et de ceux des communes adjacentes (conseillé : 2-4)",
                                             value = 4),
                                # Input: Approximation numéro ----
                                numericInput(inputId = "approx_num",
                                             label = "Approximation maximale autorisée pour la géolocalisation du batiment - en nombre de numéros à gauche ou à droite",
                                             value = 50),
                                # Input: mid num ----
                                checkboxInput("mid_street", "Prendre le numéro du milieu de la rue si le numéro n'est pas trouvé", TRUE),
                                # Horizontal line ----
                                tags$hr(),
                                  # Input: Select langue ----
                                checkboxGroupInput("lang_encoded", "Langue des adresses (augmente la rapidité du géocodage)",
                                             choices = c("FR","NL","DE"),
                                             selected =  c("FR","NL","DE"), inline = TRUE),
                                # Horizontal line ----
                                tags$hr(),
                                # Input: corrections orthographiques ----
                                checkboxInput("corrections_REGEX", "Correction orthographique des adresses", TRUE),
                                # Input: élargissement ----
                                checkboxInput("elargissement_com_adj", "Elargir la recherche des rues non trouvées aux communes adjacentes", TRUE),
                               # Horizontal line ----
                                tags$hr(),
                                actionButton("geocode", "Lancer le géocodage"),
)), width=4)
                                ,
                              # Main panel for displaying outputs ----
                              mainPanel(
                                #shinyjs::useShinyjs(),
                                h4("Géocodage"),
                                # Output: Data file ---- Summary_full
                                shinycssloaders::withSpinner(tableOutput("summary"),type = 3, color = "#636363", color.background ="#ffffff", size = 0.8),
                                #textOutput("cat")
                              ))),
                 tabPanel(title = "Cartes",
                          fluidPage(
                            shinycssloaders::withSpinner(tmapOutput(outputId = "tmapMap", width = "100%", height = 800)
                                                         ,type = 3, color = "#636363", color.background ="#ffffff", size = 0.8)

                          )),
                 tabPanel(title = "Export",
                          fluidPage(
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(

                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                radioButtons(
                                  "fileType_output",
                                  label = tagList(
                                    "Extension du fichier",
                                    tags$small(
                                      "Attention: l'export en format géographique (gpkg, kml, geojson) ne conserve que les points trouvés. Pour garder l'ensemble des données dont les adresses non trouvées, il faut exporter en .csv ."
                                    )
                                  ),
                                  # label = "Extension du fichier\n Attention l'export en format géographique (gpkg, kml, geijson) ne conserve que les points trouvés. Pour garder l'ensemble des données en entrées il faut exporter en .csv",
                                  choices = list(".csv" = ".csv",
                                                 #".xlsx" = ".xlsx",
                                                 ".gpkg"=".gpkg",
                                                 ".kml"= ".kml",
                                                 ".geojson"=".geojson"
                                  ),
                                  selected = ".csv",
                                  inline = TRUE
                                ),
                                downloadButton("downloadData", "Télécharger le fichier géocodé")

                              ),
                              # Main panel for displaying outputs ----
                              mainPanel(
                                h4("Aperçu des données géocodées"),
                                # Output: Data file ---- Summary_full
                                shinycssloaders::withSpinner(tableOutput("head_data_geocoded")
                                                             ,type = 3, color = "#636363", color.background ="#ffffff", size = 0.8)
                              )))),


)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  # PAGE 1 : IMPORTATION ET AFFICHAGE
  # importation des données

    data_to_geocode<- reactive({
    if (is.null(input$file1)) {  data.frame(nom= c("Observatoire de la Santé et du Social", "ULB"),
                                                                          rue= c("rue Beilliard","avenue Antoine Depage"),
                                                                          num=c("71", "30"),
                                                                          code_postal=c("1040","1000"))}
    else if(input$fileType_Input == 1) {read_delim(input$file1$datapath, delim = input$sep,quote = input$quote,trim_ws = TRUE)}
    else if(input$fileType_Input == 2) {read_excel(input$file1$datapath,col_types= "text")
    }})

  # Affichage des premières lignes
  output$head_data_to_geocode <- renderTable({
   # req(input$file1)
    head(data_to_geocode())})

  # PAGE 2 : GÉOCODAGE
  # Sélectionne les variables à choisir pour l'adresse
  observe({ updateSelectInput(session, 'colonne_num', choices = c("-",names(data_to_geocode()))) })
  observe({ updateSelectInput(session, "colonne_rue", choices = c("-", names(data_to_geocode()))) })
  observe({ updateSelectInput(session, 'colonne_code_postal', choices = c("-",names(data_to_geocode()))) })
  observe({ updateSelectInput(session, 'colonne_num_rue', choices = c("-",names(data_to_geocode()))) })
  observe({ updateSelectInput(session, 'colonne_num_rue_code_postal', choices = c("-",names(data_to_geocode()))) })
  observe({ updateSelectInput(session, 'colonne_rue_code_postal', choices = c("-",names(data_to_geocode()))) })

  # Lancer le géocodage

  result <- eventReactive(input$geocode, {
    # NULL a la place des -
    if(input$colonne_rue=="-"){colonne_rue<-NULL}else{colonne_rue<-input$colonne_rue}
    if(input$colonne_num=="-"){colonne_num<-NULL}else{colonne_num<-input$colonne_num}
    if(input$colonne_code_postal=="-"){colonne_code_postal<-NULL}else{colonne_code_postal<-input$colonne_code_postal}
    if(input$colonne_num_rue=="-"){colonne_num_rue<-NULL}else{colonne_num_rue<-input$colonne_num_rue}
    if(input$colonne_num_rue_code_postal=="-"){colonne_num_rue_code_postal<-NULL}else{colonne_num_rue_code_postal<-input$colonne_num_rue_code_postal}
    if(input$colonne_rue_code_postal=="-"){colonne_rue_code_postal<-NULL}else{colonne_rue_code_postal<-input$colonne_rue_code_postal}

    # pour l'affichage des messages en html
    #withCallingHandlers({
    #  shinyjs::html("text", "")

  #  data_path <- "/home/user/github/phacochr_data/data_phacochr/"
     phaco_geocode(data_to_geocode=data_to_geocode(),
                                colonne_rue=colonne_rue,
                                colonne_num = colonne_num,
                                colonne_code_postal = colonne_code_postal,
                                colonne_num_rue = colonne_num_rue,
                                colonne_num_rue_code_postal = colonne_num_rue_code_postal,
                                colonne_rue_code_postal = colonne_rue_code_postal,
                                method_stringdist = input$method_stringdist,
                                corrections_REGEX = input$corrections_REGEX,
                                error_max = input$error_max,
                                approx_num_max = input$approx_num,
                                elargissement_com_adj = input$elargissement_com_adj,
                                lang_encoded = input$lang_encoded,
                                mid_street = input$mid_street,
                               path_data= "data_phacochr/")
  #}
  #,
  # Affichage des messages à la ligne
  #cat = function(m) {shinyjs::html(id = "text", html = paste0(m$cat, '<br>'), add = TRUE)})

    })

    # Affichage des tableau récap
    output$summary <-  renderTable({
      req(input$geocode)
      result()$summary[2:nrow(result()$summary),c(1:3,6:8,5)]
    })


  # Affichage des données géocodées
  output$head_data_geocoded <-  renderTable({
    req(input$geocode)
    head(result()$data_geocoded)
  })
 # Télécharger les données
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$file1$name, "_phacochr", input$fileType_output)
    },
    content = function(file) {
      if(input$fileType_output == ".csv") {
        write.csv(result()$data_geocoded, file, row.names = FALSE)
      } else if(input$fileType_output %in%c(".gpkg",".kml",".geojson")) {
        st_write(result()$data_geocoded_sf, file)
      }
    })

  # Carte interactive

  output$tmapMap <- renderTmap({

    req(result()$data_geocoded_sf)

    FULL_GEOCODING_sf<-  result()$data_geocoded_sf

    source("script/Carto des points géocodés - tmap interactif_6_14.R", local=TRUE)
    Carto_map

  })



}

# Create Shiny app ----
shinyApp(ui, server )
