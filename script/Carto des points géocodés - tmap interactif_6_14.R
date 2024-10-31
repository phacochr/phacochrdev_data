
# * Type de carte : "static" ou "interactif"
# Les deux types sont programmés dans le script ci-dessous, mais en static tmap produits des résultats différents selon que l'on est sous windows ou linux avec les mêmes paramètres => pas ok
# tmap est donc utilisé uniquement pour produire des cartes interactives
library(sf)
library(stringr)


mode_carto <- "interactif"
name_to_show <- "nom"
title_carto = ""

zoom_geocoded <- TRUE
nom_admin <- TRUE

# 1. BRUXELLES ============================================================================================================================

# S'il y a des points uniquement à Bruxelles
if ((str_detect(paste(unique(FULL_GEOCODING_sf$cd_rgn_refnis[!is.na(FULL_GEOCODING_sf$cd_rgn_refnis)]), collapse = " "), "04000")) & (length(unique(FULL_GEOCODING_sf$cd_rgn_refnis[!is.na(FULL_GEOCODING_sf$cd_rgn_refnis)])) == 1)){


# a) Geopackages --------------------------------------------------------------------------------------------------------------------------

  # Je ne charge les geopackages qu'une fois si plusieurs cartes sont faites
  if (!exists("BXL_communes")){
    BXL_communes <- st_read("data_phacochr/STATBEL/PREPROCESSED/BXL_communes_PREPROCESSED.gpkg")
  }
  if (!exists("BXL_SS")){
    BXL_SS <- st_read("data_phacochr/STATBEL/PREPROCESSED/BXL_SS_PREPROCESSED.gpkg")
  }
  if (!exists("BRUXELLES")){
    BRUXELLES <- st_read("data_phacochr/STATBEL/PREPROCESSED/BRUXELLES_PREPROCESSED.gpkg")
  }


# b) Carto --------------------------------------------------------------------------------------------------------------------------------

  if(zoom_geocoded == FALSE){ # je crée une bbox pour délimiter les limites de la carte
    bbox_bxl <- st_bbox(BXL_communes)
  }
  if(zoom_geocoded == TRUE){ # Si je veux un zoom sur les points
    bbox_bxl <- st_bbox(FULL_GEOCODING_sf)
  }

  if(mode_carto == "static"){
    tmap_mode("plot") # Le mode dans le cas d'une carte statique
    Carto_map <- tm_shape(BXL_SS, bbox = bbox_bxl) +
      tm_fill("white") +
      tm_borders("gray85") +
      tm_shape(BXL_communes) +
      tm_borders("gray35", lwd = 1.5) +
      tm_shape(BRUXELLES) +
      tm_borders("black", lwd = 2) +
      tm_shape(FULL_GEOCODING_sf, bbox = bbox_bxl) +
      tm_dots("#d61d5e",
              size = 0.1,
              shape = 16,
              border.col = "#d61d5e",
              alpha = 0.7,
              id = name_to_show) +
      tm_compass(position = c("left", "top")) +
      tm_scale_bar() +
      tm_credits(
        "Observatoire de la Santé et du Social\nJoël Girès, 2022",
        size = 0.7,
        position = "left") +
      tm_layout(main.title = paste("Cartographie :", title_carto),
                main.title.size = 1.25,
                frame = "white",
                bg.color = "gray95")
  }
  if(mode_carto == "interactif"){
    tmap_mode("view") # Le mode dans le cas d'une carte interactive
    taille_point <- ifelse(nrow(FULL_GEOCODING_sf) < 20, 0.1,
                           ifelse(nrow(FULL_GEOCODING_sf) > 100, 0.01,
                                  0.05))
    Carto_map <- tm_shape(BXL_communes, bbox = bbox_bxl) +
      tm_borders("black", lwd = 1, lty = "dashed") +
      tm_shape(BRUXELLES) +
      tm_borders("black", lwd = 1.5, lty = "dashed") +
      tm_shape(FULL_GEOCODING_sf, bbox = bbox_bxl) +
      tm_dots("#d61d5e",
              size = taille_point,
              shape = 16,
              border.col = "#d61d5e",
              alpha = 1,
              id = name_to_show) +
      tm_layout(title = paste("Cartographie :", title_carto)) +
      tm_basemap(leaflet::providers$CartoDB.Positron) +
      tm_basemap(leaflet::providers$CartoDB.PositronNoLabels) +
      tm_basemap(leaflet::providers$OpenStreetMap) +
      tm_view(bbox = bbox_bxl)
  }
  if(nom_admin == TRUE & mode_carto == "static"){
    Carto_map <- Carto_map +
      tm_shape(st_point_on_surface(BXL_communes), bbox = bbox_bxl) +
      tm_text("tx_munty_descr_fr",
              shadow = TRUE,
              col = "#18707b",
              size = 0.75)
  }

  print(Carto_map)


    2 * 0.001


# 2. FLANDRE/WALLONIE =====================================================================================================================

# S'il y a des points en Flandre ou en Wallonie
} else {

# a) Geopackages --------------------------------------------------------------------------------------------------------------------------

  # Je ne charge les geopackages qu'une fois si plusieurs cartes sont faites
  if (!exists("BE_communes")){
    BE_communes <- st_read("data_phacochr/STATBEL/PREPROCESSED/BE_communes_PREPROCESSED.gpkg")
  }
  if (!exists("BE_provinces")){
    BE_provinces <- st_read("data_phacochr/STATBEL/PREPROCESSED/BE_provinces_PREPROCESSED.gpkg")
  }
  if (!exists("BE_regions")){
    BE_regions <- st_read("data_phacochr/STATBEL/PREPROCESSED/BE_regions_PREPROCESSED.gpkg")
  }
  if (!exists("BELGIQUE")){
    BELGIQUE <- st_read("data_phacochr/STATBEL/PREPROCESSED/BELGIQUE_PREPROCESSED.gpkg")
  }


  # b) Carto --------------------------------------------------------------------------------------------------------------------------------

  if(zoom_geocoded == FALSE){ # je crée une bbox pour délimiter les limites de la carte
    bbox_belgique <- st_bbox(BE_regions)
  }
  if(zoom_geocoded == TRUE){ # Si je veux un zoom sur les points
    bbox_belgique <- st_bbox(FULL_GEOCODING_sf)
  }

  if(mode_carto == "static"){
    tmap_mode("plot") # Le mode dans le cas d'une carte statique
    Carto_map <- tm_shape(BE_communes, bbox = bbox_belgique) +
      tm_fill("white") +
      tm_borders("gray90") +
      tm_shape(BE_provinces) +
      tm_borders("gray30", lwd = 1.5) +
      tm_shape(BE_regions) +
      tm_borders("black", lwd = 1.5) +
      tm_shape(BELGIQUE) +
      tm_borders("black", lwd = 2) +
      tm_shape(FULL_GEOCODING_sf, bbox = bbox_belgique) +
      tm_dots("#d61d5e",
              size = 0.025,
              shape = 16,
              border.col = "#d61d5e",
              alpha = 0.5,
              id = name_to_show) +
      tm_compass(position = c("left", "top")) +
      tm_scale_bar() +
      tm_credits(
        "Observatoire de la Santé et du Social\nJoël Girès, 2022",
        size = 0.7,
        position = "left") +
      tm_layout(main.title = paste("Cartographie :", title_carto),
                main.title.size = 1.25,
                frame = "white",
                bg.color = "gray95",
                inner.margins = c(0.08, 0, 0, 0))
  }
  if(mode_carto == "interactif"){
    tmap_mode("view") # Le mode dans le cas d'une carte interactive
    Carto_map <- tm_shape(BE_provinces, bbox = bbox_belgique) +
      tm_borders("black", lwd = 1, lty = "dashed") +
      #tm_shape(BE_regions) +
      #tm_borders("black", lwd = 1, lty = "dashed") +
      tm_shape(BELGIQUE) +
      tm_borders("black", lwd = 1, lty = "dashed") +
      tm_shape(FULL_GEOCODING_sf, bbox = bbox_belgique) +
      tm_dots("#d61d5e",
              size = 0.005,
              shape = 16,
              border.col = "#d61d5e",
              alpha = 1,
              id = name_to_show) +
      tm_basemap(leaflet::providers$CartoDB.Positron) +
      tm_basemap(leaflet::providers$CartoDB.PositronNoLabels) +
      tm_basemap(leaflet::providers$OpenStreetMap) +
      tm_view(bbox = bbox_belgique) +
      tm_layout(title = paste("Cartographie :", title_carto))
  }
  if(nom_admin == TRUE & mode_carto == "static"){
    Carto_map <- Carto_map +
      tm_shape(st_point_on_surface(BE_provinces), bbox = bbox_belgique) +
      tm_text("tx_prov_descr_fr",
              shadow = TRUE,
              col = "#18707b",
              size = ifelse(zoom_geocoded == TRUE, 1, 0.85))
  }

  print(Carto_map)

}
