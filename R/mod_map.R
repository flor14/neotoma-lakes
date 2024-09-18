leafletmapUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(
      leaflet::leafletOutput(ns('map'),
                             height = 450),
      type = 2,
      color = '#1b3964',
      color.background = 'white'
    )
  )
}

leafletmapServer <- function(id,
                             r_neosites_data,
                             r_lake_data,
                             modify,
                             nooptions,
                             removelakes) {
  moduleServer(id,
               function(input, output, session) {
                 # Map
                 output$map <- leaflet::renderLeaflet({

                   # Creation of a basic map
                   lm  <-   leaflet::leaflet() |>
                     leaflet::addTiles(group = "OpenStreetMap") |>
                     leaflet::addProviderTiles(
                       "Esri.WorldImagery",
                       group = "Esri.WorldImagery",
                       options = leaflet::providerTileOptions(
                         attribution = paste(
                           'Tiles',
                           '&copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS,',
                           'AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP,',
                           'and the GIS User Community - Powered by Esri'
                         )
                       )
                     ) |>
                     leaflet::addLayersControl(
                       baseGroups = c("EsriWorldImagery",
                                      "OpenStreetMap"),
                       options = leaflet::layersControlOptions(collapsed = FALSE)
                     ) |>
                     leaflet::addMeasure(primaryLengthUnit = "kilometers",
                                         secondaryLengthUnit = "kilometers")


                   r_neosites_data  <- r_neosites_data() |>
                     sf::st_transform(4326)
                   lake_data <- r_lake_data() |>
                     sf::st_transform(4326)

                   # To zoom in the map
                   bbx = sf::st_bbox(sf::st_union(r_neosites_data,
                                                  lake_data))

                   # The NeotomaDB site could be a point or a polygon
                   if (sf::st_is(r_neosites_data, "POINT")) {
                     lm  <-   lm |>
                       leaflet::addPolygons(
                         data = lake_data,
                         group = 'lakes',
                         layerId = ~ Hylak_id,
                         # https://rstudio.github.io/leaflet/showhide.html
                         weight = 5,
                         color = "blue",
                         fillColor = "lightblue"
                       ) |> # hydrolakes
                       leaflet::addMarkers(data = r_neosites_data) |> # neotoma sites
                       leaflet::fitBounds(
                         lng1 = bbx$xmin[[1]],
                         lat1 = bbx$ymin[[1]],
                         lng2 = bbx$xmax[[1]],
                         lat2 = bbx$ymax[[1]]
                       ) |>
                       leafem::addMouseCoordinates()

                   } else{
                     lm <-  lm |>
                       leaflet::addPolygons(
                         data = r_lake_data(),
                         group = 'lakes',
                         layerId = ~ Hylak_id,
                         # https://rstudio.github.io/leaflet/showhide.html
                         weight = 5,
                         color = "blue",
                         fillColor = "lightblue"
                       ) |> # hydrolakes
                       leaflet::addPolygons(
                         data = r_neosites_data,
                         weight = 5,
                         color = "#DB576B",
                         fillColor = "pink"
                       ) |> # neotoma sites
                       leaflet::fitBounds(
                         lng1 = bbx$xmin[[1]],
                         lat1 = bbx$ymin[[1]],
                         lng2 = bbx$xmax[[1]],
                         lat2 = bbx$ymax[[1]]
                       ) |>
                       leafem::addMouseCoordinates()
                   }

                   # Add toolbar to draw polygons if this is requested by the user
                   if (!is.null(nooptions()) &&
                       nooptions() == "Create lake polygon") {
                     lm |>
                       leaflet.extras::addDrawToolbar(
                         markerOptions = FALSE,
                         circleMarkerOptions = FALSE,
                         polylineOptions = FALSE,
                         circleOptions = FALSE,
                         rectangleOptions = FALSE,
                         singleFeature = TRUE
                       )
                   } else{
                     lm
                   }
                 })

                 # The user can request to hide the lakes layer
                 shiny::observe({
                   shiny::req(nooptions())

                   proxy <- leaflet::leafletProxy('map')

                   if (modify() == 'Update Current Location' &&
                       nooptions() == "Create lake polygon") {
                     proxy |>  leaflet::hideGroup('lakes')
                   } else{
                     proxy |>  leaflet::showGroup('lakes')
                   }
                 })

                 # The user can request to hide the lakes layer
                 shiny::observeEvent(removelakes(), {
                   proxy <- leaflet::leafletProxy('map')

                   if (removelakes() == TRUE) {
                     proxy |>  leaflet::hideGroup('lakes')
                   } else{
                     proxy |>  leaflet::showGroup('lakes')
                   }
                 })

                 return(list(
                   map_shape_click = reactive({
                     input$map_shape_click
                   }),
                   map_draw_new_feature = reactive({
                     input$map_draw_new_feature
                   })
                 ))
               })
}