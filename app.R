library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(stringr)
library(dplyr)
library(shinycssloaders)
library(leafem)


sites <- read.csv('../Dropbox/neotoma_project/sites_20231101_.csv')
#lakes <- st_read('https://www.dropbox.com/scl/fi/6g8m1p6wcnfytkyfriuvl/HydroLAKES_polys_v10.shp?rlkey=szmv5thra18ba5hkrd5284vt3&dl=1')

# Some sites are points and other ones are polygons
pointsites <- sites[stringr::str_detect(sites$geog, pattern = "POINT"), ] |>
  sf::st_as_sf(wkt = 'geog') |> select(siteid, sitename, geog) |>
  mutate(lonc = sf::st_coordinates(geog)[,1], # need this to setView in leaflet
         latc = sf::st_coordinates(geog)[,2])

polysites <- sites[stringr::str_detect(sites$geog, pattern = "POLYGON"), ] |>
  sf::st_as_sf(wkt = 'geog') |> select(siteid, sitename, geog) |>
  st_simplify(preserveTopology = TRUE, dTolerance = 100) |>
  mutate(centroid = st_point_on_surface(geog),  # need this to setView in leaflet
         lonc = sf::st_coordinates(centroid)[,1],
         latc = sf::st_coordinates(centroid)[,2]) |>
  select(-centroid)

datasf <- rbind(pointsites, polysites)


# Links
link_neotoma <- tags$a(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank")
link_slack <- tags$a(shiny::icon("r-project"), "Posit", href = "https://posit.co", target = "_blank")


ui <-  bslib::page_navbar(
  title = "Neotoma Lakes",
  theme = bslib::bs_theme(bootswatch = "minty",
                   base_font = font_google("Inter")),
  bslib::nav_panel(title = "correct",
            bslib::layout_columns(
    col_widths = c(8, 4, 8, 4),
    row_heights = c(7, 4),
        bslib::layout_sidebar(class = "p-0",
                             sidebar = sidebar(width = 200,
textInput('neositeid', 'SiteId', placeholder = "write here"),

 # selectizeInput(inputId = 'neositeid',
 #             label = 'SiteId',
 #             choices = NULL
 #             # choices = sort(unique(data$siteid)),
 #             # selected = data$siteid[3]
 #             ),
 shiny::actionButton('search', 'Search!'),
 hr(),
tags$h6('NeotomaDB data'),
 textOutput('sitename')
 ),

 shinycssloaders::withSpinner(
 leaflet::leafletOutput('map', height = 645),
 type = 4,
 color = 'darkgreen',
 color.background = 'white')),
    card(
      card_header("Lake"),
      'hello'
    ),
    card(

    ),
    card(
      card_header("Country"),
      'hello'
    ))
  ),
nav_spacer(),
nav_menu(
  title = "Links",
  align = "right",
  nav_item(link_neotoma),
  nav_item(link_slack)
)
)

server <- function(input, output, session) {

  # observe({
  #
  #   shiny::updateSelectizeInput(session,
  #                     inputId = 'neositeid',
  #                     label = 'SiteId',
  #                     choices = sort(unique(datasf$siteid)))
  #
  # })



  map_data <- eventReactive(input$search,ignoreInit = FALSE,{




    datasf |>
       dplyr::mutate(siteid = as.character(siteid)) |>
      dplyr::filter(siteid %in% as.character(input$neositeid))


  })

  output$map <- leaflet::renderLeaflet({


   lm =  leaflet::leaflet() |>
     leaflet::addTiles(group = "OpenStreetMap") |>
      leaflet::addProviderTiles("Esri.WorldImagery",
                                group = "Esri.WorldImagery",
                                options = providerTileOptions(attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community - Powered by Esri')) |>
     leaflet::addLayersControl(
       baseGroups = c("EsriWorldImagery", "OpenStreetMap"),
       options = layersControlOptions(collapsed = FALSE)
     )



   if(sf::st_is(map_data(), "POINT")){

    lm |>
       leaflet::addCircles(data = map_data()) |>
       setView(lng = map_data()$lonc, lat= map_data()$latc, zoom = 9) |>
       leafem::addMouseCoordinates()

   }else{

    lm |>
       leaflet::addPolygons(data = map_data())  |>
       setView(lng = map_data()$lonc, lat= map_data()$latc, zoom = 9) |>
       leafem::addMouseCoordinates()

   }


  })


  output$sitename <- renderText({
  #  req(input$search)
   paste("sitename:", as.character(map_data()$sitename))
    })




}

shinyApp(ui, server)