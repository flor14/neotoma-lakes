library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(stringr)
library(dplyr)
library(shinycssloaders)
library(leafem)
library(tidyr)
library(ggplot2)
# https://github.com/r-spatial/sf/issues/1762
sf_use_s2(FALSE)

# sites <- read.csv('../Dropbox/neotoma_project/sites_20231101_.csv')
#
# lakes <- st_read('../Dropbox/neotoma_project/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10_shp') |>
#   select(Lake_name, Country, Continent, Vol_total, Shore_len, Depth_avg, Elevation, geometry)
#
# # I select some countries to try the app. Later I will use all.
# can <- st_read('data/gadm41_CAN.gpkg') |>
#   st_simplify(preserveTopology = TRUE, dTolerance = 100)
#
# arg <- st_read('data/gadm41_ARG.gpkg') |>
#   st_simplify(preserveTopology = TRUE, dTolerance = 100)
#
# # jpn <- st_read('data/gadm41_JPN.gpkg') |>
# #   st_simplify(preserveTopology = TRUE, dTolerance = 100)
#
# countries =  countries_sf = rbind(arg, can)
#
# st_geometry(countries) <- NULL
#
#
# # Some sites are points and other ones are polygons
# pointsites <- sites[stringr::str_detect(sites$geog, pattern = "POINT"), ] |>
#   sf::st_as_sf(wkt = 'geog') |>
#   select(siteid, sitename, geog) |>
#   mutate(lonc = sf::st_coordinates(geog)[,1], # need this to setView in leaflet
#          latc = sf::st_coordinates(geog)[,2])
#
# polysites <- sites[stringr::str_detect(sites$geog, pattern = "POLYGON"), ] |>
#   sf::st_as_sf(wkt = 'geog') |>
#   select(siteid, sitename, geog) |>
#   st_simplify(preserveTopology = TRUE,
#               dTolerance = 100) |>
#   mutate(centroid = st_point_on_surface(geog),  # need this to setView in leaflet
#          lonc = sf::st_coordinates(centroid)[,1],
#          latc = sf::st_coordinates(centroid)[,2]) |>
#   select(-centroid)
#
# datasf <- rbind(pointsites, polysites) |> st_set_crs(4326)
#
#
# lakes_country <- countries |> left_join(lakes, by = c('COUNTRY' = 'Country'))
# write_sf(lakes_country, "data/lakes_country.gpkg")
lakes_country <- st_read("data/lakes_country.gpkg")

# sites_country <- datasf |> st_join(countries_sf, left = TRUE) |> tidyr::drop_na(COUNTRY)
#write_sf(sites_country, "data/sites_country.gpkg")
sites_country <- st_read("data/sites_country.gpkg")

# Links
link_neotoma <- tags$a(shiny::icon("github"), "GitHub", href = "https://github.com/NeotomaDB", target = "_blank")
link_slack <- tags$a(shiny::icon("slack"), "Slack", href = "https://neotomadb.slack.com/ssb/redirect", target = "_blank")


ui <-  bslib::page_navbar(
  title = "NeotomaDB",
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
      plotOutput('hydrolake')
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



  neosites_data <- eventReactive(input$search,{


    sites_country |>
       dplyr::mutate(siteid = as.character(siteid)) |>
      dplyr::filter(siteid %in% as.character(input$neositeid))

  })


  lake_data <- eventReactive(input$search,{


    lakes_country[st_is_within_distance(neosites_data(), lakes_country, dist = 9000)[[1]],]

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



   if(sf::st_is(neosites_data(), "POINT")){

    lm |>
       leaflet::addCircles(data = neosites_data()) |> # neotoma sites
       leaflet::addPolygons(data = lake_data()) |> # lakes
       setView(lng = neosites_data()$lonc, lat= neosites_data()$latc, zoom = 9) |>
       leafem::addMouseCoordinates()

   }else{

    lm |>
       leaflet::addPolygons(data = neosites_data())  |> # neotoma sites
       leaflet::addPolygons(data = lake_data())  |> # lakes
       setView(lng = neosites_data()$lonc, lat= neosites_data()$latc, zoom = 9) |>
       leafem::addMouseCoordinates()

   }


  })


  output$sitename <- renderText({
  #  req(input$search)
   paste("sitename:", as.character(neosites_data()$sitename))
    })


  output$hydrolake <- renderPlot({
req(input$search)
    ggplot()+
      geom_sf(data = lake_data(),
              fill = 'transparent',
              lwd = 2)+
      theme_void()


  })


}

shinyApp(ui, server)