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
#   select(Hylak_id, Lake_name, Country, Vol_total, Shore_len, Depth_avg, Elevation, geometry)
#
#
# # I select some countries to try the app. Later I will use all.
#
#
# mdg <- st_read('data/gadm41_MDG.gpkg') |>
#    st_simplify(preserveTopology = TRUE)
# prt <- st_read('data/gadm41_PRT.gpkg') |>
#    st_simplify(preserveTopology = TRUE)
# # arg <- st_read('data/gadm41_ARG.gpkg') |>
# #   st_simplify(preserveTopology = TRUE)
# jpn <- st_read('data/gadm41_JPN.gpkg') |>
#   st_simplify(preserveTopology = TRUE)
#
# countries =  countries_sf = rbind(jpn, prt, mdg)
#
# write_sf(countries_sf, 'data/countries.gpkg')

countries_sf <- st_read('data/countries.gpkg') |> st_transform(3857) |>  st_simplify(preserveTopology = TRUE, dTolerance=100)

# st_geometry(countries) <- NULL
#
# #Some sites are points and other ones are polygons
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
# #extra step= removing sites of countries that aren't in the demo
# datasf_country = st_filter(datasf, countries_sf)

# lakes_country <- countries |> left_join(lakes, by = c('COUNTRY' = 'Country'))

#|>
#    st_transform(3857) |>
#    st_simplify(preserveTopology = TRUE,
#               dTolerance = 100) |>
#    st_transform(4326)
# write_sf(lakes_country, "data/lakes_country.gpkg")
 lakes_country <- st_read("data/lakes_country.gpkg") |> st_transform(3857)
#
#  sites_country <- datasf |> st_join(countries_sf, left = TRUE) |> tidyr::drop_na(COUNTRY)
# write_sf(datasf_country, "data/sites_country.gpkg")
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
      card_header("HydroLAKES"),
      plotOutput('hydrolake'),
      textOutput('lakeinfo')
    ),
    card(

    ),
    card(
      card_header("Country"),
      plotOutput('country'),
      textOutput('countryinfo')
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
      dplyr::filter(siteid %in% as.character(input$neositeid)) |> st_transform(3857)

  })


  lake_data <- eventReactive(input$search,{

    lakes_country[st_is_within_distance(neosites_data(),
                                        lakes_country,
                                        dist = 50000)[[1]],] |>
      st_set_crs(3857)

  })

  output$map <- leaflet::renderLeaflet({

    print(lake_data())

   lm =  leaflet::leaflet() |>
     leaflet::addTiles(group = "OpenStreetMap") |>
      leaflet::addProviderTiles("Esri.WorldImagery",
                                group = "Esri.WorldImagery",
                                options = providerTileOptions(attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community - Powered by Esri')) |>
     leaflet::addLayersControl(
       baseGroups = c("EsriWorldImagery", "OpenStreetMap"),
       options = layersControlOptions(collapsed = FALSE)
     )

# bbox for zoom


   neosites_data = neosites_data() |> st_transform(4326)

   lake_data = lake_data() |> st_transform(4326)

   bbx = st_bbox(st_union(neosites_data, lake_data))

  # bbx = bbx |> st_transform(4326)


   if(sf::st_is(neosites_data(), "POINT")){

    lm |>
       leaflet::addPolygons(data = lake_data,
                            layerId = ~lake_data$Hylak_id, # https://rstudio.github.io/leaflet/showhide.html
                            weight = 5,
                            color = "blue",
                            fillColor = "lightblue") |> # lakes
       leaflet::addCircles(data = neosites_data,
                           weight = 5,
                           color = "darkorange",
                           fillColor = "orange") |> # neotoma sites
       fitBounds(lng1 = bbx$xmin[[1]],
                 lat1= bbx$ymin[[1]],
                 lng2 = bbx$xmax[[1]],
                 lat2 = bbx$ymax[[1]]) |>
       leafem::addMouseCoordinates()

   }else{

    lm |>
       leaflet::addPolygons(data = lake_data,
                            layerId = ~lake_data$Hylak_id, # https://rstudio.github.io/leaflet/showhide.html
                            weight = 5,
                            color = "blue",
                            fillColor = "lightblue")  |> # lakes
       leaflet::addPolygons(data = neosites_data,
                            weight = 5,
                            color = "darkorange",
                            fillColor = "orange")  |> # neotoma sites
       fitBounds(lng1 = bbx$xmin[[1]],
               lat1= bbx$ymin[[1]],
               lng2 = bbx$xmax[[1]],
               lat2 = bbx$ymax[[1]]) |>
       leafem::addMouseCoordinates()

   }


  })


  output$sitename <- renderText({
  #  req(input$search)
   paste("sitename:", as.character(neosites_data()$sitename))
    })


  lk_hover <- reactive({

    req(input$map_shape_click)

    lake_data = lake_data() |> st_transform(4326)

    if(!is.null(input$map_shape_click$id)){

    # convert hover coordinates in a sfc point
    p = st_sfc(st_point(x=c(input$map_shape_click$lng,
                            input$map_shape_click$lat),
                        dim="XY"),
               crs = 4326)

    # detect detect polygon hovered by the user
    lk_hover = lake_data[st_intersects(lake_data, p, sparse = FALSE),]
    lk_hover

    }
  })

  output$hydrolake <- renderPlot({

    lake_data = lake_data() |> st_transform(4326)
    countries_sf |> st_transform(4326)

    req(input$search)

    if(!is.null(input$map_shape_click$id)){



      # plot polygon of lake of interest
      ggplot()+
        geom_sf(data = lk_hover(),
                fill = 'transparent',
                lwd = 0.5)+
        theme_void()



    }else{


      ggplot()+
        geom_sf(data = lake_data[1,], # only the closest lake
                fill = 'transparent',
                lwd = 0.5)+
        theme_void()


    }


  })

  output$lakeinfo <- renderText({


    lake_data = lake_data() |> st_transform(4326)

    print(lake_data)

    if(!is.null(input$map_shape_click$id)){

      paste("Hylak ID:", as.character(lk_hover()$Hylak_id))


      }else{

        paste("Hylak ID:",as.character(lake_data[1,]$Hylak_id))
    }

  })



  output$country <- renderPlot({

print(input$map_shape_click)
    lake_data = lake_data() |> st_transform(4326)
    countries_sf |> st_transform(4326)

    if(!is.null(input$map_shape_click$id)){

      csf =  countries_sf |> dplyr::filter(COUNTRY == lk_hover()$COUNTRY)
      print(csf)
      # plot polygon of lake of interest
      ggplot()+
        geom_sf(data = csf,
                fill = 'transparent',
                lwd = 0.5)+
        theme_void()



    }else{

      csf =  countries_sf |> dplyr::filter(COUNTRY == lake_data[1,]$COUNTRY)
print(csf)
      ggplot()+
        geom_sf(data = csf,
                fill = 'transparent',
                lwd = 0.5)+
        theme_void()


    }



  })

  output$countryinfo <- renderText({
print(lk_hover())

    lake_data = lake_data() |> st_transform(4326)



    if(!is.null(input$map_shape_click$id)){
      csf =  countries_sf |> dplyr::filter(COUNTRY == lk_hover()$COUNTRY)
      print(csf)
      paste(as.character(csf$COUNTRY))


    }else{

      csf =  countries_sf |> dplyr::filter(COUNTRY == lake_data[1,]$COUNTRY)
      print(csf)
      paste(as.character(csf$COUNTRY))
    }

  })

}

shinyApp(ui, server)