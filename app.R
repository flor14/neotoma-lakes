library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(stringr)
library(dplyr)
library(shinycssloaders)
library(DT)
library(leafem)
library(tidyr)
library(ggplot2)
library(shinyWidgets)
library(leaflet.extras)

# https://github.com/r-spatial/sf/issues/1762
sf_use_s2(FALSE)

# sites <- read.csv('../Dropbox/neotoma_project/sites_20231101_.csv')
#
# lakes <- st_read('../Dropbox/neotoma_project/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10_shp') |>
#   select(Hylak_id, Lake_name, Country, Vol_total, Shore_len, Depth_avg, Elevation, geometry)


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

# #Some sites are points and other ones are polygons
# pointsites <- sites[stringr::str_detect(sites$geog, pattern = "POINT"), ] |>
#   sf::st_as_sf(wkt = 'geog') |>
#   select(siteid, sitename, sitedescription, notes, geog) |>
#   mutate(lonc = sf::st_coordinates(geog)[,1], # need this to setView in leaflet
#          latc = sf::st_coordinates(geog)[,2])
#
# polysites <- sites[stringr::str_detect(sites$geog, pattern = "POLYGON"), ] |>
#   sf::st_as_sf(wkt = 'geog') |>
#   select(siteid, sitename, sitedescription, notes, geog) |>
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
  theme = bslib::bs_theme(version = 5,
                          bootswatch = "minty",
                   base_font = font_google("Raleway", wght = "400"),
                   heading_font = font_google("Raleway", wght = "200")),
                 #  primary = 'orange'),
  bslib::nav_panel(title = "Lakes",
            bslib::layout_columns(
    col_widths = c(9, 3),
    row_heights = c(1,1),


        bslib::layout_sidebar(class = "p-0",
                             sidebar = sidebar(width = 300,
                                               bg = 'white',
 textInput('neositeid',
           'SiteId',
           placeholder = "write here"),
 tooltip(
 shiny::actionButton('search', 'Search!'),
 "Check sitesid in the webpage. For this demo you can use site 9606 as an example"),
 accordion(
   open = 'User options',
   multiple = TRUE,
   height="100%",
   id = 'sidebar_accordion',
   accordion_panel(
     'Site metadata',
     DT::DTOutput('metadata')),
   accordion_panel(
     'Map options',
   shinyWidgets::materialSwitch(
     inputId = "removelakes",
     label = "remove HYDROlakes layer",
     status = "primary"
   )),
 accordion_panel(
   'User options',
 prettyRadioButtons( # or prettyRadioButtons
   inputId = "modify",
   label = "Is this site the correct lake polygon in NeotomaDB?",
   choices = c("Yes", "No"),
   outline = TRUE,
   plain = TRUE,
   status = "primary",
   icon = icon("check")
 ),
 uiOutput('dynamicUI')
 ))),
accordion(id = 'map_accordion',
  height="100%",
  open = c("Map"),
  accordion_panel(
    "Map",
    class = 'p-0',
 shinycssloaders::withSpinner(
 leaflet::leafletOutput('map', height = 600),
 type = 4,
 color = 'darkgreen',
 color.background = 'white')),
 accordion_panel(
   "Comments",
textAreaInput('notes',
              label = 'Comments:'),
downloadButton('submit', 'Submit')))),
    card(
      card_header("HydroLAKES"),
       bslib::layout_columns(
        col_widths = c(6, 6),
      plotOutput('hydrolake'),
      DT::DTOutput('lakeinfo')),
      textOutput('lakename'),
      textOutput('dist', inline = TRUE)
     # plotOutput('country'),
     # textOutput('countryinfo')

    )

  )),
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
      dplyr::filter(siteid %in% as.character(input$neositeid)) |>
      st_transform(3857)

  })


  lake_data <- eventReactive(input$search,{

    lakes_country[st_is_within_distance(neosites_data(),
                                        lakes_country,
                                        dist = 50000)[[1]],] |>
      st_set_crs(3857)

  })

  output$map <- leaflet::renderLeaflet({

    validate(
      need(!is.null(input$neositeid), "Please, provide a NeotomaDB siteId")
    )


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





   # the plot will change if it is a point or a polygon
   if(sf::st_is(neosites_data(), "POINT")){

   lm =  lm |>
       leaflet::addPolygons(data = lake_data,
                            group = 'lakes',
                            layerId = ~Hylak_id, # https://rstudio.github.io/leaflet/showhide.html
                            weight = 5,
                            color = "blue",
                            fillColor = "lightblue") |>
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

   lm = lm |>
       leaflet::addPolygons(data = lake_data,
                            group = 'lakes',
                            layerId = ~Hylak_id, # https://rstudio.github.io/leaflet/showhide.html
                            weight = 5,
                            color = "blue",
                            fillColor = "lightblue") |>
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


# Add toolbar to draw polygons if this is requested by the user
   if(!is.null(input$nooptions) && input$nooptions == "Create lake polygon"){

    lm |>
      leaflet.extras::addDrawToolbar(markerOptions = FALSE)


   }else{ lm }



  })

# hide the lakes layer
  observeEvent(input$removelakes, {

    proxy <- leafletProxy('map')

   if(input$removelakes == TRUE){
       proxy |>  hideGroup('lakes')
   }else{
       proxy |>  showGroup('lakes')}
    }
  )

  observeEvent(input$map_draw_new_feature,{
    feature <- input$map_draw_new_feature

    print(feature$geometry$coordinates)

  })

observeEvent(input$neositeid,{
  accordion_panel_open(id = 'sidebar_accordion',
                       value = 'Site metadata')
})


observeEvent(input$modify,{
  accordion_panel_open(id = 'map_accordion',
                       value = 'Comments')
})


  output$metadata <- DT::renderDT({

   name =  as.character(neosites_data()$sitename)
   description = as.character(neosites_data()$sitedescription)
   notes = as.character(neosites_data()$notes)

   data <- data.frame(c(description,
                        'notes', notes))

   colnames(data) <- as.character(name)

   DT::datatable(data,
                 rownames= FALSE,
                 options = list(dom = 't',
                 ordering = FALSE))



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



    }

  })

  output$dist <- renderText({



    validate(
      need(!is.null(input$map_shape_click), "Please, click one of the HYDROlakes in the map")
    )



  })

  output$dynamicUI <- renderUI({
    if (input$modify == "No") {
      prettyRadioButtons( # or prettyRadioButtons
        inputId = "nooptions",
        label = "How would you update NeotomaDB?",
        choices = c("Replace it with a HYDROLakeDB (preferred)" ,
                    "Create lake polygon"),
        outline = TRUE,
        plain = TRUE,
        status = "primary",
        icon = icon("check"))
    } else {
      # If the option is not "Option 1", render an empty div
      div()
    }
  })

  output$lakename <- renderText({

    as.character(lk_hover()$Lake_name)

  })

  output$lakeinfo <- DT::renderDT({


print(input$map_shape_click)



    if(!is.null(input$map_shape_click)){


      datalk <- t(data.frame(c('Hylak_id',
                           lk_hover()$Hylak_id),
                           c('Elevation',
                           lk_hover()$Elevation),
                           c('Shore length',
                           lk_hover()$Shore_len),
                           c('Depth average',
                           lk_hover()$Depth_avg),
                           c('Volume total',
                           lk_hover()$Vol_total)))

      colnames(datalk) <- NULL



      DT::datatable(datalk,
                    rownames= FALSE,
                    options = list(ordering = FALSE,
                                   dom = 't'),
                    colnames = rep("", ncol(datalk)))


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


  # Subset of data to send in relation with user input

  data_submit <- reactive({


    if(input$modify == 'Yes'){

     data_submit <-  data.frame('siteId' = input$neositeid,
                                 'comments' = input$notes)

    }else if(input$modify == "No" & input$nooptions == "No"){

      data_submit <-  data.frame('siteId' = input$neositeid,
                                 'HYDROlake_id' = lk_hover()$Hylak_id,
                                 'comments' = input$notes)

    }else{


      data_submit <-  data.frame('siteId' = input$neositeid,
                                 'HYDROlake_id' = lk_hover()$Hylak_id,
                                 'polygon' = features$geometry,
                                 'comments' = input$notes)

    }

    data_submit
  })


# Save user selections as csv
  output$submit <- downloadHandler(
    filename = function(){
      paste0("lake_",
             input$neositeid,
             ".csv")},
    content = function(fname){
      write.csv(data_submit(), fname)
    }
  )


  # Display user comments in the screen

  output$notes <- renderText({ input$notes })

}

shinyApp(ui, server)