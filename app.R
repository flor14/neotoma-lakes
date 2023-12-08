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
library(markdown) # needed to use includeMarkdown()
library(leaflet.extras)
library(thematic)
library(shinyFeedback)
library(lwgeom) # needed to use sf::st_distance()
library(rnaturalearth)

# https://github.com/r-spatial/sf/issues/1762
sf_use_s2(FALSE)

# Apply the app style to ggplot2 plots
thematic::thematic_shiny()

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

countries_sf <- st_read('data/countries.gpkg') |> st_transform(3857) |>
  st_simplify(preserveTopology = TRUE, dTolerance=100)

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
link_neotoma <- tags$a(shiny::icon("github"),
                       "GitHub",
                       href = "https://github.com/NeotomaDB",
                       target = "_blank")
link_slack <- tags$a(shiny::icon("slack"),
                     "Slack",
                     href = "https://neotomadb.slack.com/ssb/redirect",
                     target = "_blank")


# Accordions

accordion_left <-  accordion(
  open = 'User options',
  multiple = TRUE,
  height="100%",
  id = 'sidebar_accordion',
  accordion_panel(
    id = 'Site metadata',
    title = 'Site metadata',
    DT::DTOutput('metadata')),
  accordion_panel(
    id = 'Map options',
    title = 'Map options',
    shinyWidgets::materialSwitch(
      inputId = "removelakes",
      label = "remove HYDROlakes layer",
      status = "primary"
    )),
  accordion_panel(
    id = 'User options',
    title = 'User options',
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
  ))

accordion_center <- accordion(
  id = 'map_accordion',
  open = 'How to use this app?',
  height="100%",
  accordion_panel(
    id = "Map",
    title = "Map displaying NeotomaDB and HYDROlakeDB",
    class = 'p-0',
    shinycssloaders::withSpinner(
      leafletOutput('map',
                    height = 620),
      type = 2,
      color = 'darkgreen',
      color.background = 'white')),
  accordion_panel(
    id = "Comments",
    title = "User decision",
    layout_columns(
      textOutput('action'),
      tags$div(
        textAreaInput('notes',
                      label = 'Comments:'),
        downloadButton('submit', 'Submit')))),
  accordion_panel(
    id = "help",
    title = "How to use this app?",
    tags$img(src = 'img-app.png',
             height = 'auto',
             width = '100%'),
    includeMarkdown("help.md"),
    tags$img(src = 'img-app-end.png',
             height = 'auto',
             width = '100%')))

accordion_right <- accordion(
  id = "lakes_or_polygons",
  open = "Hydrolakes DB",
  height="100%",
  accordion_panel(
    id = "lakes",
    title = "Hydrolakes DB",

      plotOutput('hydrolake',
                 height = 190),
      DT::DTOutput('lakeinfo'),
      textOutput('dist',
                 inline = TRUE),
      textOutput('countryinfo'),
      plotOutput('country',
                 height = 190)

  ),
  accordion_panel(
    id = "polygon",
    title = "Polygon" ,
    plotOutput('polygon_new'),
    DT::DTOutput('polyinfo'),

  ))


# UI

ui <-  bslib::page_navbar(
  title = "NeotomaDB",
  theme = bslib::bs_theme(version = 5,
                          bootswatch = "minty",
                          navbar_bg = "#1b3964",
                   base_font = font_google("Raleway",
                                           wght = "400"),
                   heading_font = font_google("Raleway",
                                              wght = "200")),
  bslib::nav_panel(title = "Lakes",
                   shinyFeedback::useShinyFeedback(),
            bslib::layout_columns(
    col_widths = c(9, 3),
    row_heights = list("auto", 1),
        bslib::layout_sidebar(class = "p-0",
                             sidebar = sidebar(width = 300,
                                              bg = 'white',
 textInput('neositeid',
           'SiteId',
           placeholder = "write here"),
 tooltip(
 shiny::actionButton('search', 'Search!'),
 paste("Check sitesid in the webpage.",
       "For this demo you can use site 9606 as an example")),
accordion_left),
accordion_center),
accordion_right)),
nav_spacer(),
nav_menu(
  title = "Links",
  align = "right",
  nav_item(link_neotoma),
  nav_item(link_slack)
))

server <- function(input, output, session) {


  observeEvent(input$search, {
    hideFeedback("neositeid")

    if (!(input$neositeid %in% unique(sites_country$siteid))){

      feedbackDanger(
        inputId = "neositeid",
        show = TRUE,
        text = "Please select a valid siteId"
      )}
    })
# Select site from the database
  neosites_data <- eventReactive(input$search,{

    sites_country |>
       dplyr::mutate(siteid = as.character(siteid)) |>
      dplyr::filter(siteid %in% as.character(input$neositeid)) |>
      st_transform(3857)

  })

# Keep the closest lakes (50 km) to NeotomaDb selected point/polygon
  lake_data <- eventReactive(input$search,{

    lakes_country[st_is_within_distance(neosites_data(),
                                        lakes_country,
                                        dist = 50000)[[1]],] |>
      st_set_crs(3857)

  })


  # Map
  output$map <- renderLeaflet({

  # Creation of a basic map
   lm =  leaflet::leaflet() |>
     leaflet::addTiles(group = "OpenStreetMap") |>
     leaflet::addProviderTiles("Esri.WorldImagery",
                      group = "Esri.WorldImagery",
                      options = providerTileOptions(attribution = paste('Tiles',
                      '&copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS,',
                      'AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP,',
                      'and the GIS User Community - Powered by Esri'))) |>
     leaflet::addLayersControl(
       baseGroups = c("EsriWorldImagery", "OpenStreetMap"),
       options = layersControlOptions(collapsed = FALSE)
     ) |>
        leaflet::addMeasure(primaryLengthUnit="kilometers",
                            secondaryLengthUnit="kilometers")


   neosites_data = neosites_data() |> st_transform(4326)
   lake_data = lake_data() |> st_transform(4326)

   # To zoom in the map
   bbx = st_bbox(st_union(neosites_data, lake_data))

   # The NeotomaDB site could be a point or a polygon
   if(sf::st_is(neosites_data(), "POINT")){

   lm =  lm |>
       leaflet::addPolygons(data = lake_data,
                            group = 'lakes',
                            layerId = ~Hylak_id, # https://rstudio.github.io/leaflet/showhide.html
                            weight = 5,
                            color = "blue",
                            fillColor = "lightblue") |> # hydrolakes
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
                            fillColor = "lightblue") |> # hydrolakes
       leaflet::addPolygons(data = neosites_data,
                            weight = 5,
                            color = "darkorange",
                            fillColor = "orange")  |> # neotoma sites
       leaflet::fitBounds(lng1 = bbx$xmin[[1]],
               lat1= bbx$ymin[[1]],
               lng2 = bbx$xmax[[1]],
               lat2 = bbx$ymax[[1]]) |>
       leafem::addMouseCoordinates()

   }


   # Add toolbar to draw polygons if this is requested by the user
   if(!is.null(input$nooptions) && input$nooptions == "Create lake polygon"){

    lm |>
      leaflet.extras::addDrawToolbar(markerOptions = FALSE,
                                     circleMarkerOptions = FALSE,
                                     polylineOptions = FALSE,
                                     circleOptions = FALSE,
                                     rectangleOptions = FALSE,
                                     singleFeature = TRUE)

   }else{ lm }

  })


# The user can request to hide the lakes layer
  observeEvent(input$removelakes, {

    proxy <- leafletProxy('map')

    print(input$nooptions)

   if(input$removelakes == TRUE){
       proxy |>  hideGroup('lakes')
   }else{
       proxy |>  showGroup('lakes')}
    }
  )


  # The user can request to hide the lakes layer
  observe({
        req(input$nooptions)

        proxy <- leafletProxy('map')

    if(input$modify == 'No' &&
       input$nooptions == "Create lake polygon"){

      proxy |>  hideGroup('lakes')

    }else{
      proxy |>  showGroup('lakes')}})



# Open the accordion metadata only once the user selects a siteid
observe({

  if(input$search == 1){
    accordion_panel_open(id = 'sidebar_accordion',
                       value = 'Site metadata')

    accordion_panel_open(id = 'map_accordion',
                         value = 'Map displaying NeotomaDB and HYDROlakeDB')

    accordion_panel_close(id = 'map_accordion',
                          value = 'How to use this app?')

  }


  if(input$modify == "No"){
  # Open the accordion to complete and submit comments once they have selected
  # what they want to do with the siteid lake data
  accordion_panel_open(id = 'map_accordion',
                        value = 'User decision')} })

observe({

  req(input$nooptions)
  if(input$modify == "No" && input$nooptions == "Create lake polygon"){

    # Open the accordion when the user wants to create a polygon
    accordion_panel_close(id = 'lakes_or_polygons',
                         value = 'Hydrolakes DB')
    accordion_panel_open(id = 'lakes_or_polygons',
                         value = 'Polygon')

  }else if(input$modify == "No" &&
          input$nooptions == "Replace with HYDROLakeDB"){

  accordion_panel_close(id = 'lakes_or_polygons',
                        value = 'Polygon')
  accordion_panel_open(id = 'lakes_or_polygons',
                       value = 'Hydrolakes DB')}
})



# Sidebar table withe the NeotomaDB siteId metadata
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

  # Add radiobuttons if the user selects 'no' as option
  output$dynamicUI <- renderUI({
    if (input$modify == "No") {
      prettyRadioButtons( # or prettyRadioButtons
        inputId = "nooptions",
        label = "How would you update NeotomaDB?",
        choices = c("Replace with HYDROLakeDB" ,
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

  # Select HYDROlakeDB polygon when clicking the map
  lk_click <- reactive({

    req(input$map_shape_click)

    lake_data = lake_data() |> st_transform(4326)

    if(!is.null(input$map_shape_click$id)){

    # convert hover coordinates in a sfc point
    p = st_sfc(st_point(x=c(input$map_shape_click$lng,
                            input$map_shape_click$lat),
                        dim="XY"),
               crs = 4326)

    # detect detect polygon hovered by the user
    lk_click = lake_data[st_intersects(lake_data, p, sparse = FALSE),]
    lk_click

    }
  })


# Display plot clicked on the right sidebar
  output$hydrolake <- renderPlot({

    lake_data = lake_data() |> st_transform(4326)
    countries_sf |> st_transform(4326)

    req(input$search)

    if(!is.null(input$map_shape_click$id)){

      # plot polygon of lake of interest
      ggplot()+
        geom_sf(data = lk_click(),
                fill = 'aliceblue',
                lwd = 0.5)+
        theme_void() +
        ggtitle(paste(as.character(lk_click()$Lake_name))) +
        theme(text = element_text(size = 15))

    }
  })


  # Hydrolake clicked distance to NeotomaDB selected point
  output$dist <- renderText({
    req(input$search)

    # Message
    validate(
      need(!is.null(input$map_shape_click),
           "Please, click one of the HYDROlakes in the map")
    )

    neosites_data = neosites_data() |> st_transform(4326)
    lk_click = lk_click() |> st_transform(4326)

    paste0("Distance within the site: ",
           round((st_distance(lk_click, neosites_data))/1000,
                 digits = 2),
           ' km')

  })





# Table with HYDROlakeDB lake clicked data
  output$lakeinfo <- DT::renderDT({

      if(!is.null(input$map_shape_click)){


      datalk <- t(data.frame(c('Hylak_id',
                           lk_click()$Hylak_id),
                           c('Elevation',
                           lk_click()$Elevation),
                           c('Shore length',
                           lk_click()$Shore_len),
                           c('Depth average',
                           lk_click()$Depth_avg),
                           c('Volume total',
                           lk_click()$Vol_total)))

      colnames(datalk) <- NULL

      DT::datatable(datalk,
                    rownames= FALSE,
                    options = list(ordering = FALSE,
                                   dom = 't'), # remove table interactive default options
                    colnames = rep("", ncol(datalk))) # remove column names

    }

  })


# Map of the country where the lake is placed
  output$country <- renderPlot({

    req(input$map_shape_click$id)


    countries_sf |> st_transform(4326)

    if(!is.null(input$map_shape_click$id)){

      csf =  countries_sf |>
        dplyr::filter(COUNTRY == lk_click()$COUNTRY)
      print(csf)
      # plot polygon of lake of interest

      world = ne_countries(scale = 'small',
                           returnclass = "sf")

      ggplot() +

         geom_sf(data = world,
                fill= 'antiquewhite') +
        geom_sf(data = csf,
                fill = 'darkgreen',
                lwd = 0.5) +
        theme_void() +
        coord_sf(crs = "+proj=moll") +
        theme(panel.grid.major = element_line(color = gray(.5),
                                              linetype = 'dashed',
                                              linewidth = 0.5))

  }
})


# Add country name
  output$countryinfo <- renderText({

    req(input$map_shape_click$id)

      if(!is.null(input$map_shape_click$id)){
      csf =  countries_sf |>
        dplyr::filter(COUNTRY == lk_click()$COUNTRY)

      paste("Country:", as.character(lk_click()$COUNTRY))

    }
  })


  # Display user comments in the screen
  output$notes <- renderText({ input$notes })

  # Text
  output$action <- renderText({



    if(input$modify == 'Yes'){

      paste("The NeotomaDB site", input$neositeid, "is correct and don't need to be replaced")

    }else if(input$nooptions == "Replace with HYDROLakeDB"){


      # Message
      validate(
        need(!is.null(input$map_shape_click),
             "Please, click one of the HYDROlakes in the map")
      )

      paste("The NeotomaDB site", input$neositeid,
            "should be replaced with the HYDROLakeDB",
            lk_click()$Hylak_id)

    }else{

      # Message
      validate(
        need(!is.null(input$map_draw_new_feature),
             "Please, create a polygon in the map")
      )


      paste("The NeotomaDB site", input$neositeid,
            "can be replaced with the polygon that I am submitting")
    }

  })


  polygon_sf <- reactive({

    coords = input$map_draw_new_feature$geometry$coordinates
    coords_matrix <- sapply(coords[[1]], unlist)
    polygon <- st_polygon(list(t(coords_matrix)))
    polygon_sf <- st_sfc(polygon, crs = 4326) # meters
    polygon_sf |> st_transform(3857)
  })


  # Subset of data to send in relation with user input
  data_submit <- reactive({



    if(input$modify == 'Yes'){

     data_submit <-  data.frame('siteId' = input$neositeid,
                                 'comments' = input$notes)

    }else if(input$modify == "No" &&
             input$nooptions == "Replace with HYDROLakeDB"){

      data_submit <-  data.frame('siteId' = input$neositeid,
                                 'HYDROlake_id' = lk_click()$Hylak_id,
                                 'comments' = input$notes)

    }else if(input$modify == "No" &&
             input$nooptions == "Create lake polygon"){



      if(!is.null(input$map_draw_new_feature)){

         polygon_sf()
      }


      data_submit <-  data.frame('siteId' = input$neositeid,
                                 'comments' = input$notes)

      poly_submit = st_sf(data_submit, geometry = polygon_sf())
      poly_submit
     }



  })


# Save user selections as csv
  output$submit <- downloadHandler(
    filename = function(){
      paste0("lake_",
             input$neositeid,
             ".csv")},
    content = function(fname){


      if(input$modify == 'Yes'){

        print(data_submit())
        write.csv(data_submit(), fname)

      }else if(input$modify == 'No' &&
               input$nooptions == 'Create lake polygon'){

        print(data_submit())
        st_write(data_submit(), fname, layer_options = "GEOMETRY=AS_WKT")

      }else{

        print(data_submit())
        write.csv(data_submit(), fname)

      }

    }
  )


  # Table with HYDROlakeDB lake clicked data
  output$polyinfo <- DT::renderDT({

    if(!is.null(input$map_draw_new_feature)){

      bbox <- st_bbox(polygon_sf())
      units <- st_crs(polygon_sf(),
                      parameters = TRUE)$units_gdal
      print(st_crs(polygon_sf(),
                   parameters = TRUE))
      datap <- t(data.frame(c('Min Latitude',
                              bbox$xmin),
                            c('Max Latitude',
                              bbox$xmax),
                            c('Min Longitude',
                              bbox$ymin),
                            c('Max Longitude',
                              bbox$ymax),
                            c(paste0('Area (', units, ')'),
                               round(st_area(polygon_sf()),
                                     digits = 2)),
                             c(paste0('Shore length (', units, ')'),
                               round(st_length(st_boundary(polygon_sf())),
                                     digits = 2))))

      colnames(datap) <- NULL

      DT::datatable(datap,
                    rownames= FALSE,
                    options = list(ordering = FALSE,
                                   dom = 't'), # remove table interactive default options
                    colnames = rep("", ncol(datap))) # remove column names

    }

  })


  output$polygon_new <- renderPlot({

    # Message
    validate(
      need(!is.null(input$map_draw_new_feature),
           "Please, create a polygon in the map")
    )

    # plot recently created polygon
      ggplot()+
        geom_sf(data = polygon_sf(),
                fill = 'aliceblue',
                lwd = 0.5)+
        theme_void()

  })


}

shinyApp(ui, server)