# https://github.com/r-spatial/sf/issues/1762
sf::sf_use_s2(FALSE)


# Links
link_neotoma <- shiny::tags$a(shiny::icon("github"),
                       "GitHub",
                       href = "https://github.com/NeotomaDB",
                       target = "_blank")

link_slack <- shiny::tags$a(shiny::icon("slack"),
                     "Slack",
                     href = "https://neotomadb.slack.com/ssb/redirect",
                     target = "_blank")


# UI
# accordion_left
accordion_left <-  bslib::accordion(
  open = 'User options',
  multiple = TRUE,
  height="100%",
  id = 'sidebar_accordion',
  bslib::accordion_panel(
    id = 'Site metadata',
    title = 'Site metadata',
    DT::DTOutput('metadata')),
  bslib::accordion_panel(
    id = 'Map options',
    title = 'Map options',
    shinyWidgets::materialSwitch(
      inputId = "removelakes",
      label = "remove HYDROlakes layer",
      status = "primary"
    )),
  bslib::accordion_panel(
    id = 'User options',
    title = 'User options',
    shinyWidgets::prettyRadioButtons(
      inputId = "modify",
      label = "Is this site the correct lake polygon in NeotomaDB?",
      choices = c("Yes", "No"),
      outline = TRUE,
      plain = TRUE,
      status = "primary",
      icon = shiny::icon("check")
    ),
    shiny::uiOutput('dynamicUI')
  ))

# accordion_center
accordion_center <- bslib::accordion(
  id = 'map_accordion',
  open = 'How to use this app?',
  height="100%",
  bslib::accordion_panel(
    id = "Map",
    title = "Map of the site area",
    class = 'p-0',
    shinycssloaders::withSpinner(
      leaflet::leafletOutput('map',
                    height = 620),
      type = 2,
      color = '#1b3964',
      color.background = 'white')),
  bslib::accordion_panel(
    id = "Comments",
    title = "User decision",
    bslib::layout_columns(
      shiny::textOutput('action'),
      shiny::tags$div(
        shiny::textAreaInput('notes',
                      label = 'Comments:'),
        shiny::downloadButton('submit', 'Submit')))),
  bslib::accordion_panel(
    id = "help",
    title = "How to use this app?",
    shiny::tags$img(src = "img_app.png",
             height = 'auto',
             width = '100%'),
    shiny::includeMarkdown("help.md"),
    shiny::tags$img(src = "img_app_end.png",
             height = 'auto',
             width = '100%')))

# accordion_right
accordion_right <- bslib::accordion(
  id = "lakes_or_polygons",
  height="100%",
  open = FALSE,
  bslib::accordion_panel(
    id = "lakes",
    title = "Hydrolakes DB",

    shiny::plotOutput('hydrolake',
               height = 190),
    DT::DTOutput('lakeinfo'),
    shiny::textOutput('dist',
               inline = TRUE),
    shiny::textOutput('countryinfo'),
    shiny::plotOutput('country',
               height = 190)

  ),
  bslib::accordion_panel(
    id = "polygon",
    title = "Polygon" ,
    shiny::plotOutput('polygon_new'),
    DT::DTOutput('polyinfo'),

  ))


# UI
ui =  bslib::page_navbar(
  title = "NeotomaDB",
  theme = bslib::bs_theme(version = 5,
                          bootswatch = "minty",
                          navbar_bg = "#1b3964",
                          primary = "#1b3964",
                          base_font = bslib::font_google("Raleway",
                                                  wght = "400"),
                          heading_font = bslib::font_google("Raleway",
                                                     wght = "200")),
  bslib::nav_panel(title = "Lakes",
                   shinyFeedback::useShinyFeedback(),
                   bslib::layout_columns(
                     col_widths = c(9, 3),
                     row_heights = list("auto", 1),
                     bslib::layout_sidebar(class = "p-0",
                                           sidebar = bslib::sidebar(
                                                    width = 300,
                                                     bg = 'white',
                                                     shiny::textInput(
                                                            'neositeid',
                                                            'NeotomaDB',
                                                    placeholder = "Insert siteID"),
                                                    bslib::tooltip(
                                                    shiny::actionButton('search',
                                                                        'Search!'),
                                                    paste("Check sitesid in the webpage.",
                                                    "For this demo you can use site 9606",
                                                    "as an example")),
                                                             accordion_left),
                                           accordion_center),
                     accordion_right)),
  bslib::nav_spacer(),
  bslib::nav_menu(
    title = "Links",
    align = "right",
    bslib::nav_item(link_neotoma),
    bslib::nav_item(link_slack)
  ))

server = function(input, output, session) {


  shiny::observeEvent(input$search, {
    shinyFeedback::hideFeedback("neositeid")

    if (!(input$neositeid %in% unique(sites_country$siteid))){

      shinyFeedback::feedbackDanger(
        inputId = "neositeid",
        show = TRUE,
        text = "Please select a valid siteId"
      )}
  })
  # Select site from the database
  neosites_data <- shiny::eventReactive(input$search,{

    sites_country |>
      dplyr::mutate(siteid = as.character(siteid)) |>
      dplyr::filter(siteid %in% as.character(input$neositeid)) |>
      sf::st_transform(3857)

  })

  # Keep the closest lakes (50 km) to NeotomaDb selected point/polygon
  lake_data <- shiny::eventReactive(input$search,{

    lakes_country[sf::st_is_within_distance(neosites_data(),
                                           lakes_country,
                                           dist = 50000)[[1]],] |>
      sf::st_set_crs(3857)

  })


  # Map
  output$map <- leaflet::renderLeaflet({

    # Creation of a basic map
    lm  <-   leaflet::leaflet() |>
      leaflet::addTiles(group = "OpenStreetMap") |>
      leaflet::addProviderTiles("Esri.WorldImagery",
              group = "Esri.WorldImagery",
              options = leaflet::providerTileOptions(attribution = paste(
                     'Tiles',
                      '&copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS,',
                      'AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP,',
                      'and the GIS User Community - Powered by Esri'))) |>
      leaflet::addLayersControl(
        baseGroups = c("EsriWorldImagery",
                       "OpenStreetMap"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) |>
      leaflet::addMeasure(primaryLengthUnit="kilometers",
                          secondaryLengthUnit="kilometers")


    neosites_data = neosites_data() |>
      sf::st_transform(4326)
    lake_data = lake_data() |>
      sf::st_transform(4326)

    # To zoom in the map
    bbx = sf::st_bbox(sf::st_union(neosites_data,
                                   lake_data))

    # The NeotomaDB site could be a point or a polygon
    if(sf::st_is(neosites_data(), "POINT")){

      lm  <-   lm |>
        leaflet::addPolygons(data = lake_data,
                             group = 'lakes',
                             layerId = ~Hylak_id, # https://rstudio.github.io/leaflet/showhide.html
                             weight = 5,
                             color = "blue",
                             fillColor = "lightblue") |> # hydrolakes
        leaflet::addMarkers(data = neosites_data) |> # neotoma sites
        leaflet::fitBounds(lng1 = bbx$xmin[[1]],
                  lat1= bbx$ymin[[1]],
                  lng2 = bbx$xmax[[1]],
                  lat2 = bbx$ymax[[1]]) |>
        leafem::addMouseCoordinates()

    }else{

      lm <-  lm |>
        leaflet::addPolygons(data = lake_data,
                             group = 'lakes',
                             layerId = ~Hylak_id, # https://rstudio.github.io/leaflet/showhide.html
                             weight = 5,
                             color = "blue",
                             fillColor = "lightblue") |> # hydrolakes
        leaflet::addPolygons(data = neosites_data,
                             weight = 5,
                             color = "#A5243D",
                             fillColor = "pink") |> # neotoma sites
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
  shiny::observeEvent(input$removelakes, {

    proxy <- leaflet::leafletProxy('map')

    if(input$removelakes == TRUE){
      proxy |>  leaflet::hideGroup('lakes')
    }else{
      proxy |>  leaflet::showGroup('lakes')}
  }
  )


  # The user can request to hide the lakes layer
  shiny::observe({
    shiny::req(input$nooptions)

    proxy <- leaflet::leafletProxy('map')

    if(input$modify == 'No' &&
       input$nooptions == "Create lake polygon"){

      proxy |>  leaflet::hideGroup('lakes')

    }else{
      proxy |>  leaflet::showGroup('lakes')}})



  # Open the accordion metadata only once the user selects a siteid
  shiny::observe({

    if(input$search == 1){
      bslib::accordion_panel_open(id = 'sidebar_accordion',
                           value = 'Site metadata')

      bslib::accordion_panel_open(id = 'map_accordion',
                           value = 'Map of the site area')

      bslib::accordion_panel_close(id = 'map_accordion',
                            value = 'How to use this app?')

      bslib::accordion_panel_open(id = 'lakes_or_polygons',
                                  value = 'Hydrolakes DB')

    }


    if(input$modify == "No"){
      # Open the accordion to complete and submit comments once they have selected
      # what they want to do with the siteid lake data
      bslib::accordion_panel_open(id = 'map_accordion',
                           value = 'User decision')} })

  shiny::observe({

    shiny::req(input$nooptions)

    if(input$modify == "No" &&
       input$nooptions == "Create lake polygon"){

      # Open the accordion when the user wants to create a polygon
      bslib::accordion_panel_close(id = 'lakes_or_polygons',
                            value = 'Hydrolakes DB')
      bslib::accordion_panel_open(id = 'lakes_or_polygons',
                           value = 'Polygon')

    }else if(input$modify == "No" &&
             input$nooptions == "Replace with HYDROLakeDB"){

      bslib::accordion_panel_close(id = 'lakes_or_polygons',
                            value = 'Polygon')
      bslib::accordion_panel_open(id = 'lakes_or_polygons',
                           value = 'Hydrolakes DB')}
  })



  # Sidebar table withe the NeotomaDB siteId metadata
  output$metadata <- DT::renderDT({

    name  <-  as.character(neosites_data()$sitename)
    description  <-  as.character(neosites_data()$sitedescription)
    notes  <-  as.character(neosites_data()$notes)

    data <- data.frame(c(description,
                         'notes', notes))

    colnames(data) <- as.character(name)

    DT::datatable(data,
                  rownames= FALSE,
                  options = list(dom = 't',
                                 ordering = FALSE))

  })

  # Add radiobuttons if the user selects 'no' as option
  output$dynamicUI <- shiny::renderUI({
    if (input$modify == "No") {
      shinyWidgets::prettyRadioButtons( # or prettyRadioButtons
        inputId = "nooptions",
        label = "How would you update NeotomaDB?",
        choices = c("Replace with HYDROLakeDB" ,
                    "Create lake polygon"),
        outline = TRUE,
        plain = TRUE,
        status = "primary",
        icon = shiny::icon("check"))
    } else {
      # If the option is not "Option 1", render an empty div
      shiny::div()
    }
  })

  # Select HYDROlakeDB polygon when clicking the map
  lk_click <- shiny::reactive({

    shiny::req(input$map_shape_click)

    lake_data = lake_data() |>
      sf::st_transform(4326)

    if(!is.null(input$map_shape_click$id)){

      # convert hover coordinates in a sfc point
      p = sf::st_sfc(sf::st_point(x=c(input$map_shape_click$lng,
                              input$map_shape_click$lat),
                          dim="XY"),
                 crs = 4326)

      # detect detect polygon hovered by the user
      lk_click  <-  lake_data[sf::st_intersects(lake_data, p,
                                             sparse = FALSE),]
      lk_click

    }
  })


  # Display plot clicked on the right sidebar
  output$hydrolake <- shiny::renderPlot({

    shiny::req(input$search)

    lake_data <-  lake_data() |> sf::st_transform(4326)
    countries_sf |> sf::st_transform(4326)

    if(!is.null(input$map_shape_click$id)){


      # plot polygon of lake of interest
      ggplot2::ggplot()+
        ggplot2::geom_sf(data = lk_click(),
                fill = 'aliceblue',
                lwd = 0.5)+
        ggplot2::theme_void() +
        ggplot2::ggtitle(paste(as.character(lk_click()$Lake_name))) +
        ggplot2::theme(text = ggplot2::element_text(size = 15))

    }
  })


  # Hydrolake clicked distance to NeotomaDB selected point
  output$dist <- shiny::renderText({

     shiny::req(input$search)

    # Message if there is no click on a lake
    shiny::validate(
      shiny::need(!is.null(input$map_shape_click),
           "Please, click one of the HYDROlakes in the map")
    )

    neosites_data  <-  neosites_data() |>
      sf::st_transform(4326)
    lk_click = lk_click() |>
      sf::st_transform(4326)

    paste0("Distance within the site: ",
           round((sf::st_distance(lk_click,
                                  neosites_data))/1000,
                 digits = 2),
           ' km')

  })

  # Table with HYDROlakeDB lake clicked data
  output$lakeinfo <- DT::renderDT({

    if(!is.null(input$map_shape_click)){


      datalk <- t(data.frame(c('Hylak_id',
                               lk_click()$Hylak_id),
                             c('Elevation (masl)',
                               lk_click()$Elevation),
                             c('Shore length (km)',
                               lk_click()$Shore_len),
                             c('Lake area (km^2)',
                               lk_click()$Lake_area),
                             c('Volume total (mcm)',
                               lk_click()$Vol_total),
                             c('Depth average (m)',
                               lk_click()$Depth_avg)))

      colnames(datalk) <- NULL

      DT::datatable(datalk,
                    rownames= FALSE,
                    options = list(ordering = FALSE,
                                   dom = 't'), # remove table interactive default options
                    colnames = rep("", ncol(datalk))) # remove column names

    }

  })


  # Map of the country where the lake is placed
  output$country <- shiny::renderPlot({

    shiny::req(input$map_shape_click$id)

    countries_sf |>
      sf::st_transform(4326)

    if(!is.null(input$map_shape_click$id)){

      csf  <- countries_sf |>
        dplyr::filter(COUNTRY == lk_click()$COUNTRY)

      # plot polygon of lake of interest
      world <-  rnaturalearth::ne_countries(scale = 'small',
                           returnclass = "sf")

      ggplot2::ggplot() +
        ggplot2::geom_sf(data = world,
                fill= 'antiquewhite') +
        ggplot2::geom_sf(data = csf,
                fill = '#A5243D',
                lwd = 0.5) +
        ggplot2::theme_void() +
        ggplot2::coord_sf(crs = "+proj=moll") +
        ggplot2::theme(panel.grid.major = ggplot2::element_line(color = gray(.5),
                                              linetype = 'dashed',
                                              linewidth = 0.5))

    }
  })


  # Add country name
  output$countryinfo <- shiny::renderText({

    shiny::req(input$map_shape_click$id)

    if(!is.null(input$map_shape_click$id)){
      csf =  countries_sf |>
        dplyr::filter(COUNTRY == lk_click()$COUNTRY)

      paste("Country:", as.character(lk_click()$COUNTRY))

    }
  })


  # Display user comments in the screen
  output$notes <- shiny::renderText({ input$notes })

  # Text
  output$action <- shiny::renderText({



    if(input$modify == 'Yes'){

      paste("The NeotomaDB site",
            input$neositeid, "is correct and don't need to be replaced")

    }else if(input$nooptions == "Replace with HYDROLakeDB"){


      # Message
      shiny::validate(
        shiny::need(!is.null(input$map_shape_click),
             "Please, click one of the HYDROlakes in the map")
      )

      paste("The NeotomaDB site", input$neositeid,
            "should be replaced with the HYDROLakeDB",
            lk_click()$Hylak_id)

    }else{

      # Message
      shiny::validate(
        shiny::need(!is.null(input$map_draw_new_feature),
             "Please, create a polygon in the map")
      )


      paste("The NeotomaDB site", input$neositeid,
            "can be replaced with the polygon that I am submitting")
    }

  })


  polygon_sf <- shiny::reactive({

    # coordinates new polygon
    coords <- input$map_draw_new_feature$geometry$coordinates
    coords_matrix <- sapply(coords[[1]], unlist)

    # convert the polygon to sf
    polygon <- sf::st_polygon(list(t(coords_matrix)))
    polygon_sf <- sf::st_sfc(polygon, crs = 4326)
    polygon_sf |>
      sf::st_transform(3857) # meters
  })


  # Subset of data to send in relation with user input
  data_submit <- shiny::reactive({

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

      poly_submit <-  sf::st_sf(data_submit, geometry = polygon_sf())
      poly_submit
    }



  })


  # Save user selections as csv
  output$submit <- shiny::downloadHandler(
    filename = function(){
      paste0("lake_",
             input$neositeid,
             ".csv")},
    content = function(fname){


      if(input$modify == 'Yes'){

        write.csv(data_submit(), fname)

      }else if(input$modify == 'No' &&
               input$nooptions == 'Create lake polygon'){


        sf::st_write(data_submit(), fname,
                     layer_options = "GEOMETRY=AS_WKT")

      }else{

        write.csv(data_submit(), fname)

      }

    }
  )


  # Table with HYDROlakeDB lake clicked data
  output$polyinfo <- DT::renderDT({

    if(!is.null(input$map_draw_new_feature)){

      poly_4326 <- polygon_sf() |>
        sf::st_transform(4326)

      bbox <- sf::st_bbox(poly_4326)

      area <- sf::st_area(polygon_sf())
      area <- units::set_units(area,
                               km^2)

      perimeter <- sf::st_length(st_boundary(polygon_sf()))
      perimeter <- units::set_units(perimeter,
                                    km)


      datap <- t(data.frame(c('Min Latitude',
                              round(bbox$xmin,
                                    digit = 2)),
                            c('Max Latitude',
                              round(bbox$xmax,
                                    digit = 2)),
                            c('Min Longitude',
                              round(bbox$ymin,
                                    digit = 2)),
                            c('Max Longitude',
                              round(bbox$ymax,
                                    digit = 2)),
                            c(paste0('Area (km^2)'),
                              round(area,
                                    digits = 2)),
                            c(paste0('Shore length (km)'),
                              round(perimeter,
                                    digits = 2))))

      colnames(datap) <- NULL

      DT::datatable(datap,
                    rownames= FALSE,
                    options = list(ordering = FALSE,
                                   dom = 't'), # remove table interactive default options
                    colnames = rep("", ncol(datap))) # remove column names

    }

  })


  output$polygon_new <- shiny::renderPlot({

    # Message
    shiny::validate(
      shiny::need(!is.null(input$map_draw_new_feature),
           "Please, create a polygon in the map")
    )


    # plot recently created polygon
    ggplot2::ggplot()+
      ggplot2::geom_sf(data = polygon_sf(),
              fill = 'aliceblue',
              lwd = 0.5)+
      ggplot2::theme_void()

  })


}
shinyApp(ui, server)


