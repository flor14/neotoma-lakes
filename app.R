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
sf::sf_use_s2(FALSE)

# Read data

# Countries
countries_sf <- st_read('data/countries.gpkg') |>
  st_transform(3857) |>
  st_simplify(preserveTopology = TRUE,
              dTolerance = 100)

# Lakes
lakes_country <- st_read("data/lakes_country.gpkg") |>
  st_transform(3857)

# Sites
sites_country <- st_read("data/sites_country.gpkg")


# Links for navbar
link_neotoma <- tags$a(shiny::icon("github"),
                       "GitHub",
                       href = "https://github.com/NeotomaDB",
                       target = "_blank")
link_slack <- tags$a(shiny::icon("slack"),
                     "Slack",
                     href = "https://neotomadb.slack.com/ssb/redirect",
                     target = "_blank")

# UI
ui =  bslib::page_navbar(
  title = "Neotoma",
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "style.css"),
    tags$link(rel = "shortcut icon",
              href = "water.ico", # https://gauger.io/fonticon/
              type = "image/x-icon")
  ),
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "minty",
    navbar_bg = "#1b3964",
    primary = "#1b3964",
    base_font = bslib::font_google("Raleway",
                                   wght = "300"),
    heading_font = bslib::font_google("Raleway",
                                      wght = "100")
  ),
  bslib::nav_panel(
    title = "Lakes",
    shinyFeedback::useShinyFeedback(),
    bslib::layout_columns(
      col_widths = c(9, 3),
      row_heights = list("auto", 1),
      bslib::layout_sidebar(
        class = 'p-0',
        sidebar = bslib::sidebar(
          width = 300,
          title = NULL,
          open = 'always',
          bg = 'white',
          bslib::accordion(
            id = 'sidebar_accordion',
            open = c('Start Here',
                     'User options'),
            multiple = TRUE,
            height = "100%",
            bslib::accordion_panel(
              id = 'starthere',
              title = 'Start Here',
              icon = shiny::icon('arrow-down'),
              shiny::textInput(
                inputId = 'neositeid',
                label = NULL,
                placeholder = "Insert Neotoma siteID"
              ),
              bslib::tooltip(
                shiny::actionButton('search',
                                    'Search!'),
                paste(
                  "Check sitesid in the webpage.",
                  "For this demo you can use site 9606",
                  "as an example"
                )
              )
            ),
            bslib::accordion_panel(id = 'Site metadata',
                                   title = 'Site metadata',
                                   metadataUI('mod_metadata')),
            bslib::accordion_panel(
              id = 'Map options',
              title = 'Map options',
              shinyWidgets::materialSwitch(
                inputId = "removelakes",
                label = "remove HYDROlakes layer",
                status = "primary"
              )
            ),
            bslib::accordion_panel(id = 'User options',
                                   title = 'User options',
                                   optionsUI('mod_options'))
          )
        ),
        # accordion right / sidebar
        bslib::accordion(
          id = 'map_accordion',
          open = 'How to use this app?',
          height = "100%",
          bslib::accordion_panel(
            id = "Map",
            title = "Map of the site area",
            class = 'p-0',
            leafletmapUI('mod_map')
          ),
          bslib::accordion_panel(
            id = "Comments",
            title = "User decision",
            bslib::layout_columns(userdecisionsUI('mod_user_decisions'))
          ),
          bslib::accordion_panel(
            id = "help",
            title = "How to use this app?",
            class = 'p-0',
            shiny::tags$img(
              src = "img-app.png",
              height = 'auto',
              width = '100%'
            ),
            shiny::includeMarkdown("www/help.md"),
            shiny::tags$img(
              src = "img-app-end.png",
              height = 'auto',
              width = '100%'
            )
          )
        ) # closes accordion center
      ),
      # closes layout sidebar
      bslib::accordion(
        id = "lakes_or_polygons",
        open = c('About'),
        height = "100%",
        bslib::accordion_panel(
          id = "lakes",
          title = "Hydrolakes DB",
          hydrolakesUI('mod_hydrolakes')
        ),
        bslib::accordion_panel(
          id = "polygon",
          title = "Polygon",
          createpolygonUI('mod_create_polygon')
        ),
        bslib::accordion_panel(
          id = "about",
          title = "About",
          shiny::includeMarkdown("www/logo.md")
        )
      ) # closes accordion right
    ) #layout columns
  ),
  #navbar
  bslib::nav_spacer(),
  bslib::nav_menu(
    title = "Links",
    align = "right",
    bslib::nav_item(link_neotoma),
    bslib::nav_item(link_slack)
  )
)

server = function(input, output, session) {
  # Select site from the database
  neosites_data <- shiny::eventReactive(input$search, {
    neosites_data <- sites_country |>
      dplyr::mutate(siteid = as.character(siteid)) |>
      dplyr::filter(siteid %in% as.character(input$neositeid)) |>
      sf::st_transform(3857)
    return(neosites_data)
  })


  # Keep the closest lakes (50 km) to NeotomaDb selected point/polygon
  lake_data <- shiny::eventReactive(input$search, {
    lakes_country[sf::st_is_within_distance(neosites_data(),
                                            lakes_country,
                                            dist = 50000)[[1]], ] |>
      sf::st_set_crs(3857)

  })


  # left accordion
  metadataServer('mod_metadata',
                 r_neosites_data = reactive({
                   neosites_data()
                 }))

  values <- optionsServer('mod_options',
                          r_neositeid = reactive({
                            input$neositeid
                          }))


  # central accordion
  mapvalues <- leafletmapServer(
    'mod_map',
    r_neosites_data = reactive({
      neosites_data()
    }),
    r_lake_data = reactive({
      lake_data()
    }),
    nooptions = values$nooptions,
    modify = values$modify,
    removelakes = reactive({
      input$removelakes
    })
  )

  userdecisionsServer(
    'mod_user_decisions',
    neositeid = reactive({
      input$neositeid
    }),
    modify = values$modify,
    nooptions = values$nooptions,
    map_draw_new_feature = mapvalues$map_draw_new_feature,
    map_shape_click = mapvalues$map_shape_click,
    lk_click = hydrolakes$lk_click,
    polygon_sf = newpolygon$polygon_sf
  )

  # right accordion
  hydrolakes <-  hydrolakesServer(
    'mod_hydrolakes',
    countries_sf = countries_sf,
    r_neosites_data = reactive({
      neosites_data()
    }),
    r_lake_data = reactive({
      lake_data()
    }),
    map_shape_click = mapvalues$map_shape_click
  )

  newpolygon <- createpolygonServer('mod_create_polygon',
                                    map_draw_new_feature = mapvalues$map_draw_new_feature)


  shiny::observeEvent(input$search, {
    shinyFeedback::hideFeedback("neositeid")

    if (!(input$neositeid %in% unique(sites_country$siteid))) {
      shinyFeedback::feedbackDanger(inputId = "neositeid",
                                    show = TRUE,
                                    text = "Please select a valid siteId")
    }
  })

  # Open the accordion metadata only once the user selects a siteid
  shiny::observe({
    if (input$search == 1) {
      bslib::accordion_panel_open(id = 'sidebar_accordion',
                                  value = 'Site metadata')

      bslib::accordion_panel_open(id = 'map_accordion',
                                  value = 'Map of the site area')

      bslib::accordion_panel_close(id = 'map_accordion',
                                   value = 'How to use this app?')

      bslib::accordion_panel_open(id = 'lakes_or_polygons',
                                  value = 'Hydrolakes DB')

      bslib::accordion_panel_close(id = 'lakes_or_polygons',
                                   value = 'About')
    }
  })

  shiny::observe({
    if (values$modify() == "Update Current Location" ||
        values$modify() == "Accept Current Location") {
      # Open the accordion to complete and submit comments once they have selected
      # what they want to do with the siteid lake data
      bslib::accordion_panel_open(id = 'map_accordion',
                                  value = 'User decision')
    }
     else if(values$modify() == "No Action"){
       bslib::accordion_panel_close(id = 'map_accordion',
                                   value = 'User decision')
     }
    })

  shiny::observe({
    req(values$nooptions())
    if (values$modify() == "Update Current Location" &&
        values$nooptions() == "Create lake polygon") {
      # Open the accordion when the user wants to create a polygon
      bslib::accordion_panel_close(id = 'lakes_or_polygons',
                                   value = 'Hydrolakes DB')
      bslib::accordion_panel_open(id = 'lakes_or_polygons',
                                  value = 'Polygon')

    } else if (values$modify() == "Update Current Location" &&
               values$nooptions() == "Replace with HYDROLakeDB") {
      bslib::accordion_panel_close(id = 'lakes_or_polygons',
                                   value = 'Polygon')
      bslib::accordion_panel_open(id = 'lakes_or_polygons',
                                  value = 'Hydrolakes DB')
    }
  })
}
shinyApp(ui = ui, server = server)
