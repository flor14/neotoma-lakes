hydrolakesUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns('hydrolake'),
                      height = 190),
    DT::DTOutput(ns('lakeinfo')),
    shiny::textOutput('dist',
                      inline = TRUE),
    shiny::textOutput(ns('countryinfo')),
    shiny::plotOutput(ns('country'),
                      height = 190)
  )
}

hydrolakesServer <- function(id,
                             countries_sf,
                             r_lake_data,
                             map_shape_click,
                             r_neosites_data) {
  moduleServer(
    id,
    function(input, output, session) {

      # Select HYDROlakeDB polygon when clicking the map
      lk_click <- eventReactive(map_shape_click(), {
        lake_data_4326  <-  r_lake_data() |>
          sf::st_transform(4326)

        if (!is.null(map_shape_click()$id)) {
          # convert hover coordinates in a sfc point
          p  <-
            sf::st_sfc(sf::st_point(
              x = c(
                map_shape_click()$lng,
                map_shape_click()$lat
              ),
              dim = "XY"
            ),
            crs = 4326)

          # detect detect polygon hovered by the user
          lake_data_4326[sf::st_intersects(lake_data_4326,
                                           p,
                                           sparse = FALSE), ]

        }}
      )
      mc_text <- eventReactive(lk_click(),{

        mc_neosites_data  <-  neosites_data() |>
          sf::st_transform(4326)

        if(!is.null(mapvalues$map_shape_click())){
          mc_lk_click  <-  lk_click$df |>
            sf::st_transform(4326)

          paste0("Distance within the site: ",
                 round((sf::st_distance(mc_lk_click,
                                        mc_neosites_data))/1000,
                       digits = 2),
                 ' km') }

      })

      # Hydrolake clicked distance to NeotomaDB selected point
      output$dist <- shiny::renderText({

        # Message if there is no click on a lake
        shiny::validate(
          shiny::need(!is.null(map_shape_click()),
                      "Please, click one of the HYDROlakes in the map")
        )
        mc_text()

      })



      # Display plot clicked on the right sidebar
      output$hydrolake <- shiny::renderPlot({
        shiny::req(r_neosites_data())

        lake_data <- r_lake_data() |>
          sf::st_transform(4326)

        countries_sf <- countries_sf |>
          sf::st_transform(4326)

        if (!is.null(map_shape_click()$id)) {
          # plot polygon of lake of interest
          ggplot2::ggplot() +
            ggplot2::geom_sf(data = lk_click(),
                             fill = 'aliceblue',
                             lwd = 0.5) +
            ggplot2::theme_void() +
            ggplot2::ggtitle(paste(as.character(lk_click()$Lake_name))) +
            ggplot2::theme(text = ggplot2::element_text(size = 15))

        }
      })

      # Table with HYDROlakeDB lake clicked data
      output$lakeinfo <- DT::renderDT({
        if (!is.null(map_shape_click())) {
          datalk <- t(data.frame(
            c('Hylak_id',
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
              lk_click()$Depth_avg)
          ))

          colnames(datalk) <- NULL

          DT::datatable(
            datalk,
            rownames = FALSE,
            options = list(ordering = FALSE,
                           dom = 't'),
            # remove table interactive default options
            colnames = rep("", ncol(datalk))
          ) # remove column names

        }

      })

      # Map of the country where the lake is placed
      output$country <- shiny::renderPlot({
        shiny::req(map_shape_click()$id)

       if (!is.null(map_shape_click()$id)) {

          csf  <- countries_sf |>
            sf::st_transform(4326) |>
            dplyr::filter(COUNTRY == lk_click()$COUNTRY)

          # plot polygon of lake of interest
          world <-  rnaturalearth::ne_countries(scale = 'small',
                                                returnclass = "sf")

          ggplot2::ggplot() +
            ggplot2::geom_sf(data = world,
                             fill = 'antiquewhite') +
            ggplot2::geom_sf(data = csf,
                             fill = '#A5243D',
                             lwd = 0.5) +
            ggplot2::theme_void() +
            ggplot2::coord_sf(crs = "+proj=moll") +
            ggplot2::theme(
              panel.grid.major = ggplot2::element_line(
                color = gray(.5),
                linetype = 'dashed',
                linewidth = 0.5
              )
            )

        }
      })


      # Add country name
      output$countryinfo <- shiny::renderText({
        shiny::req(map_shape_click()$id)

        if (!is.null(map_shape_click()$id)) {
          csf  <- countries_sf |>
            dplyr::filter(COUNTRY == lk_click()$COUNTRY)

          paste("Country:", as.character(lk_click()$COUNTRY))

        }
      })

      return(list(lk_click = reactive({ lk_click() })))
    }
  )
}