createpolygonUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns('polygon_new')),
    DT::DTOutput(ns('polyinfo'))
  )
}

createpolygonServer <- function(id,
                                map_draw_new_feature) {
  moduleServer(
    id,
    function(input, output, session) {


      polygon_sf <- shiny::reactive({

        # coordinates new polygon
        coords <- map_draw_new_feature()$geometry$coordinates
        coords_matrix <- sapply(coords[[1]], unlist)

        # convert the polygon to sf
        polygon <- sf::st_polygon(list(t(coords_matrix)))
        polygon_sf <- sf::st_sfc(polygon, crs = 4326)
        polygon_sf |>
          sf::st_transform(3857) # meters
      })


      # Table with HYDROlakeDB lake clicked data
      output$polyinfo <- DT::renderDT({
        if (!is.null(map_draw_new_feature())) {
          poly_4326 <- polygon_sf() |>
            sf::st_transform(4326)

          bbox <- sf::st_bbox(poly_4326)

          area <- sf::st_area(polygon_sf())
          area <- units::set_units(area,
                                   km ^ 2)

          perimeter <- sf::st_length(st_boundary(polygon_sf()))
          perimeter <- units::set_units(perimeter,
                                        km)


          datap <- t(data.frame(
            c('Min Latitude',
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
            c(
              paste0('Shore length (km)'),
              round(perimeter,
                    digits = 2)
            )
          ))

          colnames(datap) <- NULL

          DT::datatable(
            datap,
            rownames = FALSE,
            options = list(ordering = FALSE,
                           dom = 't'),
            # remove table interactive default options
            colnames = rep("", ncol(datap))
          ) # remove column names

        }

      })


      output$polygon_new <- shiny::renderPlot({
        # Message
        shiny::validate(shiny::need(
          !is.null(map_draw_new_feature()),
          "Please, create a polygon in the map"
        ))


        # plot recently created polygon
        ggplot2::ggplot() +
          ggplot2::geom_sf(data = polygon_sf(),
                           fill = 'aliceblue',
                           lwd = 0.5) +
          ggplot2::theme_void()

      })
      return(list(polygon_sf = reactive({
             polygon_sf()
        }))
        )
    }
  )
}