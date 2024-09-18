userdecisionsUI <- function(id) {
  ns <- NS(id)
  tagList(layout_columns(
    col_widths = c(4,8),
          shiny::textOutput(ns('action')),
          div(
          shiny::tags$div(shiny::textAreaInput(ns('notes'),
                                               label = 'Comments:',
                                               width = '100%'),
          shiny::downloadButton(ns('submit'), 'Submit')))))
}

userdecisionsServer <- function(id,
                                modify,
                                nooptions,
                                neositeid,
                                map_draw_new_feature,
                                map_shape_click,
                                lk_click,
                                polygon_sf) {
  moduleServer(id,
               function(input, output, session) {

                 # Display user comments in the screen
                 output$notes <- shiny::renderText({
                   input$notes
                 })

                 # Text
                 output$action <- shiny::renderText({

                   if (modify() == "No Action"){
                     # Message
                     shiny::validate(shiny::need(
                       modify() == "No Action"))
                       "Please, select an action"

                   }else if (modify() == 'Accept Current Location') {
                     paste("The NeotomaDB site",
                           neositeid(),
                           "is correct and don't need to be replaced")
                   } else if (modify() == "Update Current Location" &&
                              nooptions() == "Replace with HYDROLakeDB") {
                     # Message
                     shiny::validate(shiny::need(
                       !is.null(map_shape_click()),
                       "Please, click one of the HYDROlakes in the map"
                     ))

                     paste(
                       "The NeotomaDB site",
                       neositeid(),
                       "should be replaced with the HYDROLakeDB",
                       lk_click()$Hylak_id
                     )

                   } else{
                     # Message
                     shiny::validate(shiny::need(
                       !is.null(map_draw_new_feature()),
                       "Please, create a polygon in the map"
                     ))
                     paste(
                       "The NeotomaDB site",
                       neositeid(),
                       "can be replaced with the polygon that I am submitting"
                     )
                   }
                 })

                 # SUBMIT
                 data_submit <- shiny::reactive({
                 if (modify() == 'Accept Current Location') {
                   df <- data.frame('siteId' = neositeid(),
                                    'comments' = input$notes)

                   df

                 } else if (modify() == "Update Current Location" &&
                            nooptions() == "Replace with HYDROLakeDB") {
                   df <-   data.frame(
                     'siteId' = neositeid(),
                     'HYDROlake_id' = lk_click()$Hylak_id,
                     'comments' = input$notes
                   )

                   df

                 } else if (modify() == "Update Current Location" &&
                            nooptions() == "Create lake polygon") {
                   if (!is.null(map_draw_new_feature())) {

                    st_as_sf(polygon_sf())


                     df <-  data.frame('siteId' = neositeid(),
                                       'comments' = input$notes)
                     polydata <- sf::st_sf(df,
                               geometry = polygon_sf())

                     polydata }

                 }
               })

  # Save user selections as csv
  output$submit <- shiny::downloadHandler(
    filename = function() {
      paste0("lake_",
             neositeid(),
             ".csv")
    },
    content = function(fname) {

      if (
          modify() == 'Accept Current Location') {
        write.csv(data_submit(), fname)

      } else if (modify() == 'Update Current Location' &&
                 nooptions() == 'Create lake polygon') {

        sf::st_write(data_submit(), fname,
                     layer_options = "GEOMETRY=AS_WKT")

      } else if(modify() == 'Update Current Location' &&
                nooptions() == 'Replace with HYDROLakeDB' ){

        write.csv(data_submit(), fname)

      }
    }
  )
} )}