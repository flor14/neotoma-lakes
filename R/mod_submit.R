# submitUI <- function(id) {
#   ns <- NS(id)
#   tagList(shiny::downloadButton(ns('submit'), 'Submit'))
# }
#
# submitServer <- function(id,
#                          r_neositeid,
#                          modify,
#                          nooptions,
#                          lk_click,
#                          notes,
#                          map_draw_new_feature,
#                          polygon_sf) {
#   moduleServer(id,
#                function(input, output, session) {
#                  # Subset of data to send in relation with user input
#                  data_submit <- shiny::reactive({
#
#                    print('data')
#                    print(r_neositeid())
#                    print(modify())
#                    print(nooptions())
#                    print(lk_click())
#                    print(notes())
#                    print(map_draw_new_feature())
#                    print(polygon_sf())
#
#                    if (modify() == 'Yes') {
#                      df <- data.frame('siteId' = r_neositeid(),
#                                                'comments' = notes())
#                      print('df1')
#                      print(df)
#                      df
#
#                    } else if (modify() == "No" &&
#                               nooptions() == "Replace with HYDROLakeDB") {
#                      df <-   data.frame(
#                        'siteId' = r_neositeid(),
#                        'HYDROlake_id' = lk_click()$Hylak_id,
#                        'comments' = notes()
#                      )
#                      print('df2')
#                      print(df)
#                      df
#
#                    } else if (modify() == "No" &&
#                               nooptions() == "Create lake polygon") {
#                      if (!is.null(map_draw_new_feature())) {
#                        polygon_sf()
#                      }
#
#                      df <-  data.frame('siteId' = r_neositeid(),
#                                                 'comments' = notes())
#                      print('df3')
#                      print(df)
#                        sf::st_sf(df, geometry = polygon_sf())
#
#                    }
#                  })
#
#                 # Save user selections as csv
#                  output$submit <- shiny::downloadHandler(
#                    filename = function() {
#                      paste0("lake_",
#                             r_neositeid(),
#                             ".csv")
#                    },
#                    content = function(fname) {
#                      print('data_submit()')
#                      req(data_submit())
#                      print(data_submit())
#                      if (modify() == 'Yes') {
#                        write.csv(data_submit(), fname)
#                      } else if (modify() == 'No' &&
#                                 nooptions() == 'Create lake polygon'){
#                        sf::st_write(data_submit(),
#                                     fname,
#                                     layer_options = "GEOMETRY=AS_WKT")
#                      } else{
#                        write.csv(data_submit(), fname)
#                      }
#                    }
#                  )
#                  return(submit = reactive({input$submit}))
#                })
# }