optionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::prettyRadioButtons(
      inputId = ns("modify"),
      label = NULL,
      choices = c("No action",
                  "Accept Current Location",
                  "Update Current Location"),
      selected = "No action",
     # outline = NULL,
     # plain = TRUE,
      status = "primary",
      icon = shiny::icon("check")
    ),
    shiny::uiOutput(ns('dynamicUI'))
  )


}

optionsServer <- function(id,
                          r_neositeid) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 observe({
                   # Message
                   shiny::validate(shiny::need(
                     !is.null(r_neositeid()),
                     "Please, select a siteid before moving foward"
                   ))

                 })

                 # Add radiobuttons if the user selects
                 # 'Update Current Location' as option
                 output$dynamicUI <- shiny::renderUI({


                   if (input$modify == "Update Current Location") {
                     shinyWidgets::prettyRadioButtons(
                       inputId = ns("nooptions"),
                       label = "How would you update NeotomaDB?",
                       choices = c("Replace with HYDROLakeDB" ,
                                   "Create lake polygon"),
                      # outline = TRUE,
                     #  plain = TRUE,
                       status = "primary",
                       icon = shiny::icon("check"))
                   } else {
                     # If the option is not "Option 1", render an empty div
                     shiny::div()
                   }
                 })
                 return(list(modify = reactive({
                   input$modify
                 }),
                 nooptions = reactive({
                   if (!is.null(input$nooptions))
                   {
                     input$nooptions
                   } else{
                     NULL
                   }
                 })
                 ))
               })
}