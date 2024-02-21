optionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::prettyRadioButtons(
      inputId = ns("modify"),
      label = "Is this site the correct lake polygon in NeotomaDB?",
      choices = c("Yes", "No"),
      selected = "Yes",
      outline = TRUE,
      plain = TRUE,
      status = "primary",
      icon = shiny::icon("check")
    ),
    shiny::uiOutput(ns('dynamicUI'))
  )


}

optionsServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 # Add radiobuttons if the user selects 'no' as option
                 output$dynamicUI <- shiny::renderUI({
                   if (input$modify == "No") {
                     shinyWidgets::prettyRadioButtons( # or prettyRadioButtons
                       inputId = ns("nooptions"),
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