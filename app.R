library(shiny)
library(bslib)
library(leaflet)
library(sf)

ui <-  page_navbar(
  title = "Neotoma Lakes",
  theme = bs_theme(bootswatch = "minty",
                   base_font = font_google("Inter"),
                   navbar_bg = "#25443B"),
  layout_columns(
    col_widths = c(8, 4, 8, 4),
    row_heights = c(2, 1),
    card(
      full_screen = TRUE,
      class = 'p-0',
 leafletOutput('map')
    ),
    card(
      full_screen = TRUE,
      card_header("Lake"),
      plot(1,2)
    ),
    card(
      full_screen = TRUE,

      card_header("Share your comments and submit!"),
      plot(1,2),
      textInput("comments", "Please, share your comments:"),
      shiny::submitButton('submit')
    ),
    card(
      full_screen = TRUE,
      card_header("Country"),
      plot(1,2)
    )
  )

)

server <- function(input, output, session) {


  output$map <- renderLeaflet({

    leaflet() |>
      addTiles()

  })

}

shinyApp(ui, server)