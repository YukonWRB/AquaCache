server <- function(input, output, session) {
  output$textOutput <- renderText(input$textInput)
  }
