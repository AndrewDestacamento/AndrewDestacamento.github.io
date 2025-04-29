library(shiny)
library(bslib)

ui = page_sidebar(
  title = "Stats Calculator",
  sidebar = sidebar(
    helpText("Various options"),
    
  )
)

server = function(input, output) {
}

shinyApp(ui, server)
