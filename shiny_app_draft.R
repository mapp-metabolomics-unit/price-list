#endregion
#We fiollow this tutorial https://deanattali.com/blog/building-shiny-apps-tutorial/

install.packages("shiny")
library(shiny)
runExample("01_hello")

library(shiny)
ui <- fluidPage()
server <- function(input, output) {}
shinyApp(ui = ui, server = server)


library(shiny)
runApp(list(
  ui = bootstrapPage(
    numericInput('n', 'Number of obs', 100),
    textOutput('text1'),
    tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                         )
              )
  ),
  server = function(input, output) {
    output$text1 <- renderText({ paste("hello input is",input$n) })
  }
))