#endregion
#We fiollow this tutorial https://deanattali.com/blog/building-shiny-apps-tutorial/

install.packages("shiny")
library(shiny)
runExample("01_hello")

library(shiny)
ui <- fluidPage()
server <- function(input, output) {}
shinyApp(ui = ui, server = server)