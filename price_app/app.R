
library(shiny)
library(ggplot2)
library(dplyr)

price_menu <- read.csv("/Users/pma/Dropbox/git_repos/mapp-metabolomics-unit/price-list/data/price_menu.csv", stringsAsFactors = FALSE)
price_bioinfo <- read.csv("/Users/pma/Dropbox/git_repos/mapp-metabolomics-unit/price-list/data/price_bioinf.csv", stringsAsFactors = FALSE)

print(str(bcl))


ui <- fluidPage(
  titlePanel("Metabolomics MAPP Prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sampleInput", "Samples", 0, 500, 5, pre = ""),
      radioButtons("typeInput", "User",
                  choices = c("unifr", "academics", "commercials"),
                  selected = "unifr"),
      selectInput("spl_itemInput", "Sample Prep",
              c('None', sort(unique(price_menu$item[grep('spl_prep', price_menu$item)]))) ,
              selected = "None"),
      selectInput("da_itemInput", "Data Acquisition",
              c('None', sort(unique(price_menu$item[grep('da', price_menu$item)]))) ,
              selected = "None"),
      selectInput("proc_itemInput", "Processing",
              c('None', sort(unique(price_menu$item[grep('processing', price_menu$item)]))) ,
              selected = "None"),
    #   selectInput("itemInput", "Item",
    #           sort(unique(price_menu$item)),
    #           selected = "None"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      textOutput("coolplot"),
      tags$head(tags$style("#coolplot{color: black;
                                 font-size: 200px;
                                 font-style: bold;
                                 }")
      ),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
#   output$countryOutput <- renderUI({
#     selectInput("countryInput", "Country",
#                 sort(unique(bcl$Country)),
#                 selected = "CANADA")
#   })  
  
  filtered <- reactive({
    if (is.null(input$typeInput)) {
      return(NULL)
    }    
    
#     price_menu %>%
#       filter(Price >= input$priceInput[1],
#              Price <= input$priceInput[2],
#              Type == input$typeInput,
#              Country == input$countryInput
#       )
#   })
      price_menu %>%
      select(item, input$typeInput)  %>% 
      filter(item == input$spl_itemInput | item == input$da_itemInput | item == input$proc_itemInput )
  })
  
  output$coolplot <- renderText({
    if (is.null(filtered())) {
      return()
    }
    paste(sum(filtered()[,2] * input$sampleInput[1]), ' CHF')
  })

  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)