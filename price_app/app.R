library(shiny)
library(ggplot2)
library(dplyr)


price_menu <- read.csv("price_app/data/price_menu.csv", stringsAsFactors = FALSE)
price_bioinfo <- read.csv("price_app/data/price_bioinf.csv", stringsAsFactors = FALSE)


print(str(price_menu))


ui <- fluidPage(
  titlePanel("Metabolomics MAPP Prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sampleInput", "Samples", 0, 500, 5, pre = ""),
      radioButtons("typeInput", "User",
        choices = c(
          "UniFr" = "unifr",
          "Academics" = "academics",
          "Private" = "private"
        ),
        selected = "unifr"
      ),
      selectInput("spl_itemInput", "Sample Prep",
        c("None", sort(unique(price_menu$Item[grep("Vial|Prep", price_menu$Item)]))),
        selected = "None"
      ),
      selectInput("da_itemInput", "Data Acquisition",
        c("None", sort(unique(price_menu$Item[grep("da", price_menu$Item)]))),
        selected = "None"
      ),
      selectInput("proc_itemInput", "Processing",
        c("None", sort(unique(price_menu$Item[grep("processing", price_menu$Item)]))),
        selected = "None"
      ),
      checkboxInput("bioInput_biostats", "Biostats", FALSE),
      checkboxInput("bioInput_metannot", "Metabolite Annotation", FALSE),
      #   selectInput("itemInput", "Item",
      #           sort(unique(price_menu$Item)),
      #           selected = "None"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      textOutput("coolplot"),
      tags$head(tags$style("#coolplot{color: black; font-size: 200px;
                                 font-style: bold;
                                 }")),
      br(), br(),
      tableOutput("results"),
       # Button
      downloadButton("downloadData", "Download")
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
    price_menu %>%
      select(Item, input$typeInput) %>%
      filter(Item == input$spl_itemInput | Item == input$da_itemInput | Item == input$proc_itemInput)
  })

  filtered_2 <- reactive({
    if (is.null(input$typeInput)) {
      return(NULL)
    }
    price_bioinfo %>%
      filter(samples == input$sampleInput[1]) %>%
      select("prix_CHF")
  })


  output$coolplot <- renderText({
    if (is.null(filtered())) {
      return()
    }
    paste((sum(filtered()[, 2] * input$sampleInput[1]) + (filtered_2() * (as.numeric(input$bioInput_biostats) + as.numeric(input$bioInput_metannot)))), " CHF")
  })


  output$results <- renderTable({
    filtered() %>%
      #See https://stackoverflow.com/a/53842689 usefull to rename eventough columns dont exist
      rename_with(recode,
        unifr = "UniFr",
        academics = "Academics",
        private = "Private"
      )
  })
    # Reactive value for selected dataset ----
  datasetOutput <- reactive({
    output$results
  })

    # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)