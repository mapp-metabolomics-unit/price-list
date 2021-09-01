library(shiny)
library(ggplot2)
library(dplyr)


price_menu <- read.csv("price_app/data/price_menu.csv", stringsAsFactors = FALSE)
price_bioinfo <- read.csv("price_app/data/price_bioinf.csv", stringsAsFactors = FALSE)


print(str(price_menu))

original_names = c(colnames(price_menu), price_menu$item)
dico =  data.frame(original_names)

dico <- dico  %>% 
distinct(original_names)  %>% 
mutate(new_names = original_names )  %>% 
mutate(new_names = replace(new_names, new_names == "item", "Item"))  %>%
mutate(new_names = replace(new_names, new_names == "unifr", "UniFr"))  %>%
mutate(new_names = replace(new_names, new_names == "academics", "Academics"))  %>%
mutate(new_names = replace(new_names, new_names == "private", "Private"))  %>%
mutate(new_names = replace(new_names, new_names == "vial_conditionning", "Vial Conditionning"))  %>%
mutate(new_names = replace(new_names, new_names == "spl_prep_ms", "Sample Preparation MS"))  %>%
mutate(new_names = replace(new_names, new_names == "spl_prep_gcms_derivatization", "Sample Derivatization GCMS"))  %>%
mutate(new_names = replace(new_names, new_names == "da_gcms_short", "GCMS Short Run"))  %>%
mutate(new_names = replace(new_names, new_names == "da_gcms_long", "GCMS Long Run"))  %>%
mutate(new_names = replace(new_names, new_names == "da_gcfid_short", "GCFID Short Run"))  %>%
mutate(new_names = replace(new_names, new_names == "da_gcfid_long", "GCFID Long Run"))  %>%
mutate(new_names = replace(new_names, new_names == "da_gcmshr_short", "GC-QToF Short Run"))  %>%
mutate(new_names = replace(new_names, new_names == "da_gcmshr_long", "GC-QToF Long Run"))  %>%
mutate(new_names = replace(new_names, new_names == "da_lcms_short", "LCMS Short Run"))  %>%
mutate(new_names = replace(new_names, new_names == "da_lcms_long", "LCMS Long Run"))  %>%
mutate(new_names = replace(new_names, new_names == "basic_processing", "Basic Processing"))  %>% 
mutate(correspondances = paste('"', dico$original_names,'"', ' = ', '"',dico$new_names,'"', sep = ''))  %>% 
mutate(sections = c('item', rep('user_type',3), rep('sample_prep',3), rep('methods', 8), rep('processing', 1) ))



paste(dico$original_names,dico$new_names, sep = '=')

my_named_list <- as.list(dico$original_names)
names(my_named_list) <- c(dico$new_names)

grep('user_type', dico$sections)



ui <- fluidPage(
  titlePanel("Metabolomics MAPP Prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sampleInput", "Samples", 0, 500, 5, pre = ""),
      radioButtons("typeInput", "User",
        choiceNames = as.list(
        dico$new_names[grep('user_type', dico$sections)]),
        choiceValues = as.list(
        dico$original_names[grep('user_type', dico$sections)]),
        selected = "unifr"
      ),
        selectInput("spl_itemInput", "Sample Prep",
                choices = c("None", my_named_list[grep('sample_prep', dico$sections)]),
        selected = "None"
      ),
      selectInput("da_itemInput", "Data Acquisition",
        choices = c("None", my_named_list[grep('methods', dico$sections)]),
        selected = "None"
      ),
      selectInput("proc_itemInput", "Processing",
        choices = c("None", my_named_list[grep('processing', dico$sections)]),
        selected = "None"
      ),
      checkboxInput("bioInput_biostats", "Biostats", FALSE),
      checkboxInput("bioInput_metannot", "Metabolite Annotation", FALSE),
      #   selectInput("itemInput", "item",
      #           sort(unique(price_menu$item)),
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
      select(item, input$typeInput) %>%
      filter(item == input$spl_itemInput | item == input$da_itemInput | item == input$proc_itemInput)
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
    left_join(y=dico, by=c('item' ='original_names')) %>% 
    mutate(item = new_names) %>%
    select(-new_names, -correspondances, -sections) %>% 
      rename_with(recode,
        item = "Item", 
        unifr = "UniFr",
        academics = "Academics",
        private = "Private",
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