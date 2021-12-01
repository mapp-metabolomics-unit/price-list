library(shiny)
library(ggplot2)
library(dplyr)
library(janitor)

# when running from R visual studio terminal
price_menu <- read.csv("price_app/data/price_menu.csv", stringsAsFactors = FALSE)
price_bioinfo <- read.csv("price_app/data/price_bioinf_allactors.csv", stringsAsFactors = FALSE)
price_bioinfo_basics <- read.csv("price_app/data/price_bioinf_basics_allactors.csv", stringsAsFactors = FALSE)

# when deploying to shinyapps.io
# price_menu <- read.csv("data/price_menu.csv", stringsAsFactors = FALSE)
# price_bioinfo <- read.csv("data/price_bioinf_allactors.csv", stringsAsFactors = FALSE)
# price_bioinfo_basics <- read.csv("data/price_bioinf_basics_allactors.csv", stringsAsFactors = FALSE)

# input$typeInput <- 'unifr'
# input$spl_itemInput <- 'vial_conditionning'
# input$da_itemInput <- 'da_lcms_short'

# price_menu %>%
#       select(item, unifr) %>%
#       filter(item == 'vial_conditionning' | item == 'da_lcms_short')  %>% 
#       mutate(unifr = unifr * 2)


print(str(price_menu))

original_names <- c(colnames(price_menu), price_menu$item)
dico <- data.frame(original_names)

dico <- dico %>%
  distinct(original_names) %>%
  mutate(new_names = original_names) %>%
  mutate(new_names = replace(new_names, new_names == "item", "Item")) %>%
  mutate(new_names = replace(new_names, new_names == "unifr", "UniFr")) %>%
  mutate(new_names = replace(new_names, new_names == "academics", "Academics")) %>%
  mutate(new_names = replace(new_names, new_names == "private", "Private")) %>%
  mutate(new_names = replace(new_names, new_names == "vial_conditionning", "Vial Conditionning")) %>%
  mutate(new_names = replace(new_names, new_names == "spl_prep_ms", "Sample Preparation MS")) %>%
  mutate(new_names = replace(new_names, new_names == "spl_prep_gcms_derivatization", "Sample Derivatization GCMS")) %>%
  mutate(new_names = replace(new_names, new_names == "da_gcms_short", "GCMS Short Run")) %>%
  mutate(new_names = replace(new_names, new_names == "da_gcms_long", "GCMS Long Run")) %>%
  mutate(new_names = replace(new_names, new_names == "da_gcfid_short", "GCFID Short Run")) %>%
  mutate(new_names = replace(new_names, new_names == "da_gcfid_long", "GCFID Long Run")) %>%
  mutate(new_names = replace(new_names, new_names == "da_gcmshr_short", "GC-QToF Short Run")) %>%
  mutate(new_names = replace(new_names, new_names == "da_gcmshr_long", "GC-QToF Long Run")) %>%
  mutate(new_names = replace(new_names, new_names == "da_lcms_short", "LCMS Short Run")) %>%
  mutate(new_names = replace(new_names, new_names == "da_lcms_long", "LCMS Long Run")) %>%
  # mutate(new_names = replace(new_names, new_names == "basic_processing", "Basic Processing"))  %>%
  mutate(correspondances = paste('"', dico$original_names, '"', " = ", '"', dico$new_names, '"', sep = "")) %>%
  mutate(sections = c("item", rep("user_type", 3), rep("sample_prep", 3), rep("methods", 8)))



paste(dico$original_names, dico$new_names, sep = "=")

my_named_list <- as.list(dico$original_names)
names(my_named_list) <- c(dico$new_names)

grep("user_type", dico$sections)



ui <- fluidPage(
  # titlePanel("Metabolomics MAPP Prices - 2021"),
  h1(id = "big-heading", "Metabolomics MAPP Prices - 2021"),
  tags$style(HTML("#big-heading{color: black; font-size: 50px;
                                 font-style: bold;}")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("typeInput", "User",
        choiceNames = as.list(
          dico$new_names[grep("user_type", dico$sections)]
        ),
        choiceValues = as.list(
          dico$original_names[grep("user_type", dico$sections)]
        ),
        selected = "unifr"
      ),
      sliderInput("sampleInput", "Samples", 0, 500, 5, pre = ""),
      selectInput("spl_itemInput", "Sample Prep",
        choices = c("None", my_named_list[grep("sample_prep", dico$sections)]),
        selected = "None"
      ),
      selectInput("da_itemInput", "Data Acquisition",
        choices = c("None", my_named_list[grep("methods", dico$sections)]),
        selected = "None"
      ),
      # conditionalPanel(
      #   condition = "input.da_itemInput != 'None'",
      #   checkboxInput("daInput_pos", "Positive mode", FALSE),
      #   checkboxInput("daInput_neg", "Negative mode", FALSE)
      # ),
      conditionalPanel(
        condition = "input.da_itemInput == 'da_lcms_long' | input.da_itemInput == 'da_lcms_short'",
              checkboxGroupInput(inputId = "polarity", label = "",
                   choices = c("Positive" = 'pos',
                               "Negative" = 'neg'),
                   inline = TRUE,
                   selected = 'pos')
      ),
      conditionalPanel(
        condition = "input.da_itemInput == 'da_lcms_long' | input.da_itemInput == 'da_lcms_short'",
              checkboxGroupInput(inputId = "phase", label = "",
                   choices = c("C18" = 'c18',
                               "HILIC" = 'hilic'),
                   inline = TRUE,
                   selected = 'c18')
      ),
      selectInput("proc_itemInput", "Processing",
        choices = c("None", my_named_list[grep("processing", dico$sections)], "Basic Processing", "Advanced Processing"),
        selected = "None"
      ),
      conditionalPanel(
        condition = "input.proc_itemInput == 'Basic Processing'",
        checkboxInput("bioInput_basics", "Peak Table", FALSE)
      ),
      conditionalPanel(
        condition = "input.proc_itemInput == 'Advanced Processing'",
        checkboxInput("bioInput_biostats", "Biostats", FALSE),
        checkboxInput("bioInput_metannot", "Metabolite Annotation", FALSE)
      )
      #   selectInput("itemInput", "item",
      #           sort(unique(price_menu$item)),
      #           selected = "None"),
      # uiOutput("countryOutput")
      #     h2(id="big-heading", "Metabolomics MAPP Prices - 2021"),
      # tags$style(HTML("#big-heading{color: black; font-size: 50px;
      #                                font-style: bold;}"))
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

  # filtered <- reactive({
  #   if (is.null(input$typeInput)) {
  #     return(NULL)
  #   }
  #   price_menu %>%
  #     select(item, input$typeInput) %>%
  #     filter(item == input$spl_itemInput | item == input$da_itemInput | item == input$proc_itemInput)
  # })

  filtered <- reactive({
    if (is.null(input$typeInput)) {
      return(NULL)
    } else if (length(input$polarity) * length(input$phase) == 1 ){
    price_menu %>%
      select(item, input$typeInput) %>%
      filter(item == input$spl_itemInput | item == input$da_itemInput | item == input$proc_itemInput)
    } else if (length(input$polarity) * length(input$phase) == 2 ){
    price_menu %>%
      select(item, input$typeInput) %>%
      filter(item == input$spl_itemInput | item == input$da_itemInput | item == input$proc_itemInput)  %>% 
      # see https://stackoverflow.com/a/62864238
      mutate(!!as.symbol(input$typeInput) := sum(rep(.[[2]],2)))
    } else if (length(input$polarity) * length(input$phase) == 4 ){
    price_menu %>%
      select(item, input$typeInput) %>%
      filter(item == input$spl_itemInput | item == input$da_itemInput | item == input$proc_itemInput)  %>% 
      # see https://stackoverflow.com/a/62864238
      mutate(!!as.symbol(input$typeInput) := sum(rep(.[[2]],4)))
    }
  })

  filtered_2 <- reactive({
    if (is.null(input$typeInput)) {
      return(NULL)
    }
    price_bioinfo %>%
      filter(samples == input$sampleInput[1]) %>%
      select("price_chf_unifr")
  })


  filtered_basic <- reactive({
    req(input$typeInput)
    if (input$typeInput == "unifr" & input$bioInput_basics == TRUE) {
      price_bioinfo_basics %>%
        filter(samples == input$sampleInput[1]) %>%
        mutate(item = "Basic Processing") %>%
        select(item, "price_chf_unifr") %>%
        rename("total_price" = "price_chf_unifr")
    } else if (input$typeInput == "academics" & input$bioInput_basics == TRUE) {
      price_bioinfo_basics %>%
        filter(samples == input$sampleInput[1]) %>%
        mutate(item = "Basic Processing") %>%
        select(item, "price_chf_academics") %>%
        rename("total_price" = "price_chf_academics")
    } else if (input$typeInput == "private" & input$bioInput_basics == TRUE) {
      price_bioinfo_basics %>%
        filter(samples == input$sampleInput[1]) %>%
        mutate(item = "Basic Processing") %>%
        select(item, "price_chf_private") %>%
        rename("total_price" = "price_chf_private")
    }
  })

  filtered_biostats <- reactive({
    req(input$typeInput)
    if (input$typeInput == "unifr" & input$bioInput_biostats == TRUE) {
      price_bioinfo %>%
        filter(samples == input$sampleInput[1]) %>%
        mutate(item = "Biostats") %>%
        select(item, "price_chf_unifr") %>%
        rename("total_price" = "price_chf_unifr")
    } else if (input$typeInput == "academics" & input$bioInput_biostats == TRUE) {
      price_bioinfo %>%
        filter(samples == input$sampleInput[1]) %>%
        mutate(item = "Biostats") %>%
        select(item, "price_chf_academics") %>%
        rename("total_price" = "price_chf_academics")
    } else if (input$typeInput == "private" & input$bioInput_biostats == TRUE) {
      price_bioinfo %>%
        filter(samples == input$sampleInput[1]) %>%
        mutate(item = "Biostats") %>%
        select(item, "price_chf_private") %>%
        rename("total_price" = "price_chf_private")
    }
  })

  filtered_metannot <- reactive({
    req(input$typeInput)
    if (input$typeInput == "unifr" & input$bioInput_metannot == TRUE) {
      price_bioinfo %>%
        filter(samples == input$sampleInput[1]) %>%
        mutate(item = "Metabolite Annotation") %>%
        select(item, "price_chf_unifr") %>%
        rename("total_price" = "price_chf_unifr")
    } else if (input$typeInput == "academics" & input$bioInput_metannot == TRUE) {
      price_bioinfo %>%
        filter(samples == input$sampleInput[1]) %>%
        mutate(item = "Metabolite Annotation") %>%
        select(item, "price_chf_academics") %>%
        rename("total_price" = "price_chf_academics")
    } else if (input$typeInput == "private" & input$bioInput_metannot == TRUE) {
      price_bioinfo %>%
        filter(samples == input$sampleInput[1]) %>%
        mutate(item = "Metabolite Annotation") %>%
        select(item, "price_chf_private") %>%
        rename("total_price" = "price_chf_private")
    }
  })

  observe(print(input$typeInput))
  observe(print(input$bioInput_metannot))


  final_table <- reactive({
    filtered() %>%
      left_join(y = dico, by = c("item" = "original_names")) %>%
      mutate(item = new_names) %>%
      select(-new_names, -correspondances, -sections) %>%
      mutate("total_price" = .[[2]] * input$sampleInput[1]) %>%
      # # add_row(item = 'bioingo', unifr = 0)  %>%
      bind_rows(filtered_biostats()) %>%
      bind_rows(filtered_metannot()) %>%
      bind_rows(filtered_basic()) %>%
      adorn_totals("row") %>%
      rename_with(recode,
        item = "Item",
        unifr = "Price Per Unit (UniFr)",
        academics = "Price Per Unit (Academics)",
        private = "Price Per Unit (Private)",
        total_price = "Total Price",
      )
  })

  output$results <- renderTable({
    final_table()
  })


  output$coolplot <- renderText({
    if (final_table()[nrow(final_table()), ncol(final_table())] == 0) {
      return('_')
    }
    paste(final_table()[nrow(final_table()), ncol(final_table())], " CHF")
    # paste((sum(filtered()[, 2] * input$sampleInput[1]) + (filtered_basic()[, 2] * as.numeric(input$bioInput_basics)) + (filtered_metannot()[, 2] * (as.numeric(input$bioInput_biostats) + as.numeric(input$bioInput_metannot)))), " CHF")
  })

  observe(print(input$bioInput_metannot))
  observe(print(length(input$polarity)))
  observe(print(input$typeInput))

  # Reactive value for selected dataset ----
  datasetOutput <- reactive({
    output$results
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('preliminary_invoice_',input$sampleInput[1],'_samples_',input$typeInput,'_2021', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(final_table(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)