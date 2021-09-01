library(shiny)

optionsGroupUI <- function(id, label, choices, selected = NULL, ...) {
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(
      ns("choices"),
      label,
      choices,
      selected,
      ...
    ),
    div(class = "btn-group", role = "group",
      actionButton(ns("select_all"), "Select all", class = "btn-xs"),
      actionButton(ns("deselect_all"), "Deselect all", class = "btn-xs")
    )
  )
}

# This is a module server function; you never call it directly, but instead
# use callModule.
#
# The `choices` parameter should be the same choices passed to the
# corresponding `optionsGroupUI` call.
#
# The return value is a reactive expression for the user's selection.
optionsGroup <- function(input, output, session, choices) {
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "choices", selected = choices)
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "choices", selected = character(0))
  })

  reactive(input$choices)
}

ui <- fluidPage(
  fluidRow(
    column(6,
      tabsetPanel(
        tabPanel("Iris",
          optionsGroupUI("one", "Iris variables", names(iris))
        ),
        tabPanel("airquality",
          optionsGroupUI("two", "airquality variables", names(airquality))
        )
      )
    ),
    column(6,
      h3("Selected:"),
      verbatimTextOutput("result", placeholder = TRUE)
    )
  )
)

server <- function(input, output, session) {
  # These lines provide the server-side initialization of the modules,
  # and the `one` and `two` variables are reactive expressions that
  # yield the selected option(s).
  one <- callModule(optionsGroup, "one", choices = names(iris))
  two <- callModule(optionsGroup, "two", choices = names(airquality))
  
  output$result <- renderPrint({
    selected <- c(one(), two())
    cat(paste0(selected, collapse = "\n"))
  })
}

shinyApp(ui, server)