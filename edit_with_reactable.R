library(shiny)
library(reactable)
library(dplyr)
library(readr)
library(purrr)

local_file <- "data/mtcars.csv"

source("utilis/funcs.R")

ui <- fluidPage(
  reactableOutput("table")
)

server <- function(input, output, session) {
  
  data <- reactiveVal(
    read_csv(local_file)  
  )
  
  output$table <- renderReactable({
    reactable(data(), onClick = "select", selection = "single")
  })
  
  selected <- reactive(getReactableState("table", "selected"))
  
  
  observeEvent(req(selected()), {
    showModal(modalDialog(
      title = "Edit row",
      data_input(data()[selected(),]),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("delete","Delete"),
        actionButton("add","Add"),
        actionButton("update", "Update")
      )
    ))
  })
  
  observeEvent(input$add, {
    df <- data()
    added <- map(names(df), ~input[[.x]])
    names(added) <- names(df)
    df <- rbind(data(), data.frame(added))
    write_csv(df, local_file)
    data(df)
    removeModal()
    
  })
  
  observeEvent(input$update, {
    df <- data()
    df[selected(),] <- map(names(df), ~input[[.x]]) 
    write_csv(df, local_file)
    data(df)
    removeModal()
  })
  
  observeEvent(input$delete, {
    df <- data()
    df <- df[-selected(), ]
    write_csv(df, local_file)
    data(df)
    removeModal()
  })
  
}

shinyApp(ui, server)
