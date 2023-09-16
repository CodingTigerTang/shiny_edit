library(shiny)
library(DT)
library(purrr)

local_file <- "data/mtcars.csv"

source("utilis/funcs.R")

ui <- fluidPage(
  dataTableOutput("mytable")
)


server <- function(input, output, session) {
  
  data <- reactiveVal(
    read_csv(local_file)  
  )
  
  output$mytable <- renderDT({
    datatable(data(),selection = "single")
  }, server = FALSE)
  
  observeEvent(req(input$mytable_rows_selected), {
    showModal(modalDialog(
      title = "Edit row",
      data_input(data()[input$mytable_rows_selected,]),
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
    df[input$mytable_rows_selected,] <- map(names(df), ~input[[.x]]) 
    write_csv(df, local_file)
    data(df)
    removeModal()
  })
  
  observeEvent(input$delete, {
    df <- data()
    df <- df[-input$mytable_rows_selected, ]
    write_csv(df, local_file)
    data(df)
    removeModal()
  }) 
  
} 

shinyApp(ui = ui, server = server)