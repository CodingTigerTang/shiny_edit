
# generate input UIs

generateInput <- function(id, label, value) {
  switch(
    class(value),
    numeric = numericInput(id, label, value),
    integer = numericInput(id, label, value),
    character = if(nchar(value) > 100) {textAreaInput(id, label, value)} else {textInput(id,label, value)},
    factor = selectInput(id, label, choices = levels(value), selected = value[1]),
    logical = checkboxInput(id, label, value),
    Date = dateInput(id, label, value),
    stop("Unsupported data type")
  )
}

# generate input UIs from the data set

data_input <- function(data) {
  map2(names(data), data, ~generateInput(.x, .x, .y[1]))
}