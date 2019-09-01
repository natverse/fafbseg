# library(rclipboard)
library(shiny)
library(fafbseg)

is.url <- function(x) {
  res <- try(httr::parse_url(x), silent = TRUE)
  if(inherits(res, 'try-error')) return(FALSE)
  isTRUE(grepl("http", res$scheme))
}

is.location <- function(x) {
  if(is.null(x)) return(FALSE)
  grepl("[0-9.]+,[0-9.]+,[0-9.]+", x)
}

convert_url <- function(x) {
  if(!is.url(x)) return(NA)
  res=try(catmaid2ngl(x), silent = TRUE)
  if(!inherits(res, 'try-error')) return(res)
  NA
}

# The UI
ui <- bootstrapPage(

  titlePanel("CATMAID - Neuroglancer URL converter"),

  # rclipboardSetup(),

  # A text input for testing the clipboard content.
  textInput("inputurl", "Paste input url here:"),
  # Add a text input

  uiOutput("outputurlhtml"),
  textOutput("outputurl")
  # textInput("outputurl2", "Output:"),
  # uiOutput("clip")
)

# The server
server <- function(input, output) {

  # Add clipboard buttons
  # output$clip <- renderUI({
  #   rclipButton("clipbtn", "rclipButton Copy", output$outputurl, icon("clipboard"))
  # })

  output$outputurl <- renderText({
    u=convert_url(input$inputurl)
    if(is.na(u)) "<invalid url>" else u
  })

  output$outputurlhtml <- renderUI({
    u=convert_url(input$inputurl)
    if(is.na(u)) HTML("Invalid URL!")
    else HTML(sprintf('<a href="%s">Jump to Location</a>', u))
  })

}

shinyApp(ui = ui, server = server)
