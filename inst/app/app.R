# library(rclipboard)
library(shiny)
library(fafbseg)
library(memoise)

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
  res=try(elmr::open_fafb((ngl_decode_scene(x))), silent = TRUE)
  if(!inherits(res, 'try-error')) return(res)
  NA
}
m_convert_url <- memoise(convert_url)

# The UI
ui <- fluidPage(

  titlePanel("CATMAID - Neuroglancer URL converter"),

  # rclipboardSetup(),

  sidebarLayout(
    sidebarPanel(
      h2("Run at home"),
      p(
        "This application is distributed with the fafbseg package. See ",
        a("natverse.github.io/fafbseg",
          href = "https://natverse.github.io/fafbseg"),
        "for details."
      )
    ),
    mainPanel(
      h1("URL input"),
      p("CATMAID and Neuroglancer both use URLs to encode locations and some ",
        "scene elements. You can convert between these two URL formats to ",
        "take you to the same location in each web application."
        ),
      h2("Instructions"),
      div("Simply paste either form of url into the text box. In response you ",
          "will get a hyperlink that you can click and a text element with the ",
          "full converted URL", style = "color:magenta"),
      h2("Input"),
      textInput("inputurl", "Paste input url here:"),
      h2("Output"),
      uiOutput("outputurlhtml"),
      p(""),
      textOutput("outputurl")
    )
  )
)

# The server
server <- function(input, output) {

  # Add clipboard buttons
  # output$clip <- renderUI({
  #   rclipButton("clipbtn", "rclipButton Copy", output$outputurl, icon("clipboard"))
  # })

  output$outputurl <- renderText({
    u=m_convert_url(input$inputurl)
    if(is.na(u)) "<invalid url>" else u
  })

  output$outputurlhtml <- renderUI({
    u=m_convert_url(input$inputurl)
    if(is.na(u)) HTML("Invalid URL!")
    else HTML(sprintf('<a href="%s">Jump to Location</a>', u))
  })

}

shinyApp(ui = ui, server = server)
