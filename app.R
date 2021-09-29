library(shiny)

ui <- fluidPage(
  titlePanel("Extent Calculator"),
  numericInput("qsmall", label = "Archival Box - Small", value=NA, min=0),
  numericInput('qstd', label = 'Archival Box - Standard', value=NA, min=0),
  numericInput('qurc', label = 'URC Tote Box', value=NA, min=0),
  actionButton('calculate', label='Calculate Total'),
  actionButton('reset', label = 'Reset'),
  textOutput('total')
)

server <- function(input, output, session) {
  
  container_total <- function() {
   t1 <- ifelse(!is.na(input$qsmall), input$qsmall*6.25, 0)
   t2 <- ifelse(!is.na(input$qstd), input$qstd*12.5, 0)
   t3 <- ifelse(!is.na(input$qurc), input$qurc*33.33, 0)
   total <- paste0((t1+t2+t3)," cm")
   return(total)
  }
  
  observeEvent(input$calculate, {
    output$total <- renderText({container_total()})
  })

  observeEvent(input$reset, {
    updateNumericInput(session, 'qsmall', value=NA)
    updateNumericInput(session, 'qstd', value=NA)
    updateNumericInput(session, 'qurc', value=NA)

  })

}

shinyApp(ui, server)