library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  
  h1("Extent Calculator"),
  hr(),
  fluidRow(
    column(6,
           h2("Complete boxes"),
           numericInput("qsmall", label = "Small boxes", value=NA, min=0),
           numericInput('qstd', label = 'Standard boxes', value=NA, min=0),
           numericInput('qurc', label = 'URC tote boxes', value=NA, min=0),
           
           actionButton('calculate', label='Calculate Total', class='btn-primary'),
           actionButton('reset', label = 'Reset'),
           h3(textOutput('total'))
           ),
    column(6,
           h2("Partial boxes"),
           selectInput('part1', label = "Shared box type 1", c("Small", "Standard", "URC")),
           numericInput('totalfiles1', label = "Total files in box", value = NA, min = 0),
           numericInput('partfiles1', label = "Number of files in series", value = NA, min = 0),
           hr(),
           selectInput('part2', label = "Shared box type 2", c("Small", "Standard", "URC")),
           numericInput('totalfiles2', label = "Total files in box", value = NA, min = 0),
           numericInput('partfiles2', label = "Number of files in series", value = NA, min = 0),
           )
    )
)

server <- function(input, output, session) {
  
  container_total <- function() {
   t1 <- ifelse(!is.na(input$qsmall), input$qsmall*6.25, 0)
   t2 <- ifelse(!is.na(input$qstd), input$qstd*12.5, 0)
   t3 <- ifelse(!is.na(input$qurc), input$qurc*33.33, 0)
   if(is.na(input$totalfiles1)|is.na(input$partfiles1)){
     p1 <- 0
   } else
     p1 <- switch(input$part1, 
            Small = 6.25*(input$partfiles1/input$totalfiles1),
            Standard = 12.5*(input$partfiles1/input$totalfiles1),
            URC = 33.33*(input$partfiles1/input$totalfiles1))
   if(is.na(input$totalfiles2)|is.na(input$partfiles2)){
     p2 <- 0
   } else
     p2 <- switch(input$part2, 
                  Small = 6.25*(input$partfiles2/input$totalfiles2),
                  Standard = 12.5*(input$partfiles2/input$totalfiles2),
                  URC = 33.33*(input$partfiles2/input$totalfiles2))
   t <- t1+t2+t3+p1+p2
   total <- ifelse(t<100, paste0(round(t, digits = 2), " cm"), paste0(round(t/100, digits = 2), " m"))
   return(total)
  }
  
  observeEvent(input$calculate, {
    output$total <- renderText({container_total()})
  })

  observeEvent(input$reset, {
    updateNumericInput(session, 'qsmall', value=NA)
    updateNumericInput(session, 'qstd', value=NA)
    updateNumericInput(session, 'qurc', value=NA)
    updateNumericInput(session, 'totalfiles1', value=NA)
    updateNumericInput(session, 'partfiles1', value=NA)
    updateNumericInput(session, 'totalfiles2', value=NA)
    updateNumericInput(session, 'partfiles2', value=NA)

  })

}

shinyApp(ui, server)