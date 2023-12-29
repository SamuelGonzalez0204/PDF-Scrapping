#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel(img(src = "LogoSACYL.png", align="left", height = 40, width = 300)),

  br(),
  h1(
    "PDF-Scrapping:", span("Shiny app", style = "font-weight: 300"), 
     style = "font-family: 'Source Sans Pro';
         text-align: center;
        background-image: url('texturebg.png');
        padding: 20px"),
  br(),

    fluidRow(
      
      column(3,
             
      )
    ),
    
    fluidRow(
      
      column(3,
             fileInput("file", h3("File input"))),
    ),
    
    fluidRow(
      
      column(3,
             sidebarPanel(
             h3("Buttons"),
             actionButton("action", "Action"),
             br(),
             br(), 
             submitButton("Analizar"))
             ),

        column(3,
              selectInput("var", 
                          label = "Choose a variable to display",
                          choices = list("Percent White", 
                                         "Percent Black",
                                         "Percent Hispanic", 
                                         "Percent Asian"),
                          selected = "Percent White"),
        )
      )
    )
        

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
