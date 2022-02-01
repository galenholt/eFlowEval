#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
setwd(here::here())

# Kind of hacky check to only run if needed
if (!('weraiCropInun' %in% ls())) {
    source(file.path(here::here(), 'Scripts', 'Scenarios', 'plotting', 'metabolismLocalAndStatic.R'))
}

# need the plotting libraries that don't get loaded in the data script
library(tmap)
library(colorspace)
availDays <- st_get_dimension_values(weraiCropTemp, which = 'time')

# Can I make a UI with columns?
ui <- fluidPage(
    # Application title
    titlePanel("Local Example: Werai Forest"),
    
    fluidRow(
        column(4,
               h4("Two months preceding: ")
               ),
        column(4,
               selectInput("datewanted", "Bimonth end", choices = availDays)
               )
    ),
    
    fluidRow(column(12, h3("Drivers"))),
    
    fluidRow(
        column(6,
               tmap::tmapOutput("temp")
               ),
        column(6,
               tmap::tmapOutput("inun")
               )
    ),
    
    fluidRow(column(12, h3("Predictions"))),
    
    fluidRow(
        column(6,
               tmap::tmapOutput("gpp")
        ),
        column(6,
               tmap::tmapOutput("er")
        )
    )
)

# # Define UI
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Local Example: Werai Forest"),
#     
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             selectInput("datewanted", "Bimonth end", choices = availDays)
#         ),
#         
#         # Show a plot of the generated distribution
#         mainPanel(
#             # tmap::tmapOutput("invars"),
#             tmap::tmapOutput("temp"),
#             tmap::tmapOutput("inun"),
#             tmap::tmapOutput("gpp"),
#             tmap::tmapOutput("er"),
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$temp <- tmap::renderTmap({
        # Works. the others might work, if I do something reactive?
        # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
        tempfun(weraiCropTemp, 1, input$datewanted, titled = TRUE)
        # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
        # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
    })
    
    output$inun <- tmap::renderTmap({
        # Works. the others might work, if I do something reactive?
        # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
        inunfun(weraiCropTemp, 1, input$datewanted, titled = FALSE)
        # fastgrab(input$datewanted)
        # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
        # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
    })
    
    output$gpp <- tmap::renderTmap({
        # Works. the others might work, if I do something reactive?
        # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
        gppfun(weraiCropPredGPP, 1, input$datewanted, titled = FALSE)
        # fastgrab(input$datewanted)
        # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
        # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
    })
    
    output$er <- tmap::renderTmap({
        # Works. the others might work, if I do something reactive?
        # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
        erfun(weraiCropPredER, 1, input$datewanted, titled = FALSE)
        # fastgrab(input$datewanted)
        # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
        # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
    })
    # test
    # output$texttest <- renderText(input$datewanted)
}

# Run the application 
shinyApp(ui = ui, server = server)
