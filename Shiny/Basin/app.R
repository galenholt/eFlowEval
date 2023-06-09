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
if (!('logGPPdays' %in% ls())) {
  source(file.path(here::here(), 'Scripts', 'plotting', 'metabPlotSetup_Basin.R'))
}

# need the plotting libraries that don't get loaded in the data script
library(tmap)
library(colorspace)
availDays <- st_get_dimension_values(logGPPdaysannual, which = 'time')

# Can I make a UI with columns?
ui <- fluidPage(
  # Application title
  titlePanel("Basin-scale drivers and predictions"),
  
  fluidRow(
    # column(4,
    #        h4("Two months following: ")
    # ),
    column(4,
           selectInput("datewanted", "Water year", choices = availDays)
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
    tempfun(temperatureannual, 1, input$datewanted, titled = FALSE)
    # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
    # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
  })
  
  output$inun <- tmap::renderTmap({
    # Works. the others might work, if I do something reactive?
    # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
    inunfun(inundationannual, 1, input$datewanted, titled = FALSE)
    # fastgrab(input$datewanted)
    # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
    # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
  })
  
  output$gpp <- tmap::renderTmap({
    # Works. the others might work, if I do something reactive?
    # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
    gppfun(logGPPdaysannual, 1, input$datewanted, titled = FALSE)
    # fastgrab(input$datewanted)
    # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
    # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
  })
  
  output$er <- tmap::renderTmap({
    # Works. the others might work, if I do something reactive?
    # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
    erfun(logERdaysannual, 1, input$datewanted, titled = FALSE)
    # fastgrab(input$datewanted)
    # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
    # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
  })
  # test
  # output$texttest <- renderText(input$datewanted)
}

# Run the application 
shinyApp(ui = ui, server = server)
