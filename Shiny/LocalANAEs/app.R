#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# The basin scale app reports dates at the *start* of the interval, because they have been tempaggregated.
# The local scale app reports dates at the *end* of the interval, because they are indexed to the raw inundation data.

library(tidyverse)
library(shiny)
library(sf)
library(stars)

# Shiny sets the wd to the app directory
setwd(here::here())

# Kind of hacky check to only run if needed
# re-create the data
source('directorySet.R')
source(file.path('Scripts', 'plotting', 'metabPlotSetup_Local.R'))
# need the plotting libraries that don't get loaded in the data script
library(tmap)
library(colorspace)

# If I pre-generate the plots, this would be much faster, I think.

# Can we make a scenario comparison version, where the user can choose which
# scenarios to look at?

# choose a sub_poly
ramsarpath <- file.path(datOut, 'WetlandBoundaries', 'ramsarMDB.rdata')
# We know the name, so this is silly, but I really hate reading data in and
# having to remember the name. save/readRDS would fix this.
ramsarMDB <- load_rename(ramsarpath, returnOne = 'ramsarMDB') %>% 
  mutate(WNAME = stringr::str_remove_all(WNAME, pattern = "\""))

catchpath <- file.path(datOut, 'ANAEprocessed', 'ltimNoNorth.rdata')
ltimNoNorth <- load_rename(catchpath, returnOne ='ltimNoNorth')

# werai_one <- filter(ramsarMDB, WNAME == 'Werai Forest') %>%
#   summarise()
# 
# metab_local <- setup_local_metab(sub_poly = werai_one)

# We can pass this around with the uiOutput code that's commented out, but that always starts with an error and then we have to choose a date. so easier to hardcode for now.
availDays <- c('2014-01-01', '2014-03-01', '2014-05-01', '2014-07-01', '2014-09-01', '2014-11-01', '2015-01-01', '2015-03-01', '2015-05-01', '2015-07-01', '2015-09-01', '2015-11-01', '2016-01-01', '2016-03-01', '2016-05-01', '2016-07-01', '2016-09-01', '2016-11-01', '2017-01-01', '2017-03-01', '2017-05-01', '2017-07-01', '2017-09-01', '2017-11-01', '2018-01-01', '2018-03-01', '2018-05-01', '2018-07-01', '2018-09-01', '2018-11-01', '2019-01-01', '2019-03-01', '2019-05-01', '2019-07-01', '2019-09-01', '2019-11-01', '2020-01-01', '2020-03-01', '2020-05-01', '2020-07-01', '2020-09-01')
  
  # st_get_dimension_values(metab_local$temp_anae, which = 'time')

# Can I make a UI with columns?
ui <- fluidPage(
    # Application title
    titlePanel("Individual wetlands"),
    
    fluidRow(
        column(4, 
               selectInput("ramsar_site", "Ramsar site", 
                           choices = sort(unique(ramsarMDB$WNAME)), 
                           selected = "Great Cumbungi Swamp")),
        # column(2,
        #        h4("Two months preceding: ")
        #        ),
        column(4,
               # uiOutput('dates') # This works, but always starts with error until everything loads.
               selectInput("datewanted", "Bimonth end", 
                           choices = availDays,
                           selected = '2019-11-01')
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



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ml_react <- reactive({
    ramsar_one <- filter(ramsarMDB, WNAME == input$ramsar_site) %>% 
      summarise()
    setup_local_metab(sub_poly = ramsar_one, 
                      datOut = here::here(datOut))
  })
  
  # This makes the dates reactive, but always starts with an error
  # output$dates <- renderUI({
  #   metab_local <- ml_react()
  #   selectInput("datewanted", "Two months preceding", 
  #               choices = st_get_dimension_values(metab_local$temp_anae, which = 'time'),
  #               selected = '2014-01-01')
  # })
  

    output$temp <- tmap::renderTmap({
      metab_local <- ml_react()
        # Works. the others might work, if I do something reactive?
        # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
        tempfun(metab_local$temp_anae, 1, input$datewanted, titled = TRUE, titlePrefix = 'Two months preceding')
        # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
        # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
    })
    
    output$inun <- tmap::renderTmap({
      metab_local <- ml_react()
        # Works. the others might work, if I do something reactive?
        # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
        inunfun(metab_local$inun_anae, 1, input$datewanted, titled = FALSE)
        # fastgrab(input$datewanted)
        # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
        # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
    })
    
    output$gpp <- tmap::renderTmap({
      metab_local <- ml_react()
        # Works. the others might work, if I do something reactive?
        # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
        gppfun(metab_local$predGPP, 1, input$datewanted, titled = FALSE)
        # fastgrab(input$datewanted)
        # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
        # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
    })
    
    output$er <- tmap::renderTmap({
      metab_local <- ml_react()
        # Works. the others might work, if I do something reactive?
        # Seee https://stackoverflow.com/questions/62836370/saving-a-tmap-plot-in-shiny
        erfun(metab_local$predER, 1, input$datewanted, titled = FALSE)
        # fastgrab(input$datewanted)
        # tmap_leaflet(inputsfun(input$datewanted), in.shiny = TRUE)
        # tmap_leaflet(tempfun(input$datewanted), in.shiny = TRUE)
    })
    # test
    # output$texttest <- renderText(input$datewanted)
}

# Run the application 
shinyApp(ui = ui, server = server)
