# Load Packages
library(AER)
library(broom)
library(car)
library(ggrepel)
library(gridExtra)
library(htmltools)
library(htmlwidgets)
library(lubridate)
library(modelsummary)
library(patchwork)
#library(plotly)
library(qualtRics)
library(readr)
library(reactable)
library(sandwich)
library(scales)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
#library(shinythemes)
library(shinyWidgets)
library(stargazer)
library(tidyverse)
library(vtable)


## Set up Space
remove(list = ls())
appfolder <- "G:/My Drive/Research-Synced/Projects/Maine Farmers Market Prices/FM Price App"
datafolder <- "G:/My Drive/Research-Synced/Projects/Maine Farmers Market Prices/data"
scriptsfolder <- "G:/My Drive/Research-Synced/Projects/Maine Farmers Market Prices/scripts"
tablesfigures <- "G:/My Drive/Research-Synced/Projects/Maine Farmers Market Prices/tablesfigures/disability status"

## Load Data
setwd(datafolder)
load("FMuse.RData")

## Return to App Folder
setwd(appfolder)


## User Interface Section (layout and appearance)
ui <- fluidPage( 

    navbarPage("Maine Farmers Market Price Report",
             # Tab Panel 1 - Week-by-Week Prices
                tabPanel("Weekly Price Report",
                    sidebarLayout( 
                        sidebarPanel(position = "left",
                                     fluidRow(class = "btn-sm",
                                              tags$style(HTML(".checkbox {margin: -10px;}")),
                                              radioButtons(inputId = "marketselect", label = "Region", 
                                                           choices = c("All" = "All",
                                                                       "Central" = "Central",
                                                                       "Mid Coast" = "Mid Coast",
                                                                       "Southern" = "Southern"),
                                                           selected = "All")
                                                             ),
                                     
                                     fluidRow(class = "btn-sm",
                                              selectInput(inputId = "week", label = "Week Starting On:", 
                                                           choices = weeklist$weekdate,
                                                           selected = max(weeklist$weekdate))
                                     ),                                     
                                     
                                     fluidRow(class = "btn-sm",
                                             tags$style(HTML(".checkbox {margin: -10px;}")),
                                             radioButtons(inputId = "organic", label = "Prices", 
                                                          choices = c("All Prices" = "0",
                                                                      "Organic Only" = "1"),
                                                          selected = "0")
                                     ),
                                     
                                     downloadButton("marketreport", "Generate Market Report")
                                     
                        ),
                        mainPanel(
                        plotOutput("marketplot"),  
                          
                        tableOutput("reporttable")
                        
                        )
                        )),
             
             
             # Tab Panel 2 - Product Prices Over Time
             
             tabPanel("Product Prices Over Time",
                      sidebarLayout( 
                          sidebarPanel(position = "left",
                                       fluidRow(class = "btn-sm",
                                                tags$style(HTML(".checkbox {margin: -10px;}")),
                                                radioButtons(inputId = "marketselect2", label = "Region", 
                                                             choices = c("All" = "All",
                                                                         "Central" = "Central",
                                                                         "Mid Coast" = "Mid Coast",
                                                                         "Southern" = "Southern"),
                                                             selected = "All")
                                       ),
                                       
                                       fluidRow(class = "btn-sm",
                                                sliderInput("daterange", label = h3("Date Range"), 
                                                            min = min(weeklist$weekdate), 
                                                            max = max(weeklist$weekdate), 
                                                            value = c(min(weeklist$weekdate), max(weeklist$weekdate)),
                                                            timeFormat = "%F")
                                       ),                                     
                                       
                                       fluidRow(class = "btn-sm",
                                                tags$style(HTML(".checkbox {margin: -10px;}")),
                                                radioButtons(inputId = "organic2", label = "Prices", 
                                                             choices = c("All Prices" = 0,
                                                                         "Organic Only" = 1),
                                                             selected = 0)
                                       ),
                                       
                                       
                                       
                                       fluidRow(class = "btn-sm",
                                                tags$style(HTML(".checkbox {margin: 10px;}")),
                                                checkboxGroupInput(inputId = "products", label = "Product", 
                                                                   choices = productlist,
                                                                   selected = "Eggs (doz)")
                                       ),
                                       downloadButton("productreport", "Generate Products Report")
                                       
                          ),
                          mainPanel( 
                            
                            plotOutput("producttimeplot"),  
                            
                            tableOutput("producttimetable")

                            )
                      ))
    ),
    
    br(),
    
    h3(strong("Acknowledgements")),
    
    h4("This report is made possible with funding through the Maine Department of Agriculture, Conservation, and Forestry and the support of the Maine Federation of Farmers Markets, Maine Organic Farmers and Gardeners Association, Maine Farmland Trust, and the University of Maine."),
    
    HTML('<center><img src="banner.png" width= "100%" ></center>')
    
    )



### Server Function (Instructions to computer)
server <- function(input, output) {
 
  ## First Page - Map
  
  # Set up data
  marketplotdata <- reactive({
    set <- markets %>% filter(marketregion == input$marketselect | mr2 == input$marketselect)
    return(set)
    })
    
  # Make Map
  output$marketplot <- renderPlot({
    ggplot(mainemap, aes(x = long, y = lat)) +
     geom_polygon(aes(group = group),fill = "white",colour="black") + 
      geom_point(data = marketplotdata(), aes(x = lon, y = lat), size=4, color = marketplotdata()$regioncol) + coord_fixed(1.3) + empty +
      geom_label_repel(data = marketplotdata(), aes(x = lon, y = lat,label = marketlab), box.padding   = 0.35, point.padding = 0.5, segment.color = 'grey50')
})

  
  ## First Page Table
  
  # Set up data
  reporttabledata <- reactive({
    rtdata <- data.use %>% filter(marketregion == input$marketselect | mr2 == input$marketselect) %>% 
                          filter(weekdate == input$week) %>%   
                          filter(organic >= as.numeric(input$organic)) 
    return(rtdata)
  })
  
  # Make Table
  output$reporttable <- function()({
    reporttabledata() %>% 
    group_by(Product) %>% 
    summarize(`Average Price` = round(mean(price),2),
              `Minimum` = round(min(price),2),
              `Maximum` = round(max(price),2),
              `Number Reporting` = n()) %>% 
    knitr::kable("html") %>%
    kable_styling("striped", full_width = F) 
    })
  
  

  ## Second Page Time Series Plot
  
  # Set up Data
   producttimedata <- reactive({
    ptdata <- data.use %>% filter(marketregion == input$marketselect2 | mr2 == input$marketselect2) %>% 
      filter(weekdate >= min(input$daterange) & weekdate <= max(input$daterange)) %>%   
      filter(organic >= as.numeric(input$organic2)) %>% 
      filter(Product %in% input$products) %>% 
      group_by(Product,`Week Starting`) %>% 
      summarize(`Average Price` = round(mean(price),2),
                `Minimum` = round(min(price),2),
                `Maximum` = round(max(price),2),
                `Number Reporting` = n())   
    return(ptdata)
  })
  
  
  # Make Plot
  
  output$producttimeplot <- renderPlot({
    producttimedata() %>% 
       ggplot(aes(x=`Week Starting`,y=`Average Price`,color = Product,linetype=Product)) + geom_line(size=1.5) + geom_point()+
       scale_color_manual(values=cbPalette,name="Product") + scale_linetype_manual(values=linetypelist,name="Product") +
      scale_x_date(date_breaks = "1 week", date_labels = "%d-%b")
  })
   
  
  
  ## Second Page Table
  # Make Table
  output$producttimetable <- function()({
    producttimedata() %>% 
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) 
  })
    

  
  
  
  ## Main Page Report
  
  output$marketreport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "marketpricereport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "marketpricereport.Rmd")
      file.copy("marketpricereport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(marketselect = input$marketselect,
                    week  = input$week,
                    prices = input$organic,
                    markets = markets,
                    datause = data.use,
                    mainemap = mainemap,
                    empty = empty)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  
  ## Second Page Report

  output$productreport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "productspricereport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "productspricereport.Rmd")
      file.copy("productspricereport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(marketselect = input$marketselect2,
                     daterange  = input$daterange,
                     prices = input$organic2,
                     productlist = input$products,
                     datause = data.use,
                     empty = empty)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
    
  
    
}

## Call to the shiny function

# Run the application 
shinyApp(ui = ui, server = server)
