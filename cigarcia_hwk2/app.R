library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(dplyr)
library(scales)
library(shinydashboard)
library(reshape2)
library(plotly)
library(hms)
library(readr)
library(tidyverse)



#Importing Data

housing <- read.csv("miami_housing_data.csv") 

print(colnames(housing))

#Changing Column names 
colnames(housing) <- c("lat","long","parcel.no","sale.prc","lnd.sqft", "tot.lvg.area", "spec.feat.val", 
                       "rail.dist", "ocean.dist", "water.dist", "cntr.dist", "subcntr.di", "hw.dist", "age", 
                       "avno60plus", "month.sold", "struct.quality")

#class(housing$sale.prc) <-"Numeric"

housing$sale.prc <- as.numeric(gsub(",","",housing$sale.prc))

housing$price.range <- 0 

housing$price.range[housing$sale.prc < 100000] <- "Less than $100,000"
housing$price.range[housing$sale.prc >= 100000 & housing$sale.prc <= 250000] <- "$100,000 - $250,000"
housing$price.range[housing$sale.prc > 250000 & housing$sale.prc <= 5000000] <- "$250,001 - $500,000"
housing$price.range[housing$sale.prc > 500000 & housing$sale.prc <= 1000000] <- "$500,001 - $1,000,000"
housing$price.range[housing$sale.prc > 1000000] <- "More than $1,000,000"


housing$month.sold.name <- 0
housing$month.sold.name[housing$month.sold == 1] <- "January"
housing$month.sold.name[housing$month.sold == 2] <- "February"
housing$month.sold.name[housing$month.sold == 3] <- "March"
housing$month.sold.name[housing$month.sold == 4] <- "April"
housing$month.sold.name[housing$month.sold == 5] <- "May"
housing$month.sold.name[housing$month.sold == 6] <- "June"
housing$month.sold.name[housing$month.sold == 7] <- "July"
housing$month.sold.name[housing$month.sold == 8] <- "August"
housing$month.sold.name[housing$month.sold == 9] <- "September"
housing$month.sold.name[housing$month.sold == 10] <- "October"
housing$month.sold.name[housing$month.sold == 11] <- "November"
housing$month.sold.name[housing$month.sold == 12] <- "December"

housing$month.sold.name <- factor(housing$month.sold.name, 
                                  levels= rev(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Miami Housing Market")


# Define UI for application that plots features of movies -----------
  
sidebar <- dashboardSidebar(
  sidebarMenu(
  
    id = "tabs",
    
    #Menu Items------------------------------------------------------------
    menuItem("Plots", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Statistics", icon = icon("cog"), tabName = "averages", badgeLabel = "", badgeColor = "blue"),
    menuItem("Data Table", icon = icon("table"), tabName = "table", badgeLabel = "", badgeColor = "blue"),
    
    
    # Horizontal line for visual separation -----------------------
    hr(),
      
    # Select variable for y-axis ----------------------------------
    selectInput(inputId = "y", 
                  label = "Plots - Market Analysis Y-axis:",
                  choices = c("Sale price" = "sale.prc", 
                              "Land area (sqft)" = "lnd.sqft", 
                              "Floor area (sqft)" = "tot.lvg.area", 
                              "Value of special features" = "spec.feat.val", 
                              "Distance to the ocean" = "ocean.dist", 
                              "Distance to nearest water body" = "water.dist",
                              "Distance to business dist." = "cntr.dist", 
                              "Distance to the highway" = "hw.dist"), 
                  selected = "sale.prc"),
     
    
    # Select variable for x-axis ----------------------------------
    selectInput(inputId = "x", 
                  label = "Plots - Market Analysis X-axis:",
                  choices = c("Sale price" = "sale.prc", 
                              "Land area (sqft)" = "lnd.sqft", 
                              "Floor area (sqft)" = "tot.lvg.area", 
                              "Value of special features" = "spec.feat.val", 
                              "Distance to the ocean" = "ocean.dist", 
                              "Distance to nearest water body" = "water.dist",
                              "Distance to business dist." = "cntr.dist", 
                              "Distance to the highway" = "hw.dist"), 
                  selected = "lnd.sqft"),
      
      
    # Horizontal line for visual separation -----------------------
    hr(),      
    
      # Set Age of the property ------------------------------------
      sliderInput(inputId = "property.age",
                  label = "Select age of the property for all graphs, averages and table:", 
                  min = 0, max = 96, 
                  value = c(0, 10)),
      
    
     # Set Age of the property ------------------------------------
     sliderInput(inputId = "property.ocean.dist",
                label = "Select properties ocean distance (ft) for all graphs, averages and table:", 
                min = 0, max = 67000, 
                value = c(0, 5280)),    
    
    
     # Set Price Range of the property -----------------------------
     checkboxGroupInput(inputId = "property.price",
                       label = "Select price of the property for all graphs, averages and table:",
                       choices = c("Less than $100,000", 
                                   "$100,000 - $250,000", 
                                   "$250,001 - $500,000", 
                                   "$500,001 - $1,000,000", 
                                   "More than $1,000,000"),
                       selected = c("Less than $100,000", "$100,000 - $250,000", "$250,001 - $500,000", "$500,001 - $1,000,000", "More than $1,000,000")),
    
  
    
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE), 
      
      
      # Add Download Button
      downloadButton("download.data", "Download"),
      h6("Press the download button to save the dataset.")
      
    )
  )

    
    # Output --------------------------------------------------------
    
dashboard.body <- dashboardBody(tabItems(
  
    #Plots---------------------------------------------------------
  tabItem("plot",
      # Tabs to separate each graph
      fluidRow(tabBox(title = "Plot",
        width = 12,
        tabPanel("Market Analysis", plotlyOutput(outputId = "scatterplot")), #tab for scatter plot
        tabPanel("Month Sold Distribution", plotlyOutput(outputId = "bar.chart")), #tab for bar chart
        tabPanel("Sale Price Distribution", plotlyOutput(outputId = "pie.chart")) #tab for pie chart
      )
      )  
      ),

  
     # Averages page ----------------------------------------------
   tabItem("averages",
              # Input and Value Boxes ----------------------------------------------
              fluidRow(
                infoBoxOutput("avg.price", width = 4),
                infoBoxOutput("avg.land", width = 4),
                infoBoxOutput("avg.tot.lvg.area", width = 4),
                infoBoxOutput("avg.ocean.dist", width = 4),
                infoBoxOutput("avg.water.dist", width = 4),
                #infoBoxOutput("avg.cntr.distt", width = 4),
                infoBoxOutput("avg.hw.dist", width = 4),
              )
      ),
      
      # Show data table ---------------------------------------------
  tabItem("table", 
          fluidPage(
          box(title = "Properties Table" , 
              DT::dataTableOutput(outputId = "table"), 
              width = 12))
  )
  )
  )

ui <- dashboardPage(header, sidebar, dashboard.body, skin = "black")


# Define server function required to create the scatter plot ---------
server <- function(input, output) {
  
  housing.subset <- reactive({
      req(input$property.age, input$property.price, input$property.ocean.dist)
      filter(housing, price.range %in% input$property.price & age >= input$property.age[1] & age <= input$property.age[2] & 
               ocean.dist >= input$property.ocean.dist[1] & ocean.dist <= input$property.ocean.dist[2])
  })
  

  # Create Bar Chart -------------------------------------------------
  output$bar.chart <- renderPlotly({
    ggplotly(
      ggplot(data = housing.subset(), aes(x = month.sold.name)) +
        geom_bar(color = 'lightblue', fill = 'lightblue') +
        ggtitle("Number of properties sold per month in 2016") +
        xlab("Month of Sale") +
        ylab("Property Count") +
        theme_classic() +
        coord_flip() +
        geom_text(stat='count', aes(label=..count..), position = position_stack(vjust= 1.03)) + 
        theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
              axis.title.y = element_text(face = "bold"))
    )
  })
  
  
  # Create scatter plot ----------------------------------------------
  output$scatterplot <- renderPlotly({
    ggplotly(
      ggplot(data = housing.subset(), aes_string(x = input$x, y = input$y)) +
        geom_point(color = "steelblue") +
        scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
        scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
        theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
              axis.title.y = element_text(face = "bold")) + 
        labs(x = toTitleCase(str_replace_all(input$x, "\\.", " ")),
             y = toTitleCase(str_replace_all(input$y, "\\.", " "))
    ))  
  }
  )
  
  
  # # Create Pie Chart-------------------------------------------------

  output$pie.chart <- renderPlotly({

    # Plotting the pie chart using plot_ly() function
    pie <- plot_ly(housing.subset(), values =  ~sale.prc, labels = ~price.range,
                   type = "pie",
                   textposition = "outside", 
                   hovertemplate = "<b>%{label}</b><br>Percent of total: %{percent}<extra></extra>")
    
    return(pie)

  })

  
  # Average Price ----------------------------------------------------
  
  output$avg.price <- renderValueBox({
    house <- housing.subset()
    avg <- round(mean(house$sale.prc, na.rm = T), 2)
    formatted_avg <- paste0("$", format(avg, nsmall = 2, big.mark = ","))
    
    valueBox(subtitle = "Average Sale Price ($)", value = formatted_avg , icon = icon("dollar-sign"), color = "green")
}) 
    
    
  # Average Land Square Ft -------------------------------------------
  output$avg.land <- renderValueBox({
    house <- housing.subset()
    avg <- round(mean(house$lnd.sqft, na.rm = T), 2)
    formatted_avg <- format(avg, big.mark = ",", decimal.mark = ".", nsmall = 2)
    
    valueBox(subtitle = "Average Land Area (sqft)", value = formatted_avg, icon = icon("mountain"), color = "blue")
  })
  
  # Average Floor Area Square Ft -------------------------------------
  output$avg.tot.lvg.area <- renderValueBox({
    house <- housing.subset()
    avg <- round(mean(as.numeric(house$tot.lvg.area), na.rm = T), 2)
    formatted_avg <- format(avg, big.mark = ",", decimal.mark = ".", nsmall = 2)
    
    valueBox(subtitle = "Average Floor Area (sqft)", value = formatted_avg , icon = icon("house"), color = "red")
  })
  
  # Average Ocean Distance -------------------------------------------
  output$avg.ocean.dist <- renderValueBox({
    house <- housing.subset()
    avg <- round(mean(as.numeric(house$ocean.dist), na.rm = T), 2)
    formatted_avg <- format(avg, big.mark = ",", decimal.mark = ".", nsmall = 2)
    
    valueBox(subtitle = "Average Ocean Distance (ft)", value = formatted_avg, icon = icon("fish"), color = "purple")
  })
  
  # Average Water Distance -------------------------------------------
  output$avg.water.dist <- renderValueBox({
    house <- housing.subset()
    avg <- round(mean(as.numeric(house$water.dist), na.rm = T), 2)
    formatted_avg <- format(avg, big.mark = ",", decimal.mark = ".", nsmall = 2)

    valueBox(subtitle = "Average Water Distance (ft)", value = formatted_avg, icon = icon("water"), color = "teal")
  })
  
  
  # Average Highway Distance -------------------------------------------
  output$avg.hw.dist <- renderValueBox({
    house <- housing.subset()
    avg <- round(mean(as.numeric(house$hw.dist), na.rm = T), 2)
    formatted_avg <- format(avg, big.mark = ",", decimal.mark = ".", nsmall = 2)
    
    valueBox(subtitle = "Average Highway Distance (ft)", value = formatted_avg, icon = icon("truck"), color = "yellow")
  })

  
  # Print data table------------------------------------------------------
  output$table <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = housing.subset()[0:14], 
                    options = list(pageLength = 20), 
                    rownames = FALSE,
                    colnames = c('latitude' = 'lat', 'longitude' = 'long', 'parcel no' = 'parcel.no', 'sale price' = 'sale.prc', 
                                 'land area' = 'lnd.sqft', 'floor area' = 'tot.lvg.area', 'special features value' = 'spec.feat.val', 
                                 'rail dist' = 'rail.dist', 'ocean dist' = 'ocean.dist', 'water dist' = 'water.dist', 'business center dist' = 'cntr.dist',
                                 'sub-center dist' = 'subcntr.di', 'highway dist' = 'hw.dist', 'property age' = 'age'))  %>% 
        formatCurrency('sale price', "$") %>% 
        formatCurrency(c('land area', 'floor area','special features value','rail dist','ocean dist','water dist', 
                         'business center dist', 'sub-center dist', 'highway dist'), "")%>% 
        formatRound(c('land area', 'floor area','special features value','rail dist','ocean dist','water dist', 
                      'business center dist', 'sub-center dist', 'highway dist'), 1)
    }
  )
  
  # Download data function------------------------------------------------
  output$download.data <- downloadHandler(
    filename = function() {
      paste("housing.miami", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(housing.subset(), file)
    }
  )
} 

# Run the application --------------------------------------------------
shinyApp(ui = ui, server = server)


