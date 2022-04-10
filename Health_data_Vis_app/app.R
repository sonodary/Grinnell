#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(shiny)
library(leaflet)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(tidyverse)
library(tools)

#https://simplemaps.com/data/us-counties

# Read the file that holds health variables
Health_Data = read_csv("CSC324_project_data.csv")

# Read the file that holds the description of each variable
Data_description = read_csv("Variable Description.csv")

# The first column is unnecessary
health_data = dplyr::select(Health_Data, -1)


#Create the list of variable names to use for selectInput
var_names = colnames(health_data)
var_names = var_names[-c(1, 2, 3)]
var_names = sort(var_names)

# Read county shapefile
county_health_contiguous <- readOGR(
    dsn= "county_final",
    layer="county_boundaries",
)

# Prepare the list of the name of the attributes
deleted = c(1, 2, 3)
attributes = colnames(health_data)[-deleted]

# Change the variable name of the dataset
for(i in 1:length(attributes)) {
    colnames(county_health_contiguous@data)[i + 12] = attributes[i]  # Rename column name
}

# The vector that holds the name of variables
county_names = county_health_contiguous@data$NAME

# Create a vector that holds national average value of every variable
mean_list = rep(NA, length(county_health_contiguous@data) - 12)
for (i in 1:length(mean_list)) {
    mean_list[i] = mean(as.numeric(unlist(na.omit(county_health_contiguous@data[i + 12]))))
}

ui = navbarPage(
    
    # Title of the App
    "Countuy Health data",
    
    # Set the theme
    theme = shinythemes::shinytheme("slate"),
    
    # The first tab 
    tabPanel("Map", 
             sidebarPanel(
                 # Give a brief instruction of the first page
                 helpText("In this page, you can create a choropleth map based on your input"),
                 
                 #W Choose which variable to display
                 selectInput(inputId="map_var",
                             label="Select the variables to display on a choropleth map",
                             choices = var_names,
                             selected = "Population"),
                 
                 # Display the summary statistics of the chosen variable
                 helpText("Summary statistics of the chosen variable"),
                 verbatimTextOutput("summary"),
                 
                 # Display the distribution
                 helpText("Distribution of the chosen variable"),
                 plotOutput(
                     "hist"
                 ),
                 
                 #Add marker to the specific country
                 selectizeInput(inputId="county_marker",
                                choices = NULL,
                                label = "Please select the county to display"
                 ),
                 # Let user choose the scale of the color
                 radioButtons(
                     inputId = "color_scale",
                     label = "Please choose the scale for the color",
                     choices = list("Numerical Value" = TRUE, "Quantile" = FALSE),
                     selected = NULL,
                     inline = FALSE,
                 ),
             ),
             
             mainPanel(
                 # Choropleth map
                 leafletOutput("county_map"),
                 
                 # Display the description of variables
                 h3(
                     "\n",
                     "\n",
                     htmlOutput("variable_discription"),
                     "\n",
                     tags$div(
                         "For detailed desctption of the chosen variable, please refer to",
                         tags$a(href="https://www.countyhealthrankings.org/", "here"),
                     ),
                 ),
                 
                 # Display the description of missing values if relevant
                 htmlOutput("missing_description"),
                 
                 # Display the table of the chosen county, State mean, and national mean with every variable 
                 dataTableOutput("table_output"),
             )
    ),
    
    # The second page
    tabPanel("Plot", 
             
             sidebarPanel(
                 # Give a brief instruction of the app
                 helpText("In this page, you can explore the correlation between two variables"),
                 
                 #Let users choose which variables to explore
                 selectInput(inputId="explanatory",
                             label="Please select the explanatory variable",
                             choices = var_names,
                             selected = "Population",
                             multiple = FALSE),
                 
                 selectInput(inputId="independent",
                             label="Please select the independent variable",
                             choices = var_names,
                             selected = "Population",
                             multiple = FALSE),
             ),
             
             mainPanel(
                 # Scatter plot
                 plotOutput("plot"),
                 # Regression's statistics
                 verbatimTextOutput(outputId = "RegSum")
             )
    )
)


server <- function(input, output, session) {
    
    # update selectinput for county marker
    updateSelectizeInput(session, 
                         'county_marker', 
                         choices = county_names, 
                         server = TRUE)
    
    
    # selectedData holds the explanatory variable and independent variable chosen by users
    selectedData = reactive({
        county_health_contiguous@data[, c(input$explanatory, input$independent)]
    })
    
    # Output of the choropleth map, implemented by leaflet
    output$county_map = renderLeaflet({
        county_health_contiguous$variable = as.numeric(
            county_health_contiguous@data[, input$map_var]
        )
        
        # Based on users' input, change the scale of color 
        if (input$color_scale == TRUE) {
            pal = colorNumeric("RdYlGn", domain = county_health_contiguous$variable)
        }
        else {
            pal = colorQuantile("RdYlGn", domain = county_health_contiguous$variable)
        }
        
        # The dataframe of the chosen county
        selected_county = subset(county_health_contiguous@data, county_health_contiguous@data$NAME == input$county_marker)
        
        # Map object
        l = leaflet(county_health_contiguous) %>%
            setView(lat=40, lng=-92, zoom=4.4) %>%
            addPolygons(
                fillColor = ~ pal(variable),
                stroke = TRUE,
                color = 'black',
                weight = 0.3,
                smoothFactor = 0.1,
                popup = paste0(tools::toTitleCase(county_health_contiguous$NAME), ": ", county_health_contiguous$variable)
            ) %>%
            # Chnage the legend to be more informative
            leaflet::addLegend(
                "bottomright",
                pal = pal,
                values = ~variable,
                title = input$map_var,
                opacity = 0.9
            ) %>% 
            # The popup displays the county name, state and the value of the chosen variable
            addPopups(lng = selected_county$lng, lat = selected_county$lat, popup = paste0("<div>",
                                                                                           "<h3>", tools::toTitleCase(selected_county$NAME), "</h3>", "County: ", selected_county$State, "<br>", "Value: ", selected_county$variable, "<div>"), options = popupOptions(closeButton = TRUE))
    })
    # Description of the variable
    output$variable_discription = renderText({
        paste0("<div>", "<b>", "Desciption of variable: ", "<h4>", Data_description$`Data description`[Data_description$`Variable name` == input$map_var], "<div>")})
    
    # Description of the missing variable
    output$missing_description = renderText({
        if (Data_description$`Missing data description`[Data_description$`Variable name` == input$map_var] != "N/A") {
            paste0("<div>", "<b>", "Description of missing data: ", "</h3>", Data_description$`Missing data description`[Data_description$`Variable name` == input$map_var], "<div>")
        }
    })
    # Summary statstics of the chosen variable
    output$summary <- renderPrint({
        county_health_contiguous$variable = as.numeric(
            county_health_contiguous@data[, input$map_var]
        )
        summary(county_health_contiguous$variable)
    })
    
    # The table of the chosen variable, state, and national average
    output$table_output = renderDataTable({
        choosen_county = subset(county_health_contiguous@data, county_health_contiguous@data$NAME == input$county_marker)
        choosen_county = choosen_county[-c(1:11)]
        if (nrow(choosen_county) == 1) {
            selected_state = subset(county_health_contiguous@data, county_health_contiguous@data$State == choosen_county$State)
            mean_state = rep(NA, length(mean_list))
            for (i in 1:length(mean_list)) {
                mean_state[i] = mean(as.numeric(unlist(na.omit(selected_state[i + 12]))))
                table = rbind(choosen_county[-c(1)], mean_state, mean_list)
                table = cbind(Measure = c(input$county_marker, choosen_county$State, "National avg"), table)
            }
        }
        # In the future work, I am going to implment the function that distinguished the same name of the county
        # else {
        #   selected_states = rep(NA, nrow(choosen_county)) 
        #   for (i in 1:nrow(choosen_county)) {
        #     selected_states[i] = subset(county_health_contiguous@data, county_health_contiguous@data$State == choosen_county[i]$State)
        #   }
        #   mean_states = rep(rep(NA, length(mean_list)), nrow(choosen_county))
        #   tables = 
        # for (j in 1:nrow(choosen_county)) {
        #   for (i in 1:length(mean_list)) {
        #   mean_states[j][i] = mean(as.numeric(unlist(na.omit(selected_state[j][i + 12]))))
        #   table = rbind(choosen_county[i][-c(1)], mean_state[j][i])
        #   table = cbind(names = c(input$county_marker, choosen_county$State), table)
        #   }
        # }
        #   table = rbind(table, mean_list)
        #   table = cbind(names = c(rownames(table),  "National avg"), table)
        #   }
        table
    },options = list(scrollX = TRUE))
    # Distribution of the values
    output$hist = renderPlot({
        hist(county_health_contiguous@data[, c(input$map_var)], xlab=input$hist, main="Distribution")
    })
    # Run regression
    lm1 = reactive({lm(as.formula(paste0("`", input$independent, "` ~ ", paste0("`", input$explanatory, "`", collapse="+"))), data=county_health_contiguous@data)})
    output$RegSum <- renderPrint({summary(lm1())})
    output$plot = renderPlot({
        plot(selectedData(),pch=16,col='blue',cex=2)
    })
}

shinyApp(ui=ui, server=server) 