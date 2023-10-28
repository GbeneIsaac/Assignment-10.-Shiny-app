library(shiny)
library(rsconnect)
library(usmap)
library(ggplot2)

file_path <- "Accident.10.csv"
AccidentData <- read.csv(file_path, fileEncoding="UTF-8-BOM")
AccidentData$state <- AccidentData$STATENAME

# Define UI for application
ui <- fluidPage(
    
  tags$style(HTML("
    body, .main-container {
      background-color: #000; 
      color: white; 
    }
    
    .well {
      background-color: #000;
    }
    
    .shiny-input-container, .shiny-bound-input, .shiny-bound-output {
      background-color: #000;
      color: white; 
    }
    
    #PlotByHour img { 
      filter: invert(1); 
    } 
    
    #PlotByLightCond img { 
      filter: invert(1); 
    }
    
    #usmap img { 
      filter: invert(1);
      width: 160vh;
      height: 60vh;
    }
    
    .nav > li > a {
      background-color: #555;
      color: lightgrey;
    }
    
    .nav > li.active > a,
    .nav > li.active > a:focus,
    .nav > li.active > a:hover {
      background-color: #444; 
      color: white; 
    }
    
    .footer {
      position: absolute;
      bottom: 10px;
      right: 10px;
      text-align: right;
      color: white;
    }
  ")),
  
    # Application title
    titlePanel("United States Vehicular Accidents in 2021"),
  
    tabsetPanel(
      tabPanel(
        "Accidents",
        sidebarLayout(
          sidebarPanel(
            selectizeInput(
              inputId = "state",
              label = "Select State(s)",
              choices = unique(AccidentData$STATENAME),
              multiple = TRUE,
              selected = "Alabama"
            ),
            selectizeInput(
              inputId = "months_a",
              label = "Select Month(s)",
              choices = unique(AccidentData$MONTHNAME),
              multiple = TRUE,
              selected = "January"
            )
          ),
          mainPanel(
            plotOutput("PlotByHour"),
            plotOutput("PlotByLightCond")
          )
        ),
      ),
      tabPanel(
        "Fatalities", 
        sidebarLayout(
          sidebarPanel(
            selectizeInput(
              inputId = "months_f",
              label = "Select Month(s)",
              choices = unique(AccidentData$MONTHNAME),
              multiple = TRUE,
              selected = "January"
            )
          ),
          mainPanel(
            plotOutput("usmap"),
          )
        ),
      )
    ),
  
    div(class = "footer", 
        textOutput("names"),
        textOutput("source")
    )
)



server <- function(input, output) {
    
  Filter_a <- reactive({
    filteredDF_a <- AccidentData[AccidentData$STATENAME %in% input$state,]
    fullFilter_a <- filteredDF_a[filteredDF_a$MONTHNAME %in% input$months_a,]
    return(fullFilter_a)
  })
  
  Filter_f <- reactive({
    fullFilter_f <- AccidentData[AccidentData$MONTHNAME %in% input$months_f,]
    return(fullFilter_f)
  })
    
    output$PlotByHour <- renderPlot({
         hour <- table(Filter_a()$HOUR)
         
         validate(
           need(nrow(Filter_a()) > 0, "No data available for the selected state(s)/month(s)."))
      
         #Draw the barchart
         barplot(height=hour, 
              main="Accidents By Hour",
              xlab="Hour (in Military Time)")
    })
    
    output$PlotByLightCond <- renderPlot({
      
         lCond <- table(Filter_a()$LGT_CONDNAME)
         
         validate(
           need(nrow(Filter_a()) > 0, "No data available for the selected state(s)/month(s)."))
        
         custom = c("#8B0000","#006400","#0F0F0F","#00008B","#8B008B",
                    "#8B8B00","#8B4500","#008B8B","#4B0082")
         
         #Draw the Pie Chart
         pie(lCond, labels = names(lCond), 
              main="Pie Chart of Lighting Conditions", col = custom)
    })
    
    output$names <- renderText({
      "This app was made by Gbene Isaac and William Ternes."})
    
    output$source <- renderText({
      "This app was made in RStudio and uses data from 
      https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars."})
    
    
    output$usmap <- renderPlot({
      validate(
         need(nrow(Filter_f()) > 0, "No data available for the selected month(s)."))
        
      plot_usmap(data = Filter_f(), values = "FATALS", labels = TRUE, color = "white") +
        scale_fill_continuous(low ="#006400", high = "#8B0000", name = "Fatalities", label = scales::comma) +
        labs(title = "2021 US Vehicular Accident Fatalities by State") +
        theme(legend.position = "right")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
