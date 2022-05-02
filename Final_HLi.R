#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(scales)
library(tidyverse)
library(ggthemes)
library(plotly)



HR <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors = T)
Accuracys <- read.csv("accuracys.csv", stringsAsFactors = T)

head(HR)
head(Accuracys)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Final H Li"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput(inputId = "Cost",
                      label = "Cost in SVM",
                      min = 5,
                      max = 40,
                      value = 5),
        
          selectInput(inputId = "Education",
                      label = "Education Level:",
                      choices = c("1", "2", "3", "4"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("MyPlot1"),
           plotlyOutput("MyPlot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$MyPlot1 <- renderPlotly({
      newAccuracy <- filter(Accuracys, Cost <= input$Cost)
      plot1 <- ggplot(data = newAccuracy, mapping = aes(x = Cost, y = Accuracy, color = Kernels)) +
        geom_point(text =paste(
          "Cost:", Accuracys$Cost,
          "\nAccuracy:", Accuracys$Accuracy,
          "\nKernel:", Accuracys$Kernels
        )) +
        geom_line(data = newAccuracy %>% filter(Kernels == "vanilladot")) +
        geom_line(data = newAccuracy %>% filter(Kernels == "rbfdot")) +
        geom_line(data = newAccuracy %>% filter(Kernels == "polydot")) +
        geom_line(data = newAccuracy %>% filter(Kernels == "tanhdot")) +
        labs(title = "Cost VS Accuracy with different Kernels in SVM",
             x = "Cost",
             y = "Accuracy")
      
     ggplotly(plot1)
    })
    output$MyPlot2 <- renderPlotly({
      
      newHR <- filter(HR, Education == input$Education)
      
      
      plot2 <- ggplot(data = newHR, mapping = aes(x = TotalWorkingYears, y = MonthlyIncome, color = as.factor(JobLevel))) +
        geom_point(text =paste(
          "MonthlyIncome:", Accuracys$MonthlyIncome,
          "\nAge:", Accuracys$Age,
          "\nTotalWorkingYears:", Accuracys$TotalWorkingYears
        )) +
        labs(title = "TotalWorkingYears VS MonthlyIncome",
             x = "TotalWorkingYears",
             y = "MonthlyIncome")
      
      ggplotly(plot2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
















