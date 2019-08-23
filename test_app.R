library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(DT)
library(stringr)
library(tools)
library(shinythemes)

expected_stats <- read_excel("expected_stats.xlsx")
exit_velocity <- read_excel("exit_velocity.xlsx")

combined <- exit_velocity %>%
  inner_join(expected_stats, by = "player_id") %>%
  select(-last_name.y, -first_name.y)

ui <- fluidPage(theme = shinytheme("journal"),
                
                titlePanel("2018 Statcast Data", windowTitle = "Statcast"),
                
                sidebarLayout(
                  
                  #inputs
                  sidebarPanel(
                    
                    #select variable for y axis
                    selectInput(inputId = "y", 
                                label = "Y-axis:", 
                                choices = c("Average Hit Angle" = "avg_hit_angle","Average Hit Speed" = "avg_hit_speed", 
                                            "Barrel Percent" = "brl_percent", "BA" = "ba" ,"Estimated BA" = "est_ba","SLG" = "slg", 
                                            "Estimated SLG" = "est_slg", "wOBA" = "woba", "Estimated wOBA" = "est_woba"), 
                                selected = "avg_hit_angle"), 
                    #select variable for x axis
                    selectInput(inputId = "x", 
                                label = "X-axis", 
                                choices = c("Average Hit Angle" = "avg_hit_angle","Average Hit Speed" = "avg_hit_speed", 
                                            "Barrel Percent" = "brl_percent", "BA" = "ba" ,"Estimated BA" = "est_ba","SLG" = "slg", 
                                            "Estimated SLG" = "est_slg", "wOBA" = "woba", "Estimated wOBA" = "est_woba"), 
                                selected = "est_ba"),
                    
                    width = 4
                    
                  ),
                  #outputs
                  mainPanel(plotOutput(outputId = "scatterplot"),
                            br(),
                            
                            DT::dataTableOutput(outputId = "stats_table"),
                            
                            width = 8
                  )
                )
)

#define server function required to create scatterplot
server <- function(input, output) {
  
  #create scatterplot
  output$scatterplot <- renderPlot({
    ggplot(data = combined, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  output$stats_table <- DT::renderDataTable(
    DT::datatable(data = combined[, c(1:2, 22, 23, 25, 26, 28, 29)],
                  options = list(pageLength = 10), 
                  rownames = FALSE, 
                  colnames = c("Last", "First", "BA", "xBA", "SLG", "xSLG",
                               "wOBA", "xwOBA"))
  )
}

#create shiny app 
shinyApp(ui = ui, server = server)