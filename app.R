library(ggplot2)
library(plotly)
library(scales)
library(dplyr)
theme_set(theme_light())
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")


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
    titlePanel("Gender Disparity Explore"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("major_category",
                        " Occupation category",
                        choices = unique(jobs_gender$major_category))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("jobs_scatter", height = "700px", )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$jobs_scatter <- renderPlotly({
      p3 <- jobs_gender %>% 
        filter(year == 2016,
               total_workers>=20000) %>%
        filter(major_category  == input$major_category) %>% 
        arrange(desc(wage_percent_of_male)) %>% 
        mutate(percent_female = workers_female/total_workers,
               wage_percent_of_female = total_earnings_female/ total_earnings_male) %>% 
        ggplot(aes( percent_female,
                    wage_percent_of_female,
                    color = minor_category, 
                    size  = total_workers,
                    label = occupation,
        ))+
        geom_point()+
        scale_size_continuous(range = c(1,10), guide = FALSE)+
        labs(size = "Total number f workers",
             x = "% of woork force reported as female",
             y = "% percentage of female salary / median male",
             title = "Gender disparity and py gap in 2016",
             color = "Minor Category",
             subtitle = "only occupation with at least 20,000 worker total")+
        scale_x_continuous(labels = percent_format())+
        scale_y_continuous(labels =  percent_format())
      
      
      
      
      ggplotly(p3)
      
      
      

  

      
    })
}

 # Run the application 
shinyApp(ui = ui, server = server)
