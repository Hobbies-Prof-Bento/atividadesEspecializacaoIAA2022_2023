#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


#load data frame
load("./data/cancer_de_mama.RData")



# Define UI for application that draws a histogram
ui <- fluidPage(
  
    #theme = "bootstrap.css",

    # Application title
    titlePanel("Painel de estimativa de câncer de mama por Região"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      # Define the sidebar with one input
        sidebarPanel(
          selectInput("region", "Region:", choices=(cancer_de_mama.df$população)) ,
          hr() ,
          helpText("Estudante: Clístenes Grizafis Bento")
        ) ,

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("breastCancer")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$breastCancer <- renderPlot({
        # generate bars based on cancer_de_mana.df$população from ui.R
        ano <- cancer_de_mama.df$ano[cancer_de_mama.df$população == input$region]
        casos <- cancer_de_mama.df$numero_de_casos[cancer_de_mama.df$população == input$region]
        populacao <- cancer_de_mama.df$população[cancer_de_mama.df$população == input$region]
        
        # draw the histogram with the specified region
        ggplot(subset(cancer_de_mama.df,cancer_de_mama.df$população == input$region), aes(fill=ano, y=casos, x=populacao)) +
          geom_bar(position="dodge", stat="identity")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
