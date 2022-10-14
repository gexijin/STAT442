# A shiny app for analyzing prediction results

# Read in and prepare data
df <- read.csv("model_performance_log.csv")
students <- colnames(df)[-c(1:14)]
factors <- colnames(df)[3:13]
# calculate residues
res <- df
res[15:24] <- res[15:24] - df$Rent

library(ggplot2)
library(shiny)

# Define UI
ui <- fluidPage(
    titlePanel("Model performance for rent data prediction"),

    sidebarLayout(
        sidebarPanel(
            # two select inputs
            selectInput(
              inputId = "student",
              label = "Select a student",
              choices = students
            ),
            
            selectInput(
              inputId = "factor",
              label = "Select a factor",
              choices = factors
            )
        ),

        # Place plots
        mainPanel(
           plotOutput("scatter_plot")
        )
    )
)

# Define server logic 
server <- function(input, output) {

      output$scatter_plot <- renderPlot({
        ggplot(
          df, 
          aes_string(
            x = "Rent", 
            y = input$student,
            color = input$factor
          )
        ) +
        geom_point() +
          geom_abline(
            intercept = 0,
            slope = 1,
            size = 0.5
          )
      })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
