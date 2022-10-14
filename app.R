# A Shiny app for evaluating the performance of 
# rent prediction models in STAT442@SDSU 2022F
# Deployed at https://sdsucamp.shinyapps.io/rent/

df <- read.csv("model_performance_log.csv")
df <- subset(df, select = -Area.Locality)
students <- colnames(df)[-c(1:13)]
categories <- colnames(df)[3:12]

# calculate residues
res <- df
res[14:23] <- res[14:23] - df$Rent

library(ggplot2)
library(shiny)
library(ggpubr) # from Grace for the equation
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("STAT 442 Model performance on rent data prediction"),

    # Select input
    sidebarLayout(
        sidebarPanel(
            selectInput(
              inputId = "student",
              label = "Select a student",
              choices = students
            ),
            selectInput(
              inputId = "category",
              label = "Select a factor",
              choices = categories
            ),
            checkboxInput(
              inputId = "jitter",
              label = "Feel Jittery?",
              value = FALSE
            ),
            textOutput("pval")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("scatter_plot"),
           plotOutput("residule_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatter_plot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        ggplot(
          df, 
          aes_string(
            x = "Rent", 
            y = input$student,
            color = input$category
          )
        ) +
        geom_point() +
        xlim(3.4, 5.7) +
        ylim(3.4, 5.7) +
        xlab("Actual Rent (log10)") + 
        geom_abline(intercept = 0, slope = 1, size = 0.5) 
    })
    
    output$residule_plot <- renderPlot({
      
      if(input$category == "Area.Locality") {
        return(NULL)
      }

      p <- ggplot(
        res, 
        aes_string(
          x = input$category, 
          y = input$student,
          color = input$category
        )
      ) + 
      labs(y = paste(input$student, "residules"))
      
      # if categorical variable, box plot
      if(length(unique( res[, input$category]))< 20) {
        p <- p + 
          geom_boxplot() 
        
        if(input$jitter){
          p <- p + geom_jitter()
        }
          
      } else  # if numeric variable, scatter plot
      if(is.numeric( res[, input$category])) {
        p <- p + 
          geom_point(alpha = 1 / 10) +
          geom_smooth(method = "lm", se = FALSE) +
          stat_regline_equation()
      } 
      
      return(p)
      
    })
    
    output$pval <- renderText({
      if (length(unique(res[, input$category])) > 20) {
        return(NULL)
      } else {
        m <- aov(res[, input$student] ~ res[, input$category])
        pval <- summary(m)[[1]][["Pr(>F)"]][1]
        texts <- paste("P val =", round(pval, 5))
        
        return(texts)
      }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
