library(shiny)
library(glmnet)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
    titlePanel("Effect of L1 Penalty (Lasso) on Regression Slope"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("penalty",
                        "Penalty (Lambda):",
                        min = 0,
                        max = 1,
                        value = 0.1,
                        step = 0.01)
        ),
        mainPanel(
           plotOutput("regressionPlot"),
           verbatimTextOutput("modelSummary")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    output$regressionPlot <- renderPlot({
        # Generate synthetic data
        set.seed(123)
        x <- matrix(rnorm(200), ncol = 2)
        y <- 3 * x[,1] + 2 * x[,2] + rnorm(100)
        
        # Fit model with Lasso penalty (alpha = 1) using the first column of x
        fit <- glmnet(x, y, alpha = 1, lambda = input$penalty)
        
        # Extract coefficients
        intercept <- coef(fit)[1]
        slope1 <- coef(fit)[2]
        slope2 <- coef(fit)[3]
        
        # Calculate predicted values using intercept and slope of the first predictor
        y_pred <- intercept + x[,1] * slope1 + x[,2] * slope2
        
        # Combine data for plotting
        data <- data.frame(X1 = x[,1], X2 = x[,2], Y = y, Y_Pred = as.vector(y_pred))
        
        # Plot with ggplot2
        p1 <- ggplot(data, aes(x = X1, y = Y)) +
              geom_point() +
              geom_abline(intercept = intercept, slope = slope1, color = "blue") +
              labs(title = "Lasso Regression Line (X1 vs Y)",
                   x = "X1",
                   y = "Y")
        
        p2 <- ggplot(data, aes(x = X1, y = Y)) +
              geom_point(color = "red") +
              geom_point(aes(y = Y_Pred), color = "blue") +
              labs(title = "Actual vs Predicted (X1 vs Y)",
                   x = "X1",
                   y = "Y")
        
        grid.arrange(p1, p2, ncol = 2)
    })
    
    output$modelSummary <- renderPrint({
        # Generate synthetic data
        set.seed(123)
        x <- matrix(rnorm(200), ncol = 2)
        y <- 3 * x[,1] + 2 * x[,2] + rnorm(100)
        
        # Fit model with Lasso penalty (alpha = 1)
        fit <- glmnet(x, y, alpha = 1, lambda = input$penalty)
        
        # Display coefficients including intercept
        intercept <- coef(fit)[1]
        slope1 <- coef(fit)[2]
        slope2 <- coef(fit)[3]
        
        cat("Intercept:", intercept, "\n")
        cat("Slope for X1:", slope1, "\n")
        cat("Slope for X2:", slope2, "\n")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
