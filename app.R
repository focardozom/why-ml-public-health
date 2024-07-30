library(shiny)
library(glmnet)
library(gridExtra)

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
        
        # Fit model with Lasso penalty (alpha = 1) using both columns of x
        fit <- glmnet(x, y, alpha = 1, lambda = input$penalty)
        
        # Extract coefficients
        intercept <- coef(fit)[1]
        slope1 <- coef(fit)[2]
        slope2 <- coef(fit)[3]
        
        # Calculate predicted values using intercept and slopes
        y_pred <- intercept + x[,1] * slope1 + x[,2] * slope2
        
        # Combine data for plotting
        data <- data.frame(X1 = x[,1], Y = y, Y_Pred = y_pred)
        
        # Plot data and regression line
        par(mfrow = c(1, 2))
        
        # Plot 1: Regression line
        plot(data$X1, data$Y, main = "Lasso Regression Line (X1 vs Y)",
             xlab = "X1", ylab = "Y", pch = 19, col = "red")
        abline(intercept, slope1, col = "blue", lwd = 2)
        
        # Plot 2: Actual vs Predicted values
        plot(data$X1, data$Y, main = "Actual vs Predicted (X1 vs Y)",
             xlab = "X1", ylab = "Y", pch = 19, col = "red")
        points(data$X1, data$Y_Pred, col = "blue", pch = 19)
        
        par(mfrow = c(1, 1))
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