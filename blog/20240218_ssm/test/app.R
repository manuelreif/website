library(shiny)
library(ggplot2)
library(data.table)

# Definieren der sim_state_space Funktion
sim_state_space <- function(time_points = 100, 
                            sigma = 1, 
                            tau = 2,
                            drift = 0){
    
    true_values <- vector(mode = "numeric", length = time_points)
    y <- vector(mode = "numeric", length = time_points)
    
    true_values[1] <- 1 # Startwert
    for(i in 2:time_points){
        true_values[i] <- true_values[i-1] + drift + rnorm(1, mean = 0, sd = sigma)
    }
    
    y <- true_values + rnorm(time_points, mean = 0, sd = tau)
    dt <- data.table(true_values = true_values, y = y, time = 1:time_points)
    
    dt
}

# UI
ui <- fluidPage(
    titlePanel("Simulation eines Zustandsraummodells"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("time_points", "Zeitpunkte:", 100, min = 1, max = 1000),
            sliderInput("sigma", "Sigma:", min = 0.01, max = 5, value = 1, step = 0.01),
            sliderInput("tau", "Tau:", min = 0.01, max = 5, value = 2, step = 0.01),
            sliderInput("drift", "Drift:", min = -5, max = 5, value = 0, step = 0.01)
        ),
        
        mainPanel(
            plotOutput("plot")
        )
    )
)

# Server-Logik
server <- function(input, output) {
    output$plot <- renderPlot({
        dat <- sim_state_space(time_points = input$time_points, 
                               sigma = input$sigma, 
                               tau = input$tau, 
                               drift = input$drift)
        
        p <- ggplot(data = dat, aes(x = time, y = y)) + 
            geom_point(shape = 1, alpha = 0.8) + 
            geom_step(aes(y = true_values)) + 
            theme_bw()
        
        p
    })
}

# App ausführen
shinyApp(ui = ui, server = server)
