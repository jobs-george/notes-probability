library(shiny)
library(dplyr)

source("nlsfit.R")

ui <- fluidPage(
  
  titlePanel("Nonlinear Regression"),
  
  helpText("Nonlinear regressions based on Bates and Watts delta-method."),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file", "Data", buttonLabel = "Upload..."),
      
      downloadButton("download", "Template"),
      
      hr(),
      
      tableOutput("preview")
      
    ),
    
    mainPanel(
      
      plotOutput("plot"),
      
      wellPanel(
        
        h4("Results"),
        
        textOutput("value50")
      )
      
    )
    
  ),
  
  hr(),
  
  helpText(a("Documentation", href="doc.pdf"))
  
)

server <- function(input, output, session) {
  
  # Download ---
  output$download <- downloadHandler(
    
    filename = function() {
      "template.csv"
    },
    
    content <- function(file) {
      file.copy(file.path("data", "template.csv"), file)
    },
    
    contentType = "text/csv"
    
  )
  
  # Upload ---
  data <- reactive({
    
    req(input$file)
    dplyr::filter(vroom::vroom(input$file$datapath, delim = ",", show_col_types = FALSE), DOSE != 0)
    
  })
  
  # Fit ---
  fit <- reactive({
    
    x <- data()$INPUT
    y <- data()$RESPONSE
    
    # guess initial value
    theta0 <- Sigmoid(NULL, FALSE, x, y, init = TRUE)
    
    # compute best fit
    nlsfit(model = Sigmoid, theta = theta0, x, y)
    
  })
  
  # Bands ---
  bands <- reactive({
    
    x <- data()$INPUT
    y <- data()$RESPONSE
    
    theta <- fit()$coef
    
    # calculate bands
    x_band <- seq(min(x), max(x), length.out=1000)
    
    y_band <- sapply(x_band, confidence_bands, model=Sigmoid, fit=fit(), theta=theta)
    y_band_upper <- sapply(x_band, confidence_bands, model=Sigmoid, fit=fit(), theta=theta, flag_upper = T)
    y_band_lower <- sapply(x_band, confidence_bands, model=Sigmoid, fit=fit(), theta=theta, flag_upper = F)
    
    list(x_band=x_band, y_band=y_band, y_band_upper=y_band_upper, y_band_lower=y_band_lower)
    
  })
  
  # value50 ---
  value50 <- reactive({
    
    value50 <- bands()$x_band[which.min(abs(bands()$y_band-50))]
    value50_upper <- bands()$x_band[which.min(abs(bands()$y_band_upper-50))]
    value50_lower <- bands()$x_band[which.min(abs(bands()$y_band_lower-50))]
    
    list(value50=value50, value50_upper=value50_upper, value50_lower=value50_lower)
    
  })
  
  # Preview ---
  output$preview <- renderTable(head(data(), n = 20))
  
  # Plot ---
  output$plot <- renderPlot({
    
    x <- data()$DOSE
    y <- data()$RESPONSE
    
    plot(x, y, xlab = "INPUT", ylab = "RESPONSE", ylim = c(0, 100))
    
    # plot best fit
    theta <- fit()$coef
    curve(100 / (1 + theta[1] * exp(theta[2] * x)), from = min(x), to = max(x), add = TRUE)
    
    # plot bands
    lines(bands()$x_band, bands()$y_band_upper, type="l", lty=2)
    lines(bands()$x_band, bands()$y_band_lower, type="l", lty=2)
    
    # plot value50
    lines(c(value50()$value50, value50()$value50), c(0, 50))
    lines(c(0, value50()$value50), c(50, 50))
    
    lines(c(value50()$value50_upper, value50()$value50_upper), c(0, 50), type="l", lty=2)
    lines(c(0, value50()$value50_upper), c(50, 50), type="l", lty=2)
    
    lines(c(value50()$value50_lower, value50()$value50_lower), c(0, 50), type="l", lty=2)
    
  })
  
  # Output ---
  output$value50 <- renderPrint({
    
    cat("Predicted value50 = ", round(value50()$value50, digits = 2), 
        ", 95% Confidence from ",
        round(value50()$value50_lower, digits = 2), 
        " to ",
        round(value50()$value50_upper, digits = 2))
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)