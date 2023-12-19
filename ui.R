# Load necessary libraries
library(shiny)
library(ggplot2) # for plotting (you can choose any other plotting library you prefer)

# Define UI
ui <- fluidPage(
  titlePanel("Simple Shiny App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select a dataset:",
                  choices = c("iris", "mtcars", "CO2"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to create the plot based on user selection
  selectedData <- reactive({
    switch(input$dataset,
           "iris" = iris,
           "mtcars" = mtcars,
           "CO2" = CO2)
  })
  
  # Determine columns for x and y axis based on selected dataset
  plotData <- reactive({
    data <- selectedData()
    if (input$dataset == "iris") {
      x_col <- "Sepal.Length"
      y_col <- "Sepal.Width"
    } else if (input$dataset == "mtcars") {
      x_col <- "mpg"
      y_col <- "disp"
    } else if (input$dataset == "CO2") {
      x_col <- "uptake"
      y_col <- "conc"
    }
    return(data.frame(x = data[, x_col], y = data[, y_col]))
  })
  
  # Render the plot
  output$plot <- renderPlot({
    ggplot(plotData(), aes(x = x, y = y)) +
      geom_line() +
      labs(title = paste("Scatter plot of", input$dataset))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

