library(shiny)


#-----------------------------
fluidPage(
    # Application title
    titlePanel("Countries Analysis through Clustring"),
    
    # Sidebar with a slider input for number of bins
    sliderInput(inputId = "num", label = "Number of Clusters", min = 2, max = 10, value = 3),
    radioButtons(inputId = "rad", label = "What ellipse type should you use?", choices = list("euclid", "convex"), selected = "euclid"),
    # Show a plot of the analysis
    mainPanel(
        plotOutput(outputId = "chart")
    )

)
