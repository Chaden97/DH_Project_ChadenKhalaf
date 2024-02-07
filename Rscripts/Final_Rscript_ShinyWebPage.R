# Load required libraries
library(shiny)
library(dplyr)
library(plotly)

# Load the cleaned CSV data
data <- read.csv("linelist.csv")
# Define the variable mapping between display names and original variable names
variable_mapping <- list(
  "Family History With Overweight" = "Family_History_With_Overweight",
  "Transportation Mode" = "Transportation_Mode",
  "Frequency Physical Activity" = "Frequency_Physical_Activity",
  "Consumption Of Food Between Meals" = "Consumption_Of_Food_Between_Meals",
  "Frequent High Caloric Food Consumption" = "Frequent_High_Caloric_Food_Consumption",
  "Number Of Main Meals" = "Number_Of_Main_Meals",
  "Consumption Of Water Daily" = "Consumption_Of_Water_Daily",
  "Calories Consumption Monitoring" = "Calories_Consumption_Monitoring",
  "Time Using Technology Devices" = "Time_Using_Technology_Devices"
  )

# Calculate BMI using the provided formula
data$bmi <- data$Weight / (data$Height)**2

# Create tooltip
data$tooltip <- paste(
  "Age: ", data$Age, "<br>",
  "BMI: ", round(data$bmi, 2), "<br>",
  "Gender: ", data$Gender,
  sep = ""
) %>%
  sapply(htmltools::HTML)

ui <- fluidPage(
  titlePanel(
    tags$h1("Interactive Analysis of Obesity Factors", style = "color: blue;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Welcome to the Interactive Analysis of Obesity Factors!"),
      p("Explore the factors influencing obesity levels among individuals from Mexico, Peru, and Colombia."),
      hr(),
      selectInput("x_var", "Select X Variable:",
                  choices = names(variable_mapping)),
      sliderInput("age_range", "Select Age Range:",
                  min = min(data$Age), max = max(data$Age), value = c(min(data$Age), max(data$Age))),
      selectInput("gender", "Select Gender:",
                  choices = c("All", unique(data$Gender))),
      
      
      # BMI Classification Table
      tableOutput("bmi_classification_table"),
      
      # Explanation of how to use the app
      tags$p("To utilize this application, start by selecting a variable of interest from the dropdown menu on the left sidebar, such as 'Family History With Overweight' or 'Transportation Mode.' Next, adjust the age range slider and choose a gender filter if desired. Once you've set your parameters, observe the histogram on the main panel to visualize the distribution of obesity levels corresponding to your selected variable. Additionally, explore the scatter plot to understand the relationship between age and BMI, with individual data points revealing gender-specific insights upon hovering. Refer to the BMI Classification Table for guidance on interpreting BMI values. Feel free to adjust the inputs and explore different combinations to gain deeper insights into factors influencing obesity levels.")
    ),
    
    mainPanel(
      plotlyOutput("obesityPlot"),
      br(), # Add line break for spacing
      plotlyOutput("scatterPlot")
    )
  ),
  
  # User feedback section at the end of the document
  fluidRow(
    column(12, align = "center", h3("Understanding Obesity")),
    column(12, align = "center", h3("Since 1980, the prevalence of obesity has risen worldwide, having major negative effects on overall health. Over 1.9 billion individuals who were 18 years of age or older were overweight in 2016. More than 650 million of them were obese. A number of factors contribute to obesity, such as excessive fat and energy intake, and decreased physical activity as a result of sedentary lifestyle, new transportation modes, and expanding urbanization. Furthermore, genetic predispositions and other biological risk factors can exacerbate obesity. On the bright side, obesity is preventable. People may significantly decrease their risk of obesity by adopting healthy lifestyle practices including mindful eating, balanced diet, and routine exercise. However, cooperation between different stakeholders, including governments, healthcare providers, communities, and individuals themselves, is necessary to achieve widespread prevention. By working together, they can put policies, initiatives, and programs into place that support education, and healthy living environments.")),
    column(12, align = "center", h3("User Feedback")),
    column(12, align = "center", textInput("feedback", "Share your feedback")),
    column(12, align = "center", actionButton("submit", "Submit"))
  )
)

server <- function(input, output) {
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    # Filter data based on selected age range
    filtered_data <- data %>%
      filter(Age >= input$age_range[1] & Age <= input$age_range[2])
    
    # Filter data based on selected gender if "All" is not selected
    if (input$gender != "All" && "Gender" %in% colnames(data)) {
      filtered_data <- filtered_data %>%
        filter(Gender == input$gender)
    }
    
    return(filtered_data)
  })
  # Create the histogram of obesity levels
  output$obesityPlot <- renderPlotly({
    p <- filtered_data() %>%
      plot_ly(x = ~get(variable_mapping[[input$x_var]]), color = ~gsub("_", " ", Obesity_Level)) %>%
      add_histogram() %>%
      layout(title = paste("Obesity Level by", input$x_var),
             xaxis = list(title = input$x_var),
             yaxis = list(title = "Count"),
             barmode = "stack")
p
})

  
  # Create the scatter plot of BMI split by gender
  output$scatterPlot <- renderPlotly({
    p <- plot_ly(data, x = ~Age, y = ~bmi, color = ~Gender,
                 colors = c("pink", "blue"), text = ~tooltip, hoverinfo = "text",
                 type = 'scatter', mode = 'markers') %>%
      layout(title = 'Age vs BMI', xaxis = list(title = 'Age'),
             yaxis = list(title = 'Body mass index(BMI)'),
             legend = list(title = list(text = '<b> Gender </b>')))
    
    p
  })
  
  # Reactive expression for BMI classification table data
  output$bmi_classification_table <- renderTable({
    classification <- data.frame(
      BMI_Classification = c("Underweight", "Normal", "Overweight", "Obesity I", "Obesity II", "Obesity III"),
      BMI_Range = c("Less than 18.5", "18.5 to 24.9", "25.0 to 29.9", "30.0 to 34.9", "35.0 to 39.9", "Higher than 40")
    )
    names(classification) <- gsub("_", " ", names(classification))
    
    classification
  }, sanitize.text.function = function(x) x)
  
  # User feedback submission
  observeEvent(input$submit, {
    feedback <- isolate(input$feedback)
    # Process feedback here, e.g., store it in a database or file
    # For demonstration purposes, printing the feedback to console
    cat("User feedback:", feedback, "\n")
    # Reset feedback input field
    updateTextInput(session, "feedback", value = "")
  })
 }
shinyApp(ui = ui, server = server)
