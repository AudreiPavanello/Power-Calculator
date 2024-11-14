# Power calculator module
powerCalculatorInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Test Parameters"),
      selectInput(
        ns("test_type"),
        "Select Test Type",
        choices = c(
          "One Sample t-test" = "t_test",
          "Two Sample t-test" = "t_test_ind",
          "One Way ANOVA" = "anova"
        )
      ),
      numericInput(
        ns("alpha"),
        "Significance Level (α)",
        value = 0.05,
        min = 0.01,
        max = 0.1,
        step = 0.01
      ),
      numericInput(
        ns("effect_size"),
        "Effect Size (d)",
        value = 0.5,
        min = 0.1,
        max = 2,
        step = 0.1
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'anova'", ns("test_type")),
        numericInput(
          ns("sample_size"),
          "Sample Size per Group",
          value = 30,
          min = 5,
          max = 1000
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'anova'", ns("test_type")),
        numericInput(
          ns("groups"),
          "Number of Groups",
          value = 3,
          min = 3,
          max = 10
        ),
        numericInput(
          ns("sample_size_anova"),
          "Sample Size per Group",
          value = 30,
          min = 5,
          max = 1000
        )
      ),
      actionButton(
        ns("calculate"),
        "Calculate Power",
        class = "btn-primary"
      )
    )
  )
}

powerCalculatorOutput <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Power Analysis Results"),
      powerPlotOutput(ns("power_plot")),
      textOutput(ns("power_result")),
      card(
        card_header("Interpretation Guide"),
        markdown("
          - **Power**: The probability of detecting a true effect if one exists
          - **Effect Size**: The magnitude of the difference between groups
          - **Sample Size**: Number of participants needed per group
          - **α (alpha)**: Probability of Type I error (false positive)
          
          Generally, a power of 0.8 (80%) or higher is considered good.
        ")
      )
    )
  )
}

powerCalculatorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Create reactive values to store parameters
    params <- reactiveVal()
    
    # Update parameters when calculate button is clicked
    observeEvent(input$calculate, {
      params(list(
        test_type = input$test_type,
        effect_size = input$effect_size,
        alpha = input$alpha,
        sample_size = if(input$test_type != "anova") input$sample_size else input$sample_size_anova,
        groups = if(input$test_type == "anova") input$groups else NULL
      ))
    })
    
    # Calculate power
    power_data <- reactive({
      req(input$calculate)
      
      calculate_power(
        test_type = input$test_type,
        effect_size = input$effect_size,
        alpha = input$alpha,
        sample_size = if(input$test_type != "anova") input$sample_size else input$sample_size_anova,
        groups = if(input$test_type == "anova") input$groups else NULL
      )
    })
    
    # Call plot module
    powerPlotServer(
      "power_plot",
      params = params,
      trigger = reactive(input$calculate)
    )
    
    # Render power results text
    output$power_result <- renderText({
      req(power_data())
      n <- if(input$test_type != "anova") input$sample_size else input$sample_size_anova
      
      paste0(
        "Statistical Power: ", round(power_data(), 3), " (", round(power_data() * 100, 1), "%)\n",
        "With current parameters:\n",
        "- Effect size (d) = ", input$effect_size, "\n",
        "- Significance level (α) = ", input$alpha, "\n",
        "- Sample size per group = ", n
      )
    })
    
    return(power_data)
  })
}