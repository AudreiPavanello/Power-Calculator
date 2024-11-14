# Plot module
powerPlotOutput <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}

powerPlotServer <- function(id, params, trigger) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      req(trigger(), params())
      
      p <- params()
      
      if (p$test_type != "anova") {
        n <- p$sample_size
        sample_sizes <- seq(5, n*2, length.out = 50)
        powers <- calculate_power_range(
          test_type = p$test_type,
          effect_size = p$effect_size,
          alpha = p$alpha,
          sample_sizes = sample_sizes
        )
        
        title <- if(p$test_type == "t_test") "One Sample t-test" else "Two Sample t-test"
        
      } else {
        n <- p$sample_size
        groups <- p$groups
        sample_sizes <- seq(5, n*2, length.out = 50)
        powers <- calculate_power_range(
          test_type = p$test_type,
          effect_size = p$effect_size,
          alpha = p$alpha,
          sample_sizes = sample_sizes,
          groups = groups
        )
        
        title <- paste("One Way ANOVA (", groups, "groups)")
      }
      
      df <- data.frame(
        sample_size = sample_sizes,
        power = powers
      )
      
      create_power_plot(df, title, n)
    })
  })
}