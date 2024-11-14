# Utility functions for power calculations
calculate_power <- function(test_type, effect_size, alpha, sample_size, groups = NULL) {
  if (test_type != "anova") {
    if (test_type == "t_test") {
      pwr.t.test(
        d = effect_size,
        n = sample_size,
        sig.level = alpha,
        type = "one.sample"
      )$power
    } else {
      pwr.t.test(
        d = effect_size,
        n = sample_size,
        sig.level = alpha,
        type = "two.sample"
      )$power
    }
  } else {
    pwr.anova.test(
      k = groups,
      n = sample_size,
      f = effect_size/2,
      sig.level = alpha
    )$power
  }
}

calculate_power_range <- function(test_type, effect_size, alpha, sample_sizes, groups = NULL) {
  sapply(sample_sizes, function(size) {
    calculate_power(test_type, effect_size, alpha, size, groups)
  })
}

create_power_plot <- function(df, title, current_n) {
  ggplot(df, aes(x = sample_size, y = power)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_vline(xintercept = current_n, linetype = "dashed", color = "green", alpha = 0.5) +
    labs(
      title = paste("Power Analysis for", title),
      x = "Sample Size per Group",
      y = "Statistical Power"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_line(color = "gray90")
    )
}