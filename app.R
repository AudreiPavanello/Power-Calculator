# Main app file
source("modules/power_calculator_module.R")
source("modules/plot_module.R")
source("utils/power_calculations.R")

library(shiny)
library(bslib)
library(pwr)
library(ggplot2)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  h2("Statistical Power Calculator"),
  layout_sidebar(
    sidebar = sidebar(
      powerCalculatorInput("power_calc"),
      width = 350
    ),
    powerCalculatorOutput("power_calc")
  )
)

server <- function(input, output, session) {
  powerCalculatorServer("power_calc")
}

shinyApp(ui, server)