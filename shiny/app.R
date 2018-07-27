library(shiny)
library(tidyverse)
source("source.R")
source("server.R")
source("ui.R")

x <- list(list(name = "Sample loan 1", balance = 2000, int = 0.065, min_pay = 22.71),
          list(name = "Sample loan 2", balance = 8000, int = 0.05, min_pay = 84.86),
          list(name = "Sample loan 3", balance = 1000, int = 0.03, min_pay = 17.97),
          list(name = "Sample loan 4", balance = 4000, int = 0.025, min_pay = 37.71))

word_num <- function(word, i){
  sprintf("%s%s", word, i)
}

shinyApp(
  ui = ui,
  server = server
)