# Runs all experiments for blink sequentially

scripts <- c("run_blink_RLdata10000.R",
             "run_blink_cora.R",
             "run_blink_nltcs.R",
             "run_blink_restaurant.R",
             "run_blink_synthdata.R")

for (script in scripts) {
  source(script)
  rm(list=ls())  # reset workspace just in case
}
