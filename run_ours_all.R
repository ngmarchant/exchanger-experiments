# Runs all experiments for our ER model sequentially

scripts <- c("run_ours_RLdata10000.R",
             "run_ours_cora.R",
             "run_ours_nltcs.R",
             "run_ours_restaurant.R",
             "run_ours_synthdata.R")

for (script in scripts) {
  source(script)
  rm(list=ls())  # reset workspace just in case
}
