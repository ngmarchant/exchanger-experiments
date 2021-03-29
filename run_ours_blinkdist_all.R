# Runs all experiments for our ER model sequentially

scripts <- c("run_ours_blinkdist_RLdata10000.R", 
             "run_ours_blinkdist_cora.R",
             "run_ours_blinkdist_nltcs.R",
             "run_ours_blinkdist_restaurant.R")

for (script in scripts) {
  source(script)
  rm(list=ls())  # reset workspace just in case
}
