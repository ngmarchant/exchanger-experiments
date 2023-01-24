# Runs all experiments for Mauricio Sadinle's model sequentially

scripts <- c("run_sadinle_RLdata10000.R",
             "run_sadinle_cora.R",
             "run_sadinle_nltcs.R",
             "run_sadinle_restaurant.R",
             "run_sadinle_synthdata.R")

for (script in scripts) {
  source(script)
  rm(list=ls())  # reset workspace just in case
}
