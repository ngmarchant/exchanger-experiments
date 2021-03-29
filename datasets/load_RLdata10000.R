library(readr)
library(dplyr)

records <- read_csv("RLdata10000.csv.gz")

true_membership <- records$ent_id
records$ent_id <- NULL
n_records <- nrow(records)
