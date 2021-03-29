library(readr)
library(dplyr)

records <- read_csv("proc_nltcs.csv.gz") %>% filter(STATE == 1)

true_membership <- records$SEQ
records$SEQ <- NULL
n_records <- nrow(records)
