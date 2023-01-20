library(tidyverse)
library(exchanger)
library(BDD)
source("run_sadinle.R")

expt_name <- paste0("restaurant_sadinle_", gsub("[ :]", "_", date()))

setwd("./datasets")
source("load_restaurant.R")
setwd("../")

rec_ids <- seq_len(n_records)
records$ID <- rec_ids

dist_2 <- function(x, y) {
  x <- strsplit(x, '\\s+')
  y <- strsplit(y, '\\s+')
  FuzzyTokenSet(Abbreviation(symmetric = TRUE), deletion = 0.5, insertion = 0.5)(x, y)
}

scoring_fns <- list(
  name = dist_2,
  addr = dist_2,
  city = BinaryComp(),
  type = BinaryComp()
)

scoring_breaks <- list(
  name = c(-Inf,.1,.2,.3,.4,Inf),
  addr = c(-Inf,.1,.2,.3,.4,Inf),
  city = c(-Inf,0,Inf),
  type = c(-Inf,0,Inf)
)

system.time(
pairs <- pairs_all(records$ID) %>%
  compute_scores(records, scoring_fns, id_col = 'ID') %>% 
  discretize_scores(scoring_breaks)
)

message("Number of pairs: ", nrow(pairs))

lambda <- list(
  name = rep(0.95, 4),
  addr = rep(0.95, 4),
  city = c(0.95),
  type = c(0.95)
)

criterion1 <- (pairs$name < 3) & (pairs$addr < 3)
criterion1[is.na(criterion1)] <- TRUE # don't drop if either attribute is missing

pairs[['candidate']] <- criterion1 # FALSE for pairs that are definite non-matches a-priori

message("Number of candidate matching pairs: ", sum(pairs$candidate))

model <- BDD(pairs, lambda, id_cols = c("ID.x", "ID.y"), candidate_col = "candidate")
run_sadinle(expt_name, model, rec_ids, true_membership, n_samples = 10000, 
            burnin_interval = 100000)
