library(BDD)
library(comparator)
library(tidyverse)
library(exchanger)
source("run_sadinle.R")

expt_name <- paste0("cora_sadinle_", gsub("[ :]", "_", date()))

setwd("./datasets")
source("load_cora.R")
setwd("../")

rec_ids <- seq_len(n_records)
records$ID <- rec_ids

dist_2 <- function(x, y) {
  x <- strsplit(x, '\\s+')
  y <- strsplit(y, '\\s+')
  FuzzyTokenSet(Abbreviation(symmetric = TRUE))(x, y)
}

scoring_fns <- list(
  authors = dist_2,
  title = dist_2,
  venue = dist_2,
  year = Levenshtein(normalize=TRUE)
)

scoring_breaks <- list(
  authors = c(-Inf,0.1,0.2,0.3,0.5,Inf),
  title = c(-Inf,0.1,0.2,0.3,0.5,Inf),
  venue = c(-Inf,0.1,0.2,0.3,0.5,Inf),
  year = c(-Inf,0.1,0.2,0.3,0.5,Inf)
)

system.time(
pairs <- records %>% 
  pairs_fuzzyblock('authors', scoring_fns$authors, 0.3, id_col = 'ID') %>%
  compute_scores(records, scoring_fns, id_col = 'ID') %>% 
  discretize_scores(scoring_breaks)
)

message("Number of pairs:", nrow(pairs))

# this one is the one I'd use
lambda <- list(
  authors = rep(0.95, 4),
  title = rep(0.95, 4),
  venue = rep(0.95, 4),
  year = rep(0.95, 4)
)

criterion1 <- (pairs$authors < 3) & (pairs$title < 3) & (pairs$venue < 4) & (pairs$year < 4)
criterion1[is.na(criterion1)] <- TRUE # don't drop if either attribute is missing

pairs[['candidate']] <- criterion1 # FALSE for pairs that are definite non-matches a-priori

message("Number of candidate matching pairs:", sum(pairs$candidate))

model <- BDD(pairs, lambda, id_cols = c("ID.x", "ID.y"), candidate_col = "candidate")
run_sadinle(expt_name, model, rec_ids, true_membership, n_samples = 10000, 
            burnin_interval = 100000)
