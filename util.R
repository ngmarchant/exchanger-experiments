library(comparator)  # Contains normalized Levenshtein distance function
library(methods)

#' Checks whether y is an abbreviation of x
#' 
#' TODO
#' 
#' @param x A character vector.
#' @param y A character vector.
#' @param symmetric If TRUE, the output is made symmetric in `x` and `y` by 
#' taking the max over `x` <-> `y`.
#' @param ensure_alpha If TRUE, an abbreviation must begin with alphabetic 
#' characters. Otherwise abbreviations can begin with any character.
#' 
#' @return a logical vector which is TRUE if `v2` abbreviates `v1`.
is_abbreviation <- function(x, y, symmetric = FALSE, ensure_alpha = TRUE) {
  
  if (length(x) < length(y)) {
    x <- rep_len(x, length(y))
  } else if (length(y) < length(x)) {
    y <- rep_len(y, length(x))
  }
  
  result <- vector(mode = "logical", length = length(x))
  
  # Length of shorter word
  for (i in seq_along(result)) {
    w1 <- x[i]
    w2 <- y[i]
    if (is.na(w1) || is.na(w2)) {
      result[i] <- NA
      next
    }
    n1 <- nchar(w1)
    n2 <- nchar(w2)
    if (n1 < n2 && !symmetric) next
    n <- min(n1, n2)
    if (n == 0) next # one string is empty
    if (ensure_alpha) {
      # Enforce abbreviation can only begin with an alphabetic character
      if (!grepl("^[[:alpha:]]", w1) || !grepl("^[[:alpha:]]", w2)) {
        next
      }
    }
    # More efficient to use raw representation, rather than converting to 
    # a vector of chars
    raw1 <- charToRaw(w1)
    raw2 <- charToRaw(w2)
    # a vector that is TRUE as long as the characters match
    m <- sum(cumprod(raw1[1:n] == raw2[1:n]))
    # the answer
    if (m == 0) next # no prefix match
    if (m == n) {     # one is prefix of other
      if (m != max(n1, n2)) {
        result[i] <- TRUE
      }
      next
    }
    s.len <- n - m
    if (all(tail(raw1, s.len) == tail(raw2, s.len))) {
      result[i] <- TRUE
      next
    }
  }
  
  result
}

#' A distance function which accounts for abbreviations
#' 
#' TODO
#' 
#' @param abbrev_weight TODO
#' @param symmetric TODO
#' @param return a `length(v1)` by `length(v2)` distance matrix
Abbreviation <- function(abbrev_weight = 0.1, symmetric = FALSE) {
  custom_fn <- function(x, y) {
    lev_dist <- Levenshtein(normalize=TRUE)(x, y)
    ifelse(is_abbreviation(x, y, symmetric=symmetric), abbrev_weight * lev_dist, lev_dist)
  }
  CustomStringComparator(custom_fn = custom_fn, distance = TRUE, 
                         symmetric = FALSE, similarity = FALSE, 
                         tri_inequal = FALSE)
}

#' Returns the path of the RDS file for an experiment with a given prefix
get_result_rds <- function(prefix) {
  files <- list.files(pattern = paste0(prefix, "[[:alnum:]_]+_result.rds"), recursive=TRUE)
  if (length(files) == 0)
    stop("no rds file found for ", prefix)
  if (length(files) > 1) 
    warning("multiple rds files found for ", prefix, ", taking first one")
  return(files[1])
}

true_memberships <- list(
  "RLdata" = {
    records <- read.csv("datasets/RLdata10000.csv.gz")
    records$ent_id
  }, 
  "cora" = {
    records <- read.csv("datasets/cora.arff.gz", skip = 18, quote = "\"'",
                        strip.white = TRUE, header = FALSE,
                        col.names = c("authors", "volume", "title", "institution", 
                                      "venue", "address", "publisher", "year", 
                                      "pages", "editor", "note", "month", "UID"))
    records$UID
  },
  "nltcs" = {
    records <- read.csv("datasets/proc_nltcs.csv.gz") %>% filter(STATE == 1)
    records$SEQ
  },
  "rest" = {
    records <- read.csv("datasets/fz-nophone.arff.gz", skip = 10, quote = "\"'",
                        strip.white = TRUE, header = FALSE,
                        col.names = c("name", "addr", "city", "type", "UID"))
    records$UID
  }
)