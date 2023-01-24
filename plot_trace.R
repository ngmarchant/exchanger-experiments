## This script generates Geweke diagnostics for our model (under the 
## generalized coupon prior) + baseline models by Steorts (2015) and Sadinle 
## (2014).
## 
## Output:
##   `{expt_id}_geweke-plot.pdf`: Geweke plot for a selection of model 
##   variables.

library(exchanger)
library(BDD)
library(coda)            # for manipulating 'mcmc' objects and `geweke.diag`
library(future)
library(future.apply)    # parallelization
library(latex2exp)       # for variable names in plot
library(stringr)
library(scales)
library(dplyr)
library(ggplot2)
library(tidyr)
source("util.R")         # contains definition of `get_result_rds`

# Entries for our model
expts <- expand.grid(
  data.name = c("RLdata10000", "nltcs", "cora", "rest"),
  model = "Ours",
  dist.model = c("Ours", "blink"),
  prior = c("Ewens", "Coupon", "PY", "GenCoupon"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Add entries for baseline models
expts <- rbind(
  expts,
  expand.grid(
    data.name = c("RLdata10000", "nltcs", "cora", "rest"),
    model = c("blink", "Sadinle"),
    dist.model = "",
    prior = "",
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
)

# Add path to RDS file
expts['path'] <- {
  expts_mod <- mutate(expts,
      data.name = recode(data.name, rest = "restaurant"),
      model = recode(model, Ours = "ours", Sadinle = "sadinle0.95"),
      dist.model = recode(dist.model, blink = "blinkdist", Ours = ""),
      prior = recode(prior, Coupon = "blinkcoupon", GenCoupon = "coupon", Ewens = "ewens", PY = "py"),
    )
  prefixes <- apply(expts_mod, 1, function(row) paste(row[nzchar(row)], collapse = "_"))
  sapply(prefixes, get_result_rds)
}

# Convert Data Frame to list of lists
expts <- transpose(expts)

theme_set(theme_bw())# + theme(text = element_text(size = 8)))

trace_plot <- function(diag, fname, plt.width = 3.26, plt.height = 0.23) {
  plt <- ggplot(diag, aes(y = value, x = iteration)) + 
    facet_grid(rows = vars(name), scales = "free", as.table = FALSE, labeller = function(x) label_parsed(apply(x, 2, TeX))) + 
    geom_line() + 
    labs(y = "Value", x = "Iteration") + 
    scale_y_continuous(breaks = scales::breaks_extended(n = 3)) +
    theme(strip.text.y = element_text(angle = 0))

  ggsave(fname, plot = plt, width = plt.width, height = (1 + length(unique(diag$name))) * plt.height, units = "in", scale = 1.7)
}

trace_blink <- function(result) {
  theta <- result@history$distort_probs
  theta_names <- str_match(colnames(theta), "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  theta_names[,1] <- str_replace(theta_names[,1], "_", "\\\\_")
  colnames(theta_names) <- c("attribute", "file")
  theta_names <- paste("$\\theta_{", theta_names[,"file"], ",", theta_names[,"attribute"], "}$", sep="")
  colnames(theta) <- theta_names

  n_linked_ents <- result@history$n_linked_ents
  colnames(n_linked_ents) <- "E"

  mcpar <- attr(result@history, "mcpar")
  iteration <- do.call(seq, as.list(attr(result@history, "mcpar")))

  diag <- cbind(iteration, theta, n_linked_ents)
  diag <- as_tibble(diag)
  diag <- diag[diag$iteration %% 100 == 0,]
  pivot_longer(as_tibble(diag), !iteration)
}

trace_ours <- function(result) {
  theta <- result@history$distort_probs
  theta_names <- str_match(colnames(theta), "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  theta_names[,1] <- str_replace(theta_names[,1], "_", "\\\\_")
  colnames(theta_names) <- c("attribute", "file")
  theta_names <- paste("$\\theta_{", theta_names[,"file"], ",", theta_names[,"attribute"], "}$", sep="")
  colnames(theta) <- theta_names
  incl_vars <- list("theta" = theta)

  if ("distort_dist_concs" %in% names(result@history)) {
    rho <- result@history$distort_dist_concs 
    if (!all(is.infinite(rho))) {
      rho_names <- colnames(rho)
      rho_names <- str_replace(rho_names, "_", "\\\\_")
      rho_names <- paste("$\\rho_{", rho_names, "}$", sep="")
      colnames(rho) <- rho_names
      incl_vars[["rho"]] <- rho 
    }
  }

  if ("clust_params" %in% names(result@history)) {
    ep_params <- result@history$clust_params
    ep_params_names <- colnames(ep_params)
    ep_params_names <- str_replace_all(ep_params_names, 
                                       c("kappa" = "\\\\kappa", "sigma" = "\\\\sigma", "alpha" = "\\\\alpha"))
    ep_params_names <- paste("$", ep_params_names, "$", sep="")
    colnames(ep_params) <- ep_params_names
    incl_vars[["ep_params"]] <- ep_params
  }

  n_linked_ents <- result@history$n_linked_ents
  colnames(n_linked_ents) <- "E"
  incl_vars[["n_linked_ents"]] <- n_linked_ents

  mcpar <- attr(result@history, "mcpar")
  iteration <- do.call(seq, as.list(attr(result@history, "mcpar")))
  incl_vars[["iteration"]] <- iteration

  diag <- do.call(cbind, incl_vars)
  diag <- as_tibble(diag)
  diag <- diag[diag$iteration %% 100 == 0,]

  pivot_longer(as_tibble(diag), !iteration)
}

trace_sadinle <- function(result) {
  m <- result@history$m
  m_names <- str_match(colnames(m), "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  m_names[,1] <- str_replace(m_names[,1], "_", "\\\\_")
  colnames(m_names) <- c("attribute", "level")
  m_names <- paste("$m_{", m_names[,"attribute"], ",", m_names[,"level"], "}$", sep="")
  colnames(m) <- m_names

  u <- result@history$u
  u_names <- str_match(colnames(u), "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  u_names[,1] <- str_replace(u_names[,1], "_", "\\\\_")
  colnames(u_names) <- c("attribute", "level")
  u_names <- paste("$u_{", u_names[,"attribute"], ",", u_names[,"level"], "}$", sep="")
  colnames(u) <- u_names

  n_linked_ents <- result@history$n_clusters
  n_linked_ents <- matrix(n_linked_ents, ncol = 1)
  colnames(n_linked_ents) <- "E"

  mcpar <- attr(result@history, "mcpar")
  iteration <- do.call(seq, as.list(attr(result@history, "mcpar")))

  diag <- cbind(iteration, m, u, n_linked_ents)
  diag <- as_tibble(diag)
  diag <- diag[diag$iteration %% 100 == 0,]

  pivot_longer(as_tibble(diag), !iteration)
}

plan(sequential)
future_lapply(expts, function(expt) {
  result <- tryCatch(readRDS(expt$path), error = function(e) NULL)
  if (is.null(result)) return(NULL)
  data.name <- expt$data.name
  model <- expt$model
  dist.model <- expt$dist.model
  prior <- expt$prior

  msg <- paste("Working on experiment for dataset", data.name, "with model", model)
  if (nchar(prior) && nchar(dist.model)) {
    msg <- paste(msg, "prior", prior, "and distortion model", dist.model)
  }
  message(msg)

  diag <- switch(model,
    blink = trace_blink(result),
    Sadinle = trace_sadinle(result),
    Ours = trace_ours(result)
  )

  fname <- str_replace(expt$path, "_result\\.rds", "_trace-plot.pdf")
  trace_plot(diag, fname)
})
