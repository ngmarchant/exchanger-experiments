## This script generates Geweke diagnostics for:
##   * Our model under two distortion models and four linkage structure priors 
##   * the blink model by Steorts (2015)
##   * the model by Sadinle (2014)
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
library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
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

geweke_plot <- function(diag, fname, plt.width = 3.26, plt.height = 0.23) {
  plt <- ggplot(diag, aes(x=variable, weight=geweke)) + 
    geom_bar() + 
    geom_hline(yintercept=c(-2,2), linetype="dotted") +
    coord_flip() + 
    scale_x_discrete(labels = TeX) +
    labs(y = "Geweke diagnostic", x = "Variable")

  ggsave(fname, plot = plt, width = plt.width, height = (1 + nrow(diag)) * plt.height, units = "in", scale = 1.7)
}

geweke_blink <- function(result) {
  theta <- geweke.diag(result@history$distort_probs)$z
  theta_names <- str_match(names(theta), "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  theta_names[,1] <- str_replace(theta_names[,1], "_", "\\\\_")
  colnames(theta_names) <- c("attribute", "file")
  theta_names <- paste("$\\theta_{", theta_names[,"file"], ",", theta_names[,"attribute"], "}$", sep="")

  n_linked_ents <- geweke.diag(result@history$n_linked_ents)$z

  data.frame(variable = c(theta_names, "$E$"),
             geweke = c(theta, n_linked_ents))
}

geweke_ours <- function(result) {
  theta <- geweke.diag(result@history$distort_probs)$z
  theta_names <- str_match(names(theta), "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  theta_names[,1] <- str_replace(theta_names[,1], "_", "\\\\_")
  colnames(theta_names) <- c("attribute", "file")
  theta_names <- paste("$\\theta_{", theta_names[,"file"], ",", theta_names[,"attribute"], "}$", sep="")

  finite_rho <- which(apply(is.finite(result@history$distort_dist_concs), 2, all))
  if (length(finite_rho)) {
    rho <- geweke.diag(result@history$distort_dist_concs[,finite_rho])$z
    rho_names <- names(rho)
    rho_names <- str_replace(rho_names, "_", "\\\\_")
    rho_names <- paste("$\\rho_{", rho_names, "}$", sep="")
  } else {
    rho <- numeric()
    rho_names <- character()
  }

  if ("clust_params" %in% names(result@history)) {
    ep_params <- geweke.diag(result@history$clust_params)$z
    ep_params_names <- names(ep_params)
    ep_params_names <- str_replace_all(ep_params_names, 
                                       c("kappa" = "\\\\kappa", "sigma" = "\\\\sigma", "alpha" = "\\\\alpha"))
    ep_params_names <- paste("$", ep_params_names, "$", sep="")
  } else {
    ep_params <- numeric()
    ep_params_names <- character()
  }

  n_linked_ents <- geweke.diag(result@history$n_linked_ents)$z

  data.frame(variable = c(theta_names, rho_names, ep_params_names, "$E$"),
             geweke = c(theta, rho, ep_params, n_linked_ents))
}

geweke_sadinle <- function(result) {
  m <- geweke.diag(result@history$m)$z
  m_names <- str_match(names(m), "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  m_names[,1] <- str_replace(m_names[,1], "_", "\\\\_")
  colnames(m_names) <- c("attribute", "level")
  m_names <- paste("$m_{", m_names[,"attribute"], ",", m_names[,"level"], "}$", sep="")

  u <- geweke.diag(result@history$u)$z
  u_names <- str_match(names(u), "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  u_names[,1] <- str_replace(u_names[,1], "_", "\\\\_")
  colnames(u_names) <- c("attribute", "level")
  u_names <- paste("$u_{", u_names[,"attribute"], ",", u_names[,"level"], "}$", sep="")

  n_linked_ents <- geweke.diag(matrix(result@history$n_clusters))$z

  data.frame(variable = c(m_names, u_names, "$E$"),
             geweke = c(m, u, n_linked_ents))
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
    blink = geweke_blink(result),
    Sadinle = geweke_sadinle(result),
    Ours = geweke_ours(result)
  )
  
  fname <- str_replace(expt$path, "_result\\.rds", "_geweke-plot.pdf")
  geweke_plot(diag, fname)
})
