## This script generates a plot of the posterior m-probabilities for the
## baseline model by Sadinle (2014).
##
## Output:
##   plot_m_sadinle.pdf`

library(exchanger)
library(BDD)
library(coda)            # for manipulating 'mcmc' objects and `geweke.diag`
library(future)
library(future.apply)    # parallelization
library(latex2exp)       # for variable names in plot
library(ggdist)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)
source("util.R")         # contains definition of `get_result_rds`

expts <- expand.grid(
  data.name = c("RLdata10000", "nltcs", "cora", "rest"),
  lambda = c(0, 0.5, 0.85, 0.95),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Add path to RDS file
expts['path'] <- {
  expts_mod <- transmute(expts,
      data.name = recode(data.name, rest = "restaurant"),
      model = paste0("sadinle", ifelse(lambda == 0.5, "", lambda)),
    )
  prefixes <- apply(expts_mod, 1, function(row) paste0(paste(row[nzchar(row)], collapse = "_"), "_"))
  sapply(prefixes, get_result_rds)
}

# Convert Data Frame to list of lists
expts <- transpose(expts)

plot.width <- 14.5
plot.height <- 16

theme_set(theme_bw())# + theme(text = element_text(size = 8)))

m_u_sadinle <- function(result) {
  m <- as.data.frame(result@history$m) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    point_interval(.interval = qi)
  m_names <- str_match(m$name, "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  colnames(m_names) <- c("attribute", "level")
  m <- bind_cols(m, m_names, variable = "m")

  u <- as.data.frame(result@history$u) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    point_interval(.interval = qi)
  u_names <- str_match(u$name, "([a-zA-Z_0-9]+)\\[([0-9])\\]")[,2:3]
  colnames(u_names) <- c("attribute", "level")
  u <- bind_cols(u, u_names, variable = "u")

  bind_rows(m, u)
}

results <- lapply(expts, function(expt) {
  result <- tryCatch(readRDS(expt$path), error = function(e) NULL)
  if (is.null(result)) return(NULL)
  data.name <- expt$data.name

  message(paste("Working on experiment for dataset", data.name, "with lambda", expt$lambda))

  m_u <- m_u_sadinle(result)
  m_u$data.name <- expt$data.name
  m_u$lambda <- paste0("λ = ", expt$lambda)
  m_u
})

results <- bind_rows(results)

results %>% 
  filter(variable == "m") %>%
  ggplot(aes(y = attribute, x = value, xmin = .lower, xmax = .upper, color = level, shape = level)) + 
  facet_grid(data.name~lambda, scales = "free") + 
  geom_pointinterval(size=0.8, position = position_dodge(-0.5)) +
  scale_y_discrete(limits=rev) +
  labs(x = expression(paste(italic("m"), " value")), y = "Attribute", color = "Agreement level", shape = "Agreement level") +
  theme(legend.position=c("top"), legend.key.height = unit(8,"points"), legend.margin=margin(c(1,1,1,1)), 
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plot_m_sadinle.pdf", width = plot.width, height = plot.height, units = "cm", device = cairo_pdf)
