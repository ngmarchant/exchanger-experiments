## This script computes the relative error in the inferred number of entities 
## (number of clusters) under four different priors on the linkage structure. 
## A point estimate of the error is computed using the posterior median, 
## as well as 95% equi-tailed credible intervals.
## 
## Outputs:
##   * `plot_err-num-ents_comparison.pdf`: A plot visualizing the results.

library(tidyverse)
library(exchanger) 
library(ggdist)          # provides geom_pointinterval
library(egg)             # provides ggarange
library(coda)            # for manipulating 'mcmc' objects
source("util.R")         # contains definition of `get_result_rds` and `true_memberships`

expts <- list(
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_coupon"), prior = "GenCoupon", dist.model = "Ours"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_coupon"), prior = "GenCoupon", dist.model = "Ours"),
  list(data.name = "cora", path = get_result_rds("cora_ours_coupon"), prior = "GenCoupon", dist.model = "Ours"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_coupon"), prior = "GenCoupon", dist.model = "Ours"),
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_py"), prior = "PY", dist.model = "Ours"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_py"), prior = "PY", dist.model = "Ours"),
  list(data.name = "cora", path = get_result_rds("cora_ours_py"), prior = "PY", dist.model = "Ours"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_py"), prior = "PY", dist.model = "Ours"),
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_ewens"), prior = "Ewens", dist.model = "Ours"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_ewens"), prior = "Ewens", dist.model = "Ours"),
  list(data.name = "cora", path = get_result_rds("cora_ours_ewens"), prior = "Ewens", dist.model = "Ours"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_ewens"), prior = "Ewens", dist.model = "Ours"),
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_blinkcoupon"), prior = "Coupon", dist.model = "Ours"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_blinkcoupon"), prior = "Coupon", dist.model = "Ours"),
  list(data.name = "cora", path = get_result_rds("cora_ours_blinkcoupon"), prior = "Coupon", dist.model = "Ours"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_blinkcoupon"), prior = "Coupon", dist.model = "Ours")
)

true.num.ents <- sapply(true_memberships, function(memb) length(unique(memb)))

theme_set(theme_bw())# + theme(text = element_text(size = 8)))

plt.width <- 0.9*6.524375
plt.height <- 0.5*2.93596875

results <- lapply(expts, function(expt) {
  result <- tryCatch(readRDS(expt$path), error = function(e) NULL)
  if (is.null(result)) return(NULL)
  data.name <- expt$data.name
  prior <- expt$prior
  num.ents <- true.num.ents[[data.name]]
  tibble(err.num.ents = 100*(as.vector(result@history$n_linked_ents) - num.ents)/num.ents, 
         data.name = data.name, 
         prior = prior)
})

results <- bind_rows(results)
results$prior <- factor(results$prior, c("PY", "Ewens", "GenCoupon", "Coupon"))
results$data.name <- factor(results$data.name, c("RLdata", "nltcs", "cora", "rest"))

results %>% 
  group_by(data.name, prior) %>% 
  point_interval(.interval = qi) %>%
  ggplot(aes(y = prior, x = err.num.ents, xmin = .lower, xmax = .upper)) + 
    facet_grid(.~data.name, scales = "free") + 
    geom_pointinterval(interval_size = 0.5, point_size = 1.0, position=position_dodge(-0.7)) +  
    scale_y_discrete(limits=rev) +
    labs(y = "Prior", x = "Relative error (%)") + 
    theme(legend.position="top", legend.margin=margin())
ggsave("plot_err-num-ents_comparison.pdf", width = plt.width, height = plt.height, units = "in", scale=1.2)
