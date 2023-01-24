## This script computes the posterior level of distortion in each attribute
## under two distortion models: Ours and blink (Steorts, 2015).
## A point estimate of the distortion level is computed using the posterior 
## median, as well as 95% equi-tailed credible intervals.
## 
## Outputs:
##   * `plot_dist-level_comparison.pdf`: A plot visualizing the results.

library(tidyverse)
library(exchanger) 
library(ggdist)          # provides geom_pointinterval
library(egg)             # provides ggarange
library(coda)            # for manipulating 'mcmc' objects
source("util.R")         # contains definition of `get_result_rds`

expts <- list(
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_blinkdist_coupon"), prior = "GenCoupon", dist.model = "blink"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_blinkdist_coupon"), prior = "GenCoupon", dist.model = "blink"),
  list(data.name = "cora", path = get_result_rds("cora_ours_blinkdist_coupon"), prior = "GenCoupon", dist.model = "blink"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_blinkdist_coupon"), prior = "GenCoupon", dist.model = "blink"),
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_coupon"), prior = "GenCoupon", dist.model = "Ours"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_coupon"), prior = "GenCoupon", dist.model = "Ours"),
  list(data.name = "cora", path = get_result_rds("cora_ours_coupon"), prior = "GenCoupon", dist.model = "Ours"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_coupon"), prior = "GenCoupon", dist.model = "Ours")
)

theme_set(theme_bw())# + theme(text = element_text(size = 8)))

plt.width <- 3.2621875
plt.height <- 0.9 * plt.width

results <- lapply(expts, function(expt) {
  result <- tryCatch(readRDS(expt$path), error = function(e) NULL)
  if (is.null(result)) return(NULL)

  data.name <- expt$data.name
  prior <- expt$prior
  dist.model <- expt$dist.model
  distort_counts <- result@history$distort_counts
  n_records <- length(result@state@rec_ids)

  # Remove file identifier, since there's only one file
  colnames(distort_counts) <- tolower(colnames(distort_counts))
  colnames(distort_counts) <- gsub('.{3}$', '', colnames(distort_counts))

  as_tibble(100 * distort_counts/ n_records) %>% 
    gather(key = "attribute.name", value = "perc.distortion") %>% 
    mutate(data.name = data.name, prior = prior, dist.model = dist.model)
})

results <- bind_rows(results)
results$dist.model <- factor(results$dist.model, c("Ours", "blink"))
results$data.name <- factor(results$data.name, c("RLdata", "nltcs", "cora", "rest"))

results %>% 
  group_by(data.name, prior, attribute.name, dist.model) %>% 
  point_interval(.interval = qi) %>%
  ggplot(aes(y = attribute.name, x = perc.distortion, xmin = .lower, xmax = .upper, color=dist.model, shape=dist.model)) + 
    facet_grid(data.name~., scales = "free_y") + 
    geom_pointinterval(interval_size = 0.5, point_size = 1.0, position=position_dodge(-0.5)) +
    labs(y = "Attribute", x = "Distortion level (%)", color = "Distortion model", shape = "Distortion model") + 
    theme(legend.position="top", legend.key.height = unit(10, "points"), legend.margin=margin())
ggsave("plot_dist-level_comparison.pdf", width = plt.width, height = plt.height, scale=1.4)
