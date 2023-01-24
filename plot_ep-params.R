## This script computes the posterior Ewens-Pitman parameters for the three 
## linkage structure priors where they are not fixed a priori.
## A point estimate for each parameter is computed using the posterior median, 
## as well as 95% equi-tailed credible intervals.
## 
## Outputs:
##   * `plot_ep-params.pdf`: A plot visualizing the results.

library(tidyverse)
library(exchanger) 
library(ggdist)          # provides geom_pointinterval
library(egg)             # provides ggarange
library(coda)            # for manipulating 'mcmc' objects
source("util.R")         # contains definition of `get_result_rds`

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
  list(data.name = "rest", path = get_result_rds("restaurant_ours_ewens"), prior = "Ewens", dist.model = "Ours")
)

theme_set(theme_bw())# + theme(text = element_text(size = 8)))

plt.width <- 0.9*6.524375
plt.height <- 0.9*2.93596875

log_breaks = function(maj, radix=10, nminor=radix) {
  function(x) {
    minx         = floor(min(logb(x,radix), na.rm=T)) - 1
    maxx         = ceiling(max(logb(x,radix), na.rm=T)) + 1
    n_major      = maxx - minx + 1
    major_breaks = seq(minx, maxx, by=1)
    if (maj) {
      breaks = major_breaks
    } else {
      steps_lin <- seq(from=0, to=radix, length.out = nminor + 1)
      steps_lin <- tail(steps_lin, -1)
      steps = logb(steps_lin,radix)
      breaks = rep(steps, times=n_major) +
        rep(major_breaks, each=length(steps_lin))
    }
    radix^breaks
  }
}

results <- lapply(expts, function(expt) {
  result <- tryCatch(readRDS(expt$path), error = function(e) NULL)
  if (is.null(result)) return(NULL)

  data.name <- expt$data.name
  prior <- expt$prior

  as_tibble(result@history$clust_params) %>% 
    gather(key = "clust.param", value = "value") %>% 
    mutate(data.name = data.name, 
           prior = prior, 
           clust.param = str_replace(clust.param, "d", "sigma"))
})

results <- bind_rows(results)
results$data.name <- factor(results$data.name, c("RLdata", "nltcs", "cora", "rest"))

coupon.plt <- results %>% 
  filter(prior == 'GenCoupon') %>%
  group_by(data.name, clust.param, prior) %>%
  point_interval(value, .interval = qi) %>%
  ggplot(aes(x = data.name, y = value, ymin = .lower, ymax = .upper)) + 
  facet_grid(cols = vars(prior), rows = vars(clust.param), scales = "free") + 
  geom_pointinterval(scale=1, interval_size_range = c(0.2, 1.2), position=position_dodge(width = 0.9), fatten_point = 1.4) + 
  scale_y_continuous(trans = 'log10', breaks = log_breaks(TRUE), minor_breaks = log_breaks(FALSE, nminor=5)) +
  labs(x = NULL, y = NULL) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ewens.plt <- results %>% 
  filter(prior == 'Ewens') %>%
  group_by(data.name, clust.param, prior) %>%
  point_interval(value, .interval = qi) %>%
  ggplot(aes(x = data.name, y = value, ymin = .lower, ymax = .upper)) + 
  facet_grid(cols = vars(prior), rows = vars(clust.param), scales = "free") + 
  geom_pointinterval(scale=1, interval_size_range = c(0.2, 1.2), position=position_dodge(width = 0.9), fatten_point = 1.4) + 
  scale_y_continuous(trans = 'log10', breaks = log_breaks(TRUE), minor_breaks = log_breaks(FALSE, nminor=5)) +
  labs(x = "Data set", y = NULL) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

py.plt <- results %>% 
  filter(prior == 'PY') %>%
  group_by(data.name, clust.param, prior) %>%
  point_interval(value, .interval = qi) %>%
  ggplot(aes(x = data.name, y = value, ymin = .lower, ymax = .upper)) + 
  facet_grid(cols = vars(prior), rows = vars(clust.param), scales = "free") + 
  geom_pointinterval(scale=1, interval_size_range = c(0.2, 1.2), position=position_dodge(width = 0.9), fatten_point = 1.4) + 
  scale_y_continuous(trans = 'log10', minor_breaks = log_breaks(FALSE, nminor=2)) +
  labs(x = NULL, y = NULL) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g <- ggarrange(py.plt, ewens.plt, coupon.plt, ncol=3)
ggsave("plot_ep-params.pdf", plot = g, width = plt.width, height = plt.height, units = "in", scale = 1.2)
