## This script computes pairwise evaluation metrics for our model under 4 
## linkage structure priors and 2 distortion models (8 variants in total). 
## A point estimate for each metric is computed using the posterior median, 
## as well as 95% equi-tailed credible intervals.
## 
## Outputs:
##   * `evaluate_prior_dist_model.csv`: A CSV file containing the results
##   * `plot_prior_dist_model.pdf`: A plot visualizing the results

library(tidyverse)
library(exchanger)
library(clevr)           # pairwise evaluation measures
library(ggdist)          # provides geom_pointinterval
library(coda)            # for manipulating 'mcmc' objects
library(future)
library(future.apply)    # parallelization
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
  list(data.name = "rest", path = get_result_rds("restaurant_ours_blinkcoupon"), prior = "Coupon", dist.model = "Ours"),
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_blinkdist_coupon"), prior = "GenCoupon", dist.model = "blink"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_blinkdist_coupon"), prior = "GenCoupon", dist.model = "blink"),
  list(data.name = "cora", path = get_result_rds("cora_ours_blinkdist_coupon"), prior = "GenCoupon", dist.model = "blink"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_blinkdist_coupon"), prior = "GenCoupon", dist.model = "blink"),
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_blinkdist_py"), prior = "PY", dist.model = "blink"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_blinkdist_py"), prior = "PY", dist.model = "blink"),
  list(data.name = "cora", path = get_result_rds("cora_ours_blinkdist_py"), prior = "PY", dist.model = "blink"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_blinkdist_py"), prior = "PY", dist.model = "blink"),
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_blinkdist_ewens"), prior = "Ewens", dist.model = "blink"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_blinkdist_ewens"), prior = "Ewens", dist.model = "blink"),
  list(data.name = "cora", path = get_result_rds("cora_ours_blinkdist_ewens"), prior = "Ewens", dist.model = "blink"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_blinkdist_ewens"), prior = "Ewens", dist.model = "blink"),
  list(data.name = "RLdata", path = get_result_rds("RLdata10000_ours_blinkdist_blinkcoupon"), prior = "Coupon", dist.model = "blink"),
  list(data.name = "nltcs", path = get_result_rds("nltcs_ours_blinkdist_blinkcoupon"), prior = "Coupon", dist.model = "blink"),
  list(data.name = "cora", path = get_result_rds("cora_ours_blinkdist_blinkcoupon"), prior = "Coupon", dist.model = "blink"),
  list(data.name = "rest", path = get_result_rds("restaurant_ours_blinkdist_blinkcoupon"), prior = "Coupon", dist.model = "blink")
)

theme_set(theme_bw())# + theme(text = element_text(size = 8)))

plot.width <- 14.5
plot.height <- 16

plan(multisession)
results <- future_lapply(expts, function(expt) {
  result <- tryCatch(readRDS(expt$path), error = function(e) NULL)
  if (is.null(result)) return(NULL)
  data.name <- expt$data.name
  prior <- expt$prior
  dist.model <- expt$dist.model
  true_membership <- true_memberships[[data.name]]
  true_pairs <- membership_to_pairs(true_membership)
  record_ids <- seq_along(true_membership) # they were defined this way in all expts
  message(paste("Working on experiment for dataset", data.name, "with prior", prior, "and distortion model", dist.model))
  if (inherits(result, "ExchangERFit")) {
    links <- result@history$links
  } else {
    stop("result is of unrecognized type")
  }
  
  # Evaluate 100 samples from the chain. Don't use entire chain, as it takes too long
  sample_idx <- sample.int(nrow(links), size = 100, replace = FALSE)
  measures <- apply(links[sample_idx,], 1, function(pred_membership) {
    pred_pairs <- membership_to_pairs(pred_membership)
    c(
      f1_score = f_measure_pairs(true_pairs, pred_pairs),
      precision = precision_pairs(true_pairs, pred_pairs),
      recall = recall_pairs(true_pairs, pred_pairs)
    )
  })
  tibble(data.name = rep_len(data.name, length(measures)), 
         prior = rep_len(prior, length(measures)), 
         dist.model = rep_len(dist.model, length(measures)), 
         measure = rep_len(c("F1 score", "Precision", "Recall"), length(measures)), 
         value = as.vector(measures))
}, future.seed = TRUE)

results <- bind_rows(results)

results$dist.model <- factor(results$dist.model, levels = c("Ours", "blink"))
results$prior <- factor(results$prior, levels = c("PY", "Ewens", "GenCoupon", "Coupon"))
results$measure <- factor(results$measure, levels = c("Precision", "Recall", "F1 score"))
results$data.name <- factor(results$data.name, levels = c("RLdata", "nltcs", "cora", "rest"))

results <- results %>% 
  group_by(data.name, prior, dist.model, measure) %>%
  point_interval(.interval = qi) %>% 
  mutate(latex_expr = paste0("\\uncertain{", 
                             formatC(value, digits=3, format="f"), "}{", 
                             formatC(.lower, digits=3, format="f"), "}{", 
                             formatC(.upper, digits=3, format="f"), "}")) 
write_csv(results, "evaluate_prior_dist_model.csv")

ggplot(results, aes(y = value, x = prior, ymin = .lower, ymax = .upper, color = dist.model, shape = dist.model)) + 
  facet_wrap(data.name~measure, scales = "free_y", nrow = 4, ncol = 3, strip.position = "top") +
  geom_pointinterval(size = 0, position = position_dodge(0.5)) + 
  #scale_y_discrete(limits=rev) +
  scale_y_continuous(expand=expansion(mult=c(0.2,0.2))) + 
  labs(x = "Linkage prior", y = "Measure value", color = "Distortion model", shape = "Distortion model") + 
  theme(legend.position="top", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plot_prior_dist_model.pdf", width = plot.width, height = plot.height, units = "cm")