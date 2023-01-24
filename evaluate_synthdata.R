## This script computes pairwise evaluation metrics for the synthetic dataset. 
## A point estimate for each metric is computed using the posterior median, 
## as well as 95% equi-tailed credible intervals.
## 
## Outputs:
##   * `evaluate_synthdata.csv`: A CSV file containing the results
##   * `plot_synthdata.pdf`: A plot visualizing the results
##   * `plot_link-conf.pdf`: A plot showing the prior distribution of cluster 
##        sizes as a function of mu

library(exchanger)
library(BDD)
library(clevr)           # pairwise evaluation measures
library(ggdist)          # provides geom_pointinterval
library(coda)            # for manipulating 'mcmc' objects
library(future)
library(future.apply)    # parallelization
library(dplyr)
library(ggplot2)
library(purrr)
source("util.R")         # contains definition of "get_result_rds"

setwd("./datasets")
source("load_synthdata.R")
setwd("../")

expts <- expand.grid(
  link_conf_mu = c(0.1, 1, 8, 100),
  dist_conf = 0:1,
  model = c("blink", "sadinle0.95", "ours_coupon"),
  exp_num_recs = c(1000, 10000),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Add path to RDS file
expts['path'] <- {
  expts_mod <- mutate(expts,
    prefix = paste0("synthdata_link-conf-mu-", link_conf_mu, "_dist-conf-", dist_conf, "_seed-0_exp-num-recs-", exp_num_recs, "_", model)
  )
  sapply(expts_mod$prefix, get_result_rds)
}

# Convert Data Frame to list of lists
expts <- transpose(expts)

theme_set(theme_bw())# + theme(text = element_text(size = 8)))

plot.width <- 16
plot.height <- 18

plan(multisession)
results <- future_lapply(expts, function(e) {
  model <- e$model
  seed <- 0
  link_conf_mu <- e$link_conf_mu
  dist_conf <- e$dist_conf
  exp_num_recs <- e$exp_num_recs
  sel_idx <- which(file.synthdata$link_conf_mu == link_conf_mu & file.synthdata$dist_conf == dist_conf & file.synthdata$seed == seed & file.synthdata$exp_num_recs == exp_num_recs)

  true_membership <- identity.synthdata[sel_idx]

  result <- tryCatch(readRDS(e$path), error = function(...) NULL)
  if (is.null(result)) return(NULL)

  true_pairs <- membership_to_pairs(true_membership)
  record_ids <- seq_along(true_membership) # they were defined this way in all expts

  msg <- paste0(
    "Working on experiment for synthdata with link_conf_mu=", link_conf_mu,
    " dist_conf=", dist_conf, " exp_num_recs=", exp_num_recs, " and model=",
    model
  )
  message(msg)

  if (inherits(result, "ExchangERFit")) {
    links <- result@history$links
  } else if (inherits(result, "BDDFit")) {
    links <- BDD::complete_links_samples(result, all_rec_ids = record_ids)
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
  tibble(link_conf_mu = rep_len(link_conf_mu, length(measures)), 
         dist_conf = rep_len(dist_conf, length(measures)), 
         exp_num_recs = rep_len(exp_num_recs, length(measures)), 
         model = rep_len(model, length(measures)), 
         measure = rep_len(c("F1 score", "Precision", "Recall"), length(measures)),
         value = as.vector(measures))
}, future.seed = TRUE)

results <- bind_rows(results)

results$model <- factor(
  results$model,
  levels = c("ours_coupon", "blink", "sadinle0.95"),
  labels = c("Ours", "blink", "Sadinle")
)
results$measure <- factor(
  results$measure,
  levels = c("Precision", "Recall", "F1 score")
)
results$link_conf_mu <- factor(
  results$link_conf_mu,
  levels = c(0.1, 1, 8, 100),
  labels = c("L. Dup.", "M. Dup.", "H. Dup.", "V. H. Dup.")
)
results$dist_conf <- factor(
  results$dist_conf,
  levels = 0:1,
  labels = c("L. Dist.", "H. Dist.")
)
results$exp_num_recs <- factor(
  results$exp_num_recs, 
  c(1000, 10000)
)

results <- results %>% 
  group_by(link_conf_mu, dist_conf, model, exp_num_recs, measure) %>%
  point_interval(.interval = qi) %>% 
  mutate(latex_expr = paste0("\\uncertain{", 
                             formatC(value, digits=3, format="f"), "}{", 
                             formatC(.lower, digits=3, format="f"), "}{", 
                             formatC(.upper, digits=3, format="f"), "}")) 
write_csv(results, "evaluate_synthdata.csv")

ggplot(results, aes(y = value, x = model, ymin = .lower, ymax = .upper, color=exp_num_recs, shape=exp_num_recs)) + 
  facet_wrap(link_conf_mu + dist_conf ~ measure, scales = "free_y", ncol=3, strip.position = "right") +
  geom_pointinterval(size = 0, position = position_dodge(0.5)) +
  labs(x = "Model", y = "Measure value", color = "Exp. number of records", shape = "Exp. number of records") + 
  theme(legend.position="top", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plot_synthdata.pdf", width = plot.width, height = plot.height, units = "cm")

link_conf_mu <- c("Low (μ=0.1)" = 0.1, "Medium  (μ=1)" = 1, "High  (μ=8)" = 8, "Very high  (μ=100)" = 100)
temp <- lapply(link_conf_mu, function(mu) {
  k <- 1:4
  pmf <- dpois(k, mu)
  pmf <- pmf / sum(pmf)
  tibble(k = k,
         pmf = pmf,
         mu = rep_len(mu, length(pmf)))
})
temp <- bind_rows(temp)
temp$mu <- factor(temp$mu, levels = link_conf_mu, labels = names(link_conf_mu))
ggplot(temp, aes(y = pmf, x = k)) + 
  facet_wrap(. ~ mu, scales = "free_y", ncol = length(link_conf_mu)) +
  geom_point() +
  labs(x = "Records per entity (cluster size)", y = "Relative frequency") + 
  theme(legend.position="top")
ggsave("plot_link-conf.pdf", width = plot.width, height = 0.25*plot.height, units = "cm", device = cairo_pdf)
