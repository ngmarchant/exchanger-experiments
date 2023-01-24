## This script computes pairwise evaluation metrics for our model (under the 
## generalized coupon prior) + baseline models by Steorts (2015) and Sadinle 
## (2014).
## A point estimate for each metric is computed using the posterior median, 
## as well as 95% equi-tailed credible intervals.
## 
## Outputs:
##   * `evaluate_models.csv`: A CSV file containing the results
##   * `plot_models.pdf`: A plot visualizing the results

library(exchanger)
library(BDD)
library(clevr)
library(ggdist)          # provides `geom_pointinterval` and `point_interval`
library(egg)             # provides ggarange
library(coda)            # for manipulating 'mcmc' objects
library(future)
library(future.apply)    # parallelization
library(dplyr)
library(ggplot2)
library(purrr)
source("util.R")         # contains definition of `get_result_rds` and `true_memberships`

expts <- expand.grid(
  data.name = c("RLdata10000", "nltcs", "cora", "rest"),
  model = c("Ours", "blink", "Sadinle"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Add path to RDS file
expts['path'] <- {
  expts_mod <- mutate(expts,
    data.name = recode(data.name, rest = "restaurant"),
    model = recode(model, Ours = "ours_coupon", Sadinle = "sadinle0.95_"),
  )
  prefixes <- apply(expts_mod, 1, function(row) paste(row[nzchar(row)], collapse = "_"))
  sapply(prefixes, get_result_rds)
}

# Convert Data Frame to list of lists
expts <- transpose(expts)

theme_set(theme_bw())# + theme(text = element_text(size = 8)))

plt.width <- 6.524375
plt.height <- 1.63109375

plan(multisession)
results <- future_lapply(expts, function(expt) {
  result <- tryCatch(readRDS(expt$path), error = function(e) NULL)
  if (is.null(result)) return(NULL)
  data.name <- expt$data.name
  model <- expt$model
  true_membership <- true_memberships[[data.name]]
  true_pairs <- membership_to_pairs(true_membership)
  record_ids <- seq_along(true_membership) # they were defined this way in all expts
  message(paste("Working on experiment for dataset", data.name, "with model", expt$model))
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
  tibble(
    data.name = rep_len(data.name, length(measures)),
    model = rep_len(model, length(measures)),
    measure = rep_len(c("F1 score", "Precision", "Recall"), length(measures)),
    value = as.vector(measures)
  )
})

results <- bind_rows(results)

results$model <- factor(results$model, levels = c("Ours", "blink", "Sadinle"))
results$measure <- factor(results$measure, levels = c("Precision", "Recall", "F1 score"))
results$data.name <- factor(results$data.name, levels = c("RLdata", "nltcs", "cora", "rest"))

results <- results %>% 
  group_by(data.name, model, measure) %>%
  point_interval(.interval = qi) %>% 
  mutate(latex_expr = paste0("\\uncertain{", 
                             formatC(value, digits=3, format="f"), "}{", 
                             formatC(.lower, digits=3, format="f"), "}{", 
                             formatC(.upper, digits=3, format="f"), "}")) 
write_csv(results, "evaluate_models.csv")

ggplot(results, aes(y = data.name, x = value, xmin = .lower, xmax = .upper, color = model)) + 
  facet_grid(.~measure, scales = "free_x") + 
  geom_pointinterval(size = 0, position = position_dodge(-0.5)) + 
  scale_y_discrete(limits=rev) +
  labs(x = "Measure", y = "Data set", color = "Model") + 
  theme(legend.position=c("top"), legend.key.height = unit(8,"points"), legend.margin=margin(c(1,1,1,1)))
ggsave("plot_models.pdf", width = plt.width, height = plt.height, units = "in", scale = 1.2)