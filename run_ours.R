source("util.R")         # Contains definition of clusterMetrics function
library(exchanger)  # Our model
library(clevr)

#' Run an experiment using our model
#' 
#' TODO Document side effects
#' 
#' @param expt_name string used to identify the experiment
#' @param model an `ExchangERModel` model object representing the initial state 
#'   of model parameters
#' @param true_membership ground truth clustering expressed as a membership 
#'   vector
#' @param n_samples number of approximate posterior samples to generate 
#'   (after thinning and burn-in)
#' @param burnin_interval TODO 
#' @param thin_interval TODO
run_ours <- function(expt_name, model, true_membership, 
                     n_samples, burnin_interval, thin_interval = 10, 
                     plotWidth = NA, plotHeight = NA) {
  message("Experiment has prefix: ", expt_name)
  
  # Run inference
  result <- run_inference(model, n_samples, burnin_interval = burnin_interval, 
                          thin_interval = thin_interval)
  saveRDS(result, file = paste0("results/", expt_name, "_result.rds"))
  
  rec_ids <- result@state@rec_ids
  n_records <- length(rec_ids)
  
  # Generate a point estimate and convert to different representations
  pred_clust <- smp_clusters(result)
  pred_matches <- clusters_to_pairs(pred_clust)
  pred_membership <- clusters_to_membership(pred_clust, elem_ids=rec_ids)
  
  # Convert ground truth clustering to a pairwise representation
  true_matches <- membership_to_pairs(true_membership, elem_ids=rec_ids)
  
  # Compute evaluation metrics and save to disk
  sink(paste0("results/", expt_name, "_eval.txt"))
  print(eval_report_pairs(true_matches, pred_matches, num_pairs=n_records*(n_records-1)/2))
  print(eval_report_clusters(true_membership, pred_membership))
  sink()
  
  # Various diagnostic plots
  it <- attr(result@history, 'mcpar')
  it <- seq.int(from=it[1],to=it[2],by=it[3])
  bind_cols(iteration = it, 
            n_linked_ents = result@history$n_linked_ents) %>% 
    ggplot(aes(x = iteration, y = n_linked_ents)) + geom_line() + 
    labs(x = "Iteration", y = "# linked entities")
  ggsave(filename=paste0("results/", expt_name, "-trace-num-entities.pdf"), 
         height = plotHeight, width = plotWidth)
  
  bind_cols(iteration = it, 
            as_tibble(result@history$distort_counts/n_records*100)) %>% 
    gather(key = "attribute", value="percDistorted", -iteration) %>% 
    ggplot(aes(x = iteration, y = percDistorted)) + 
    geom_line(aes(colour = attribute, linetype = attribute)) + 
    labs(x = "Iteration", y = "% distorted", col = "Attribute", linetype = "Attribute")
  ggsave(filename=paste0("results/", expt_name, "-trace-distortion.pdf"), 
         height = plotHeight, width = plotWidth)
  
  if (hasName(result@history, "clust_params")) {
    bind_cols(iteration = it,
              as_tibble(result@history$clust_params)) %>%
      gather(key = "clust.param", value="value", -iteration) %>% 
      ggplot(aes(x=iteration, y=value)) + 
      geom_line() + facet_grid(clust.param~., scales = "free_y") + 
      xlab("Iteration") + ylab("Value") + 
      ggtitle("Trace plot: clustering hyperparameters")
    ggsave(filename=paste0("results/", expt_name, "-trace-clust-params.pdf"), 
           height = plotHeight, width = plotWidth)
  }
  
  # Posterior distribution over number of unique entities
  qplot(result@history$n_linked_ents, geom = "histogram") + 
    xlab("Population size") + ylab("Frequency")
  ggsave(filename=paste0("results/", expt_name, "-hist-num-entities.pdf"), 
         height = plotHeight, width = plotWidth)
}
