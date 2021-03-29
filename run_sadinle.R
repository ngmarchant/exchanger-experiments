source("util.R")         # Contains definition of clusterMetrics function
library(clevr)
library(exchanger)
library(BDD)       # Mauricio Sadinle's model

#' Run an experiment using Mauricio Sadinle's model
#' 
#' TODO Document side effects
#' 
#' @param expt_name string used to identify the experiment
#' @param model a `SadinleModel` object representing the initial state 
#'   of model parameters
#' @param rec_ids 
#' @param true_membership ground truth clustering expressed as a membership 
#'   vector
#' @param n_samples number of approximate posterior samples to generate 
#'   (after thinning and burn-in)
#' @param burnin_interval TODO 
#' @param thin_interval TODO
run_sadinle <- function(expt_name, model, rec_ids, true_membership, 
                        n_samples, burnin_interval, thin_interval = 10, 
                        plotWidth = NA, plotHeight = NA) {
  message("Experiment has prefix: ", expt_name)
  
  # Run inference
  fit <- BDD::run_inference(model, n_samples = n_samples, 
                            burnin_interval = burnin_interval, 
                            thin_interval = thin_interval)
  saveRDS(fit, file = paste0("results/", expt_name, "_result.rds"))
  links_samples <- complete_links_samples(fit, rec_ids)
  
  # TODO: check that this works
  n_records <- nrow(records)
  
  # Generate a point estimate and convert to different representations
  pred_clust <- smp_clusters(links_samples)
  pred_matches <- clusters_to_pairs(pred_clust)
  pred_membership <- clusters_to_membership(pred_clust, elem_ids = rec_ids)
  
  # Convert ground truth clustering to a pairwise representation
  true_matches <- membership_to_pairs(true_membership, elem_ids = rec_ids)
  
  # Compute evaluation metrics and save to disk
  sink(paste0("results/", expt_name, "_eval.txt"))
  print(eval_report_pairs(true_matches, pred_matches, num_pairs=n_records*(n_records-1)/2))
  print(eval_report_clusters(true_membership, pred_membership))
  sink()
  
  pop_size <- apply(links_samples, 1, function(x) length(unique(x)))
  m_probs <- BDD::extract(fit, "m")
  u_probs <- BDD::extract(fit, "u")
  
  # Various diagnostics plots
  # TODO: it no longer exits
  it <- attr(fit@history, 'mcpar')
  it <- seq.int(from=it[1],to=it[2],by=it[3])
  bind_cols(iteration = it,
            n_linked_ents = pop_size) %>%
    ggplot(aes(x = iteration, y = n_linked_ents)) + geom_line() +
    labs(x = "Iteration", y = "# linked entities")
  ggsave(paste0("results/", expt_name, "_trace-num-entities.pdf"), width = plotWidth, height = plotHeight)
  
  bind_cols(as_tibble(m_probs), iteration = it) %>%
    gather(key = "key", value = "value", -iteration) %>% 
    tidyr::extract(key, into = c("attribute", "level"), regex = "(\\w+)\\[(\\d+)\\]$") %>% 
    ggplot(aes(x = iteration, y = value)) + 
    facet_grid(attribute~., scales = "free_y") + 
    geom_line(aes(color = level)) + 
    labs(x = "Iteration", y = "m", color = "Level")
  ggsave(paste0("results/", expt_name, "_trace-m-star.pdf"), width = plotWidth, height = plotHeight)
  
  bind_cols(as_tibble(u_probs), iteration = it) %>%
    gather(key = "key", value = "value", -iteration) %>% 
    tidyr::extract(key, into = c("attribute", "level"), regex = "(\\w+)\\[(\\d+)\\]$") %>% 
    ggplot(aes(x = iteration, y = value)) + 
    facet_grid(attribute~., scales = "free_y") + 
    geom_line(aes(color = level)) + 
    labs(x = "Iteration", y = "u", color = "Level")
  ggsave(paste0("results/", expt_name, "_trace-u-star.pdf"), width = plotWidth, height = plotHeight)
}
