# Experiments for "Exchangeable clustering priors for Bayesian entity resolution"

This folder contains files required to reproduce the experiments for the 
following paper:

> N. G. Marchant, B. I. P. Rubinstein and R. C. Steorts. (2021) "Bayesian 
Graphical Entity Resolution using Exchangeable Random Partition Priors".

## Data sets

Three of the four data sets are included in the `datasets` directory:
* `rest`: with filename `fz-nophone.arff.gz`. Original [source](https://www.cs.utexas.edu/users/ml/riddle/data/restaurant.tar.gz).
* `cora`: with filename `cora.arff.gz`. Modified from original [source](https://www.cs.utexas.edu/users/ml/riddle/data/cora.tar.gz) 
  to correct erroneous ground truth labels.
* `RLdata`: with filename `RLdata10000.csv.gz`. From the [RecordLinkage](https://cran.r-project.org/web/packages/RecordLinkage/) 
  R package.

`nltcs` is available from [NACDA](https://www.icpsr.umich.edu/icpsrweb/NACDA/studies/9681/summary) 
after signing a data usage agreement. 

## Dependencies

To run the experiments for our model, you must install the `exchanger` 
R package. It is hosted on [GitHub](https://github.com/cleanzr/exchanger) 
and can be installed from within R using 
`devtools::install_github("cleanzr/exchanger")`.

Similarly, to run the experiments for the model of Sadinle (2014), you 
must install the `BDD` R package. It is also hosted on 
[GitHub](https://github.com/cleanzr/BDD) and can be installed from 
within R using `devtools::install_github("cleanzr/BDD")`.

Other dependencies include the following R packages from CRAN:
* `comparator`
* `clevr`
* `tidyverse`
* `tidybayes`
* `gridExtra`
* `egg`

## R scripts

The following scripts define functions that are shared across the experiments:
* `run_ours.R`
* `run_sadinle.R`
* `util.R`

To run all of the experiments for one of the models, execute the following in 
a terminal:
```bash
$ Rscript run_<model>_all.R
```
replacing `<model>` with:

* `ours` for our model, 
* `blink` for the model by Steorts (2015), 
* `ours-blinkdist` for our model with the distortion model by Steorts (2015), or 
* `sadinle` for the model by Sadinle (2014).

To run an experiment for a particular data set and model, execute the 
following in a terminal:
```bash
Rscript run_<model>_<dataset>.R
```
where `<model>` is defined as above and `<dataset>` can be one of 
`cora`, `nltcs`, `restaurant`, or `RLdata10000`.

## Output of an experiment

Each experiment will produce several files:
* `<prefix>_result.rds`: the saved state of the model and Markov chain
* `<prefix>_eval.txt`: pairwise and clustering evaluation metrics computed 
for a point estimate.
* `<prefix>_trace-*.png`: various diagnostic plots. These vary for each 
model.

## Reproducing tables and figures

After running all experiments, tables and figures can be reproduced as follows:

* Figure 2 can be reproduced by running `plot_err-num_ents_comparison.R` 
* Table 3 and Figure S4 can be reproduced by running 
  `evaluate_prior_dist_model.R`
* Table 4 can be reproduced by running `evaluate_models.R`
* Figure 3 can be reproduced by running `plot_dist-level_comparison.R` 
* Figure S5 can be reproduced by running `plot_ep-params.R`
