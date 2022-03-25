library(targets)
library(tarchetypes)
library(stantargets)

source("R/utils-bayesian-sampling.R")
source("R/utils-cput.R")
source("R/utils-kt.R")
source("R/utils-sborg.R")
source("R/utils-stan_data.R")
source("R/utils-simulation.R")
source("R/utils-tar_bookdown.R")

# Set target-specific options such as packages.
tar_option_set(
  packages = c("tidyverse", "fs", "qs", "here",
               "foreach")
  )

# Search for Priors -----------------------------------------------------

kt <- list(
  tar_target(
    kt_proportional_choice_data,
    kt_proportion_choice_data(
      seed = 18,
      num_subjects = 100
    ),
    format = "qs"
  ),
  tar_target(
    kt_posterior,
    sample_posterior_gamma(
      num_iter = 10000,
      prospects = kt_proportional_choice_data,
      choices = kt_proportional_choice_data$chose_option1,
      initial_gamma = 0.5
    ),
    format = "qs"
  )
)

sborg_parameter_recovery <- list(
  tar_target(
    sbg_sim_agents,
    {
      set.seed(1)
      data.frame(
        subject = 1:50,
        # Simulate gamma from a beta distribution that best fits the posterior
        # from KT Metropolis Hastings
        gamma = rbeta(50, 1.1, 1.1)
      )
    }
  ),
  tar_target(
    sbg_sim_choice_data,
    simulate_sbg_prospects(
      agents = sbg_sim_agents
    ),
    format = "qs"
  ),
  tar_target(
    sbg_sim_metropolis_hastings_list,
    sborg_sample_metropolis_hastings(
      formatted_sborg_data = sbg_sim_choice_data,
      num_iter = 5000,
      initial_gamma = 0.5
    ),
    format = "qs"
  ),
  tar_stan_mcmc(
    sbg_sim_fit,
    stan_files = c(
      "stan-files/cput_gamma_reparam_bernoulli_logit.stan"
    ),
    data = preprocess_choice_data(
      sbg_sim_choice_data
    ),
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 5000
  )
)


# SBORG Task Data ------------------------------------------------

sborg_task_data <- list(
  tar_target(
    sborg_files,
    fs::dir_ls("data/nonpd/",
      recurse = TRUE,
      regexp = "*.sborg.\\d+.nosessionid.*\\d.txt"
    ),
    format = "file"
  ),
  tar_target(
    sborg_raw,
    sbg_read_data(sborg_files),
    format = "qs"
  ),
  tar_target(
    sborg_processed,
    sbg_process_raw_data(sborg_raw),
    format = "qs"
  )
)

# SBORG Parameter Estimation ------------------------------------------

sborg_parameter_estimation <- list(
  tar_target(
    sbg_estimation_data,
    format_processed_sborg_data_stan(
      processed_sborg_data = sborg_processed
    ),
    format = "qs"
  ),
  tar_target(
    sbg_metropolis_hastings_list,
    sborg_sample_metropolis_hastings(
      formatted_sborg_data = sbg_estimation_data,
      num_iter = 5000,
      initial_gamma = 0.5
    ),
    format = "qs"
  ),
  tar_stan_mcmc(
    sbg_fit,
    stan_files = c(
      "stan-files/cput_gamma_reparam_bernoulli_logit.stan"
    ),
    data = preprocess_choice_data(
      format_processed_sborg_data_stan(
        sborg_processed
      )
    ),
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 5000
  )
)

# Thesis Documents --------------------------------------------------------

thesis_document <- list(
  tar_render_book(
    thesis,
    path = "thesis-documents",
    output_format = "all",
    output_dir = "_thesis",
    new_session = TRUE
    )
)

# Return All Targets ------------------------------------------------------

list(
  kt,
  sborg_parameter_recovery,
  sborg_task_data,
  sborg_parameter_estimation,
  thesis_document
)
