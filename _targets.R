library(targets)
library(tarchetypes)
library(stantargets)

source("R/utils-bayesian-sampling.R")
source("R/utils-cput.R")
source("R/utils-kt.R")
source("R/utils-model_comparison.R")
source("R/utils-modeling-figures.R")
source("R/utils-model-tables.R")
source("R/utils-pp-checks.R")
source("R/utils-render-prospect-diagram.R")
source("R/utils-sborg.R")
source("R/utils-stan_data.R")
source("R/utils-simulation.R")
source("R/utils-tar_bookdown.R")

# Set target-specific options such as packages.
tar_option_set(
  packages = c("tidyverse", "fs", "qs", "here", "flextable",
               "ggtext", "cowplot", "foreach")
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
      "stan-files/sborg_gamma.stan"
    ),
    data = preprocess_choice_data(
      sbg_sim_choice_data
    ),
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 5000
  ),
  tar_target(
    sbg_sim_posterior_summary_data,
    sbg_summarize_simulation_data(
      sbg_mh_list = sbg_sim_metropolis_hastings_list,
      sbg_stan_fit = sbg_sim_fit_mcmc_sborg_gamma,
      sbg_sim_agent_data = sbg_sim_agents,
      credible_mass = 0.95
    )
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
      "stan-files/sborg_gamma.stan",
      "stan-files/sborg_ra.stan",
      "stan-files/sborg_ra_tau.stan",
      "stan-files/sborg_ra_gamma.stan",
      "stan-files/sborg_ra_tau_gamma.stan",
      "stan-files/sborg_tau_gamma.stan",
      "stan-files/sborg_tau.stan"
    ),
    data = preprocess_choice_data(
      format_processed_sborg_data_stan(
        sborg_processed
      )
    ),
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 5000,
    output_dir = "stan-fit-csv/"
    )
  )


sborg_model_comparison <- list(
  tar_map(
    unlist = TRUE,
    tar_target(
      sbg_stanfit,
      cmdstanr_to_rstan(
        output_files = cmdstan_fit$output_files()
      )
    ),
    tar_target(
      bridge_error,
      get_bridge_error(
        .model_fit = sbg_stanfit,
        stan_data = preprocess_choice_data(
          format_processed_sborg_data_stan(
            sborg_processed
          )
        ),
        stan_file = stan_file,
        maxiter = 10000,
        reps = 10,
      )
    ),
    tar_target(
      pp_choices,
      get_pp_choices(
        sbg_modeling_data = format_processed_sborg_data_stan(
          sborg_processed
          ),
        sbg_model_fit = cmdstan_fit
      ),
      format = "qs"
    ),
    values = tibble::tibble(
      stan_file = c(
        "stan-files/sborg_gamma.stan",
        "stan-files/sborg_ra.stan",
        "stan-files/sborg_ra_tau.stan",
        "stan-files/sborg_ra_gamma.stan",
        "stan-files/sborg_ra_tau_gamma.stan",
        "stan-files/sborg_tau_gamma.stan",
        "stan-files/sborg_tau.stan"
        ),
      stan_name = stantargets:::produce_stan_names(
        stan_files = stan_file
        ),
      cmdstan_fit = rlang::syms(
        paste0(
          "sbg_fit_mcmc_", stan_name
        )
      )
    ),
    names = stan_name
  )
)

# Thesis Documents --------------------------------------------------------

thesis_figures <- list(
  tar_target(
    prospect_diagram_kt_file,
    render_prospect_figure(
      rmd_doc = "thesis-documents/latex_to_html/prospect_diagram_kt.Rmd",
      image_output_path = "thesis-documents/figures/prospect_diagram_kt.png"
    ),
    format = "file"
  ),
  tar_target(
    prospect_diagram_file,
    render_prospect_figure(
      rmd_doc = "thesis-documents/latex_to_html/prospect_diagram.Rmd",
      image_output_path = "thesis-documents/figures/prospect_diagram_general.png"
    ),
    format = "file"
  ),
  tar_target(
    sbg_demo_figure,
    generate_sbg_demo_prospect_figure(),
    format = "qs"
  ),
  tar_target(
    sbg_demo_figure_file, {
      ggsave(
        plot = sbg_demo_figure,
        filename = "thesis-documents/figures/sbg_demo_figure.png",
        width = 1.619 * 5,
        height = 5,
        dpi = 300,
        bg = "white"
      )
      # Return file path
      "thesis-documents/figures/sbg_demo_figure.png"
    },
    format = "file"
  ),
  tar_target(
    sbg_demo_softmax_figure,
    generate_sbg_demo_softmax_figure(),
    format = "qs"
  ),
  tar_target(
    sbg_demo_softmax_figure_file, {
      ggsave(
        plot = sbg_demo_softmax_figure,
        filename = "thesis-documents/figures/sbg_demo_softmax_figure.png",
        width = 1.619 * 5,
        height = 5,
        dpi = 300,
        bg = "white"
      )
      # Return file path
      "thesis-documents/figures/sbg_demo_softmax_figure.png"
    },
    format = "file"
  ),
  tar_target(
    kt_post_approx_figure,
    generate_kt_post_approximation_plot(
      kt_posterior_draws = kt_posterior
    ),
    format = "qs"
  ),
  tar_target(
    kt_post_approx_figure_file, {
      ggsave(
        plot = kt_post_approx_figure,
        filename = "thesis-documents/figures/kt_post_approx_figure.png",
        width = 1.619 * 6,
        height = 6,
        dpi = 300,
        bg = "white"
      )
      # Return file path
      "thesis-documents/figures/kt_post_approx_figure.png"
    },
    format = "file"
  ),
  tar_target(
    sbg_sim_parameter_recovery_figure,
    generate_sbg_sim_parameter_recovery_figure(
      sbg_sim_posterior_summary_data = sbg_sim_posterior_summary_data
    ),
    format = "qs"
  ),
  tar_target(
    sbg_sim_parameter_recovery_figure_file, {
      ggsave(
        plot = sbg_sim_parameter_recovery_figure,
        filename = "thesis-documents/figures/sbg_sim_parameter_recovery_figure.png",
        width = 1.619 * 5,
        height = 8,
        dpi = 300,
        bg = "white"
      )
      # Return file path
      "thesis-documents/figures/sbg_sim_parameter_recovery_figure.png"
    },
    format = "file"
  ),
  tar_target(
    model_definition_table,
    generate_model_flextable(),
    format = "qs"
  ),
  tar_target(
    pp_choice_accuracy_data,
    generate_pp_choice_accuracy_table(
      pp_choices_sborg_gamma,
      pp_choices_sborg_ra_gamma,
      pp_choices_sborg_tau_gamma,
      pp_choices_sborg_ra_tau_gamma,
      pp_choices_sborg_ra,
      pp_choices_sborg_ra_tau,
      pp_choices_sborg_tau
      ),
    format = "qs"
  ),
  tar_target(
    marginal_likelihood_model_evidence_data,
    summarize_marginal_likelihood_model_evidence(
      bridge_error_sborg_gamma,
      bridge_error_sborg_ra,
      bridge_error_sborg_ra_gamma,
      bridge_error_sborg_ra_tau,
      bridge_error_sborg_ra_tau_gamma,
      bridge_error_sborg_tau_gamma,
      bridge_error_sborg_tau
    ),
    format = "qs"
  ),
  tar_target(
    elpd_looic_model_evidence_data,
    summarize_elpd_looic_evidence(
      sbg_fit_mcmc_sborg_gamma,
      sbg_fit_mcmc_sborg_ra_gamma,
      sbg_fit_mcmc_sborg_tau_gamma,
      sbg_fit_mcmc_sborg_ra_tau_gamma,
      sbg_fit_mcmc_sborg_ra,
      sbg_fit_mcmc_sborg_ra_tau,
      sbg_fit_mcmc_sborg_tau
    ),
    format = "qs"
  ),
  tar_target(
    model_definition_table_file, {
      flextable::save_as_image(
        x = model_definition_table,
        expand = 25,
        zoom = 1.5,
        path = "thesis-documents/figures/model_definition_table.png"
      )
      # Return file path
      "thesis-documents/figures/model_definition_table.png"
    },
    format = "file"
  ),
  tar_target(
    population_posterior_plot,
    generate_population_posterior_plot(
      model_fit_list = tibble::lst(
        sbg_fit_mcmc_sborg_gamma,
        sbg_fit_mcmc_sborg_ra_gamma,
        sbg_fit_mcmc_sborg_tau_gamma,
        sbg_fit_mcmc_sborg_ra_tau_gamma,
        sbg_fit_mcmc_sborg_ra,
        sbg_fit_mcmc_sborg_ra_tau,
        sbg_fit_mcmc_sborg_tau
      )
    ),
    format = "qs"
  ),
  tar_target(
    population_posterior_plot_file,
    {
      ggsave(
        plot = population_posterior_plot,
        filename = "thesis-documents/figures/population_posterior_plot.png",
        width = 1.619 * 6,
        height = 12,
        dpi = 300,
        bg = "white"
      )
      # Return file path
      "thesis-documents/figures/population_posterior_plot.png"
    },
    format = "file"
  ),
  tar_target(
    model_fit_summary_table,
    generate_model_fit_summary_table(
      pp_choice_accuracy_data = pp_choice_accuracy_data,
      elpd_looic_model_evidence_data = elpd_looic_model_evidence_data,
      marginal_likelihood_model_evidence_data = marginal_likelihood_model_evidence_data
    ),
    format = "qs"
  ),
  tar_target(
    model_fit_summary_table_file, {
      flextable::save_as_image(
        x = model_fit_summary_table,
        expand = 25,
        zoom = 1.5,
        path = "thesis-documents/figures/model_fit_summary_table.png"
      )
      # Return file path
      "thesis-documents/figures/model_fit_summary_table.png"
    },
    format = "file"
  ),
  tar_target(
    log_bayes_factor_tau_gamma_over_ra_tau,
    bridgesampling::bf(
      bridge_error_sborg_tau_gamma$bridge_samples,
      bridge_error_sborg_ra_tau$bridge_samples,
      log = TRUE
    ),
    format = "qs"
  ),
  tar_target(
    cput_softmax_mu_gamma_hdi,
    {
      HDInterval::hdi(
        posterior::extract_variable(
          targets::tar_read(sbg_fit_mcmc_sborg_tau_gamma)$draws("mu_gamma"),
          "mu_gamma"
        )
      )
    },
    format = "qs"
  )
)


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
  sborg_model_comparison,
  thesis_figures,
  thesis_document
)
