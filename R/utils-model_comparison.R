
#' Convert cmdstanr output to RStan fit
#'
#' @description This function accepts the output CSV files from a fit cmdstanr
#'   model (accessible with `$output_files()` method) and converts it into an
#'   RStan fit. This is necessary for performing model comparison with
#'   [bridgesampling::bridge_sampler()]. See [this GitHub
#'   issue](https://github.com/quentingronau/bridgesampling/issues/27) for
#'   context.
#'
#' @param output_files A character vector of CSV files output from a fit
#'   cmdstanr model.
#'
#' @return An 'stanfit' object
#' @export
#'
cmdstanr_to_rstan <- function(output_files) {
  rstan::read_stan_csv(
    csvfiles = output_files
  )
}

#' Perform Bridge Sampling on a Model
#'
#' @param .model_fit An object of type "stanfit"; a fit model using
#'   [rstan::stan] or `run_stan_model()`.
#' @param stan_data The output of `format_raw_stan_data()`.
#' @param stan_file Path to the Stan file -- should either be TDRL or VPRL code
#'   for the IAPS choice task.
#' @param maxiter Number of maximum iterations for bridgesampling.
#' @param reps Number of repetitions for bridgesampling.
#' @param silent Should progress of bridgesampling be printed to console?
#' @param ... Additional parameters passed to [bridgesampling::bridge_sampler]
#'
#' @return A list with element "bridge", corresponding to the output of
#'   `[bridgesampling::bridge_sampler], and element "error" corresponding to the
#'   output of [bridgesampling::error_measures] of the bridge element.
#' @export
#'
get_bridge_error <- function(.model_fit, stan_data, stan_file, maxiter = 10000, reps = 10, silent = TRUE, ...) {

  set.seed(18)

  model <- rstan::stan(model_code = readLines(stan_file),
                       data = stan_data,
                       chains = 0)

  cli::cli_text("Beginning Bridge Sampling")
  bridge <- bridgesampling::bridge_sampler(samples = .model_fit,
                                           stanfit_model = model,
                                           data = ff_stan_data,
                                           maxiter = maxiter,
                                           repetitions = reps,
                                           silent = silent,
                                           ...)

  errors <- bridgesampling::error_measures(bridge_object = bridge)

  list(bridge_samples = bridge,
       error = errors)
}

#' Summarize Marginal Likelihood Model Evidence
#'
#' @description This function summarizes the marginal likelihood model evidence
#'   for the supplied models based on the bridgesampling performed in
#'   [get_bridge_error()].
#' @param ... The output of [get_bridge_error()] for all models to be compared.
#'   This function assumes that the objects pass in will be prefixed with
#'   'bridge_error_' as part of the targets pipeline and will remove that
#'   character string as applicable from the model name.
#'
#' @return A tibble with the model names and the model evidence.
#' @export
#'
#' @examples
#'
#' summarize_marginal_likelihood_model_evidence(
#'   bridge_error_sborg_gamma,
#'   bridge_error_sborg_ra,
#'   bridge_error_sborg_ra_gamma,
#'   bridge_error_sborg_ra_tau,
#'   bridge_error_sborg_ra_tau_gamma,
#'   bridge_error_sborg_tau_gamma,
#'   bridge_error_sborg_tau
#' )
#'
summarize_marginal_likelihood_model_evidence <- function(...) {

  tibble::lst(
    ...
  ) %>%
    purrr::map_df(~{
      evidence <- .x[["bridge_samples"]][["logml"]]
      tibble(
        model_evidence = paste0(round(median(evidence), 1), " (", round(IQR(evidence), 2), ")")
      )
    },
    .id = "model_name"
    ) %>%
    mutate(
      model_name = str_remove(
        model_name,
        "bridge_error_"
      )
    )
}

#' Summarize Penalized Model Fit with LOO-CV and LOOIC
#'
#' @param ... Any number of cmdstanr model fit environments from which to
#'   summarize the penalized model fit. This function assumes that the model
#'   environments begin with the prefix 'sbg_fit_mcmc' as part of the targets
#'   pipeline and will remove those as applicable.
#'
#' @return A tibble with the model names and the columns 'elpd' and 'looic'
#'   evidence.
#' @export
#'
#' @examples
#'
#' summarize_elpd_looic_evidence(
#'   sbg_fit_mcmc_sborg_gamma,
#'   sbg_fit_mcmc_sborg_ra_gamma,
#'   sbg_fit_mcmc_sborg_tau_gamma,
#'   sbg_fit_mcmc_sborg_ra_tau_gamma,
#'   sbg_fit_mcmc_sborg_ra,
#'   sbg_fit_mcmc_sborg_ra_tau,
#'   sbg_fit_mcmc_sborg_tau
#' )
#'
summarize_elpd_looic_evidence <- function(...) {

  tibble::lst(
    ...
   ) %>%
    purrr::map_df( ~ {
      loo <- .x$loo()
      elpd <- loo$estimates["elpd_loo", ]
      looic <- loo$estimates["looic", ]

      tibble::tibble(
        elpd = paste0(round(elpd["Estimate"], 1), " (", round(elpd["SE"], 1), ")"),
        looic = paste0(round(looic["Estimate"], 1), " (", round(looic["SE"], 1), ")")
      )
    },
    .id = "model_name"
    ) %>%
    mutate(
      model_name = str_remove(
        model_name,
        "sbg_fit_mcmc_"
      )
    )

}
