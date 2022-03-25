
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
