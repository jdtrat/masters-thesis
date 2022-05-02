#' Read SBORG Data
#'
#' @param paths A vector of file paths containing SBORG data.
#'
#' @return A tibble with the raw behavioral output from SBORG task containing
#'   'subject', 'time', and 'data' columns.
#' @export
#'
#' @examples
#'
#' sborg_files <- fs::dir_ls("data/nonpd/", recurse = TRUE, regexp = "*.sborg.\\d+.nosessionid.*\\d.txt")
#' sborg_read_data(sborg_files)
sbg_read_data <- function(paths) {
  readr::read_tsv(
    file = paths, col_names = c("time", "data"),
    col_types = list(readr::col_character(), readr::col_character()),
    id = "subject"
  ) %>%
    dplyr::mutate(subject = stringr::str_extract(subject, "n\\d+"))
}

#' Process Raw Data
#'
#' @description Get information about choices participants made during the SBORG
#'   task. This includes the round number, options presented, choice taken,
#'   reward value, and subjective feelings rating.
#' @param raw_data The output of [sbg_read_data].
#'
#' @return The output of [sborgr::process_sborg] for each subject aggregated
#'   into a single data frame. Call `?sborgr::process_sborg` for column details.
#' @export
#'
sbg_process_raw_data <- function(raw_data) {
  raw_data %>%
    dplyr::group_split(subject) %>%
    purrr::map_df(~ {
      sborgr::process_sborg(.x, keep_time_information = FALSE) %>%
        dplyr::mutate(
          subject = unique(.x$subject),
          .before = 1
        )
    }) %>%
    # Some subjects have multiple values for a round number, here we are
    # selecting the latest values
    dplyr::distinct() %>%
    dplyr::group_by(subject, round_number) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    # Remove non-responses
    tidyr::drop_na(choice, choice_type, reward_value)
}

#' Format Processed SBORG Data for Stan
#'
#' @param processed_sborg_data The output of [sbg_process_raw_data()].
#'
#' @return A data frame with the SBORG data restructured in the format of
#'   EUT/CPUT analysis for Stan. Option 1 becomes Sure Bet, option 2 becomes
#'   gamble.
#' @export
#'
#' @examples
format_processed_sborg_data_stan <- function(processed_sborg_data) {
  processed_sborg_data %>%
    dplyr::transmute(subject = subject,
                     option1_outcome_a = sb_value,
                     option1_prob_a = 1,
                     option1_outcome_b = 0,
                     option1_prob_b = 1 - option1_prob_a,
                     option2_outcome_a = gamble_left_value,
                     option2_prob_a = 0.5,
                     option2_outcome_b = gamble_right_value,
                     option2_prob_b = 0.5,
                     chose_option1 = dplyr::case_when(choice_type == "sb" ~ 1,
                                                      TRUE ~ 0),
                     chose_option2 = dplyr::case_when(choice_type == "gamble" ~ 1,
                                                      TRUE ~ 0)
    ) %>%
    dplyr::mutate(
      dplyr::across(where(is.character) & !subject,
                    ~ as.numeric(.x))
    )
}

#' Generate a Grid of Possible Prospects from SBORG
#'
#' @description According to the specifications of SBORG generate prospects
#'   where the sure bet ranges from 1 to 6 by 1 and the gambles range from 0 to
#'   6 by 1 and cannot equal each other. The alternative outcome of option 1 is
#'   assumed to be 0 since no information is presented.
#' @return A tibble with 252 rows and eight columns representing the posible
#'   sure bet and gamble prospect specifications from SBORG.
#'
#' @export
#'
#' @examples
sbg_gen_possible_prospects <- function() {

  tidyr::expand_grid(
    option1_outcome_a = seq(1, 6, by = 1),
    option1_prob_a = 1,
    option1_outcome_b = 0,
    option1_prob_b = 0,
    option2_outcome_a = seq(0, 6, by = 1),
    option2_prob_a = 0.5,
    option2_outcome_b = seq(0, 6, by = 1),
    option2_prob_b = 0.5
  ) %>%
    dplyr::filter(
      option2_outcome_a != option2_outcome_b
    )

}


#' Estimate Gamma from SBORG Task with Metropolis Hastings Data
#'
#' @description This function estimates the posterior distribution of gamma for
#'   individual subjects using the Metropolis-Hastings algorithm defined in the
#'   function [sample_posterior_gamma()]. It is used to supplement the
#'   hierarchical Bayesian modeling performed with Stan.
#' @param formatted_sborg_data The output of
#'   [format_processed_sborg_data_stan()] on the processed SBORG Choice Data or
#'   equivalently formatted SBORG data (e.g., the output of
#'   [simulate_sbg_prospects()]).
#' @param num_iter The number of iterations to sample the Metropolis Hastings
#'   Algorithm
#' @param initial_gamma Set an initial guess for gamma. Defaults to 0.5.
#'
#' @return A list with one data frame per subject containing a 'subject' ID
#'   column (the same as SBORG Task ID) and the posterior samples of gamma for
#'   each subject.
#' @export
#'
sborg_sample_metropolis_hastings <- function(
  formatted_sborg_data,
  num_iter = 5000,
  initial_gamma = 0.5,
  tau = 1
) {
  purrr::map(
    .x = dplyr::group_split(
      formatted_sborg_data,
      subject
    ),
    .f = ~ {
      post <- sample_posterior_gamma(
        num_iter = num_iter,
        prospects = .x,
        choices = .x$chose_option1,
        initial_gamma = initial_gamma,
        tau = tau
      )
      data.frame(
        subject = unique(.x$subject),
        posterior_draws = post
      )
    }
  )
}


#' Summarize SBORG MH Posterior
#'
#' @param sborg_mh_list The list of data frames returned from
#'   [sborg_sample_metropolis_hastings()] containing the posterior draws for
#'   each subject.
#' @param credible_mass A value between zero and one representing the mass of
#'   the credible interval to recover. Defaults to `0.95`.
#'
#' @return A data frame with, for each subject, the boundaries of the highest
#'   density interval (e.g., for `0.95`, 'q2.5' and 'q97.5') from the posterior
#'   distribution including the median as 'q50'.
#' @export
#'
sbg_summarize_mh_posterior <- function(
  sborg_mh_list,
  credible_mass = 0.95
  ) {

  stopifnot(credible_mass <= 1, credible_mass > 0)

  label_hdi <- function(cred_mass) {
    if (0 < cred_mass & cred_mass <= 1) {
      cred_mass <- cred_mass * 100
    }

    lower_label <- (100 - cred_mass)/2
    upper_label <- lower_label + cred_mass

    paste0("q", c(lower_label, upper_label))
  }

  purrr::map_df(
    sborg_mh_list,
    ~ {

      post_hdi <- setNames(
        HDInterval::hdi(
          .x$posterior_draws,
          credMass = credible_mass
        ),
        label_hdi(
          cred_mass = credible_mass
        )
      )

      cbind(
        data.frame(
          subject = unique(.x$subject),
          q50 = median(.x$posterior_draws)
        ),
        as.data.frame(
          t(post_hdi)
        )
      )
    }
  )

}


#' Summarize SBORG Stan Posterior
#'
#' @param cmdstan_draws The draws method from a cmdstantr fit object.
#' @param credible_mass A value between zero and one representing the mass of
#'   the credible interval to recover. Defaults to `0.95`.
#'
#' @return A data frame with, for each subject, the boundaries of the highest
#'   density interval (e.g., for `0.95`, 'q2.5' and 'q97.5') from the posterior
#'   distribution including the median as 'q50'.
#' @export
#'
sbg_summarize_stan_posterior <- function(
  cmdstan_draws,
  credible_mass = 0.95
) {

  stopifnot(credible_mass <= 1, credible_mass > 0)

  label_hdi <- function(cred_mass) {
    if (0 < cred_mass & cred_mass <= 1) {
      cred_mass <- cred_mass * 100
    }

    lower_label <- (100 - cred_mass)/2
    upper_label <- lower_label + cred_mass

    paste0("q", c(lower_label, upper_label))
  }

  interval_labels <- label_hdi(
    cred_mass = credible_mass
  )

  initial_interval_data <- bayesplot::mcmc_intervals_data(
    x = cmdstan_draws,
    prob_outer = credible_mass,
    point_est = "median"
  )

  initial_interval_data %>%
    transmute(
      subject = as.numeric(
        str_extract(parameter, "\\d+")
      ),
      "{interval_labels[1]}" := ll,
      q50 = m,
      "{interval_labels[2]}" := hh
    )

}

#' Summarize SBORG Simulation Data
#'
#' @description This function creates a data frame with the simulated gamma
#'   values for agents on the SBORG task and the highest density interval for
#'   the posterior distribution recovered from the choice data for both
#'   Metropolis-Hastings Sampling and Hierarchical Stan Sampling.
#'
#' @param sbg_mh_list The list of data frames returned from
#'   [sborg_sample_metropolis_hastings()] containing the posterior draws for
#'   each subject.
#' @param sbg_stan_fit The cmdstantr fit object.
#' @param sbg_sim_agent_data The data frame of subjects and simulated gamma values.
#' @param credible_mass A value between zero and one representing the mass of
#'   the credible interval to recover. Defaults to `0.95`.
#'
#' @return A data frame with, for each subject, the boundaries of the highest
#'   density interval (e.g., for `0.95`, 'q2.5' and 'q97.5') from the posterior
#'   distribution including the median as 'q50' for both the Metropolis-Hastings
#'   Sampling algorithm and the Hierarchical Stan model. It is joined with the
#'   simulated subject data where each subject is a factor ordered by the
#'   simulated gamma value for ease of plotting.
#' @export
#'
#' @examples
sbg_summarize_simulation_data <- function(
  sbg_mh_list,
  sbg_stan_fit,
  sbg_sim_agent_data,
  credible_mass = 0.95
) {

  sbg_summarize_mh_posterior(
    sborg_mh_list = sbg_mh_list,
    credible_mass = 0.95
  ) %>%
    mutate(
      type = "Metropolis-Hastings Sampling"
    ) %>%
    bind_rows(
      sbg_summarize_stan_posterior(
        sbg_stan_fit$draws("gamma")
      ) %>%
        mutate(
          type = "Hamiltonian Monte Carlo (Stan) Sampling"
        )
    ) %>%
    left_join(
      sbg_sim_agent_data %>%
        rename(
          sim_gamma = gamma
        ),
      by = "subject"
    ) %>%
    mutate(
      subject = as_factor(subject) %>%
        fct_reorder(sim_gamma)
    )

}
