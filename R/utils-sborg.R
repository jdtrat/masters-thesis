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
  initial_gamma = 0.5
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
        initial_gamma = initial_gamma
      )
      data.frame(
        subject = unique(.x$subject),
        posterior_draws = post
      )
    }
  )
}
