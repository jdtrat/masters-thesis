
#' Get Posterior Predicted Choices
#'
#' @description This function summarizes the 'pred_choice' from Stan's generated
#'   quantities block that represents the fitted models' prediction for each
#'   subjects' choice on a given prospect considering their parameter estimates.
#'   It joins this data with participants actual choices to create a data frame
#'   with, for each subject and trial, the 'chose_option1' (binary indicator)
#'   and 'pred_chose_1'.
#'
#' @param sbg_modeling_data The result of calling
#'   [format_processed_sborg_data_stan()] on the processed SBORG choice data.
#'   This is the data frame version of what is passed into the Stan model.
#' @param sbg_model_fit The fit model environment.
#'
#' @return A data frame with, for each subject and trial, the 'chose_option1'
#'   (binary indicator) and 'pred_chose_1'.
#' @export
#'
get_pp_choices <- function(
  sbg_modeling_data,
  sbg_model_fit
) {

  pred_choices <- posterior::summarize_draws(
    sbg_model_fit$draws("pred_choice"),
    c("mean")
  ) %>%
    mutate(
      choice_info = str_extract_all(
        variable,
        "\\d+"
      )
    ) %>%
    rowwise() %>%
    mutate(
      subject = as.numeric(choice_info[[1]][1]),
      trial = as.numeric(choice_info[[2]][1]),
      pred_chose_1 = case_when(
        mean < 0.5 ~ 0,
        mean >= 0.5 ~ 1
      )
    ) %>%
    ungroup() %>%
    filter(mean >= 0) %>%
    select(
      subject, trial, pred_chose_1
    )

  # Need to do this grouping by ID to ensure consistency
  # with the subject recovered from the MCMC fit
  orig_choices <- sbg_modeling_data %>%
    group_by(
      subject
    ) %>%
    mutate(
      trial = row_number(),
      subject = cur_group_id()
    ) %>%
    ungroup() %>%
    select(subject, trial, chose_option1)

  left_join(
    orig_choices,
    pred_choices,
    by = c("subject", "trial")
  )

}


#' Get Average (SD) Accuracy of Posterior Predictive Checks
#'
#' @description This function summarizes the posterior predictive choice data
#'   into a tibble with a single column, 'pp_accuracy', that represents the mean
#'   and standard deviation of correctly predicted choices for the supplied
#'   posterior predicted data.
#' @param pp_choice_data The output of [get_pp_choices()]
#'
#' @return A tibble with a single column, 'pp_accuracy' that represents the mean
#'   and standard deviation of correctly predicted choices for the supplied
#'   posterior predicted data.
#' @export
#'
#' @examples
get_avg_pp_choice_accuracy <- function(pp_choice_data) {

  pp_choice_data %>%
    rowwise() %>%
    mutate(
      correct_pred = identical(chose_option1, pred_chose_1)
    ) %>%
    ungroup() %>%
    count(subject, correct_pred) %>%
    group_by(subject) %>%
    mutate(
      percent = n / sum(n)
    ) %>%
    ungroup() %>%
    filter(correct_pred) %>%
    summarize(
      mean_correct_pred = mean(percent),
      sd_correct_pred = sd(percent)
    ) %>%
    mutate(
      pp_accuracy = glue::glue(
        "{scales::percent(mean_correct_pred, accuracy = 0.1)} ({scales::percent(sd_correct_pred, 0.1)})"
      )
    ) %>%
    select(pp_accuracy)

}

#' Generate Posterior Predictive Choice Accuracy Table
#'
#' @param ... The output of [get_avg_pp_choice_accuracy()] for all models.
#'
#' @return A data frame with two columns, 'model_name, and 'pp_accuracy' for all
#'   models.
#' @export
#'
#' @examples
#'
#' generate_pp_choice_accuracy_table(
#'   pp_choices_sborg_gamma,
#'   pp_choices_sborg_ra_gamma,
#'   pp_choices_sborg_tau_gamma,
#'   pp_choices_sborg_ra_tau_gamma,
#'   pp_choices_sborg_ra,
#'   pp_choices_sborg_ra_tau,
#'   pp_choices_sborg_tau
#'   )
#'
generate_pp_choice_accuracy_table <- function(...) {
  tibble::lst(
   ...
  ) %>%
    purrr::map_df(
      ~ get_avg_pp_choice_accuracy(.x),
      .id = "model_name"
    ) %>%
    mutate(
      model_name = str_remove(
        model_name,
        "pp_choices_"
      )
    )
}
