#' Return a Data Frame with Info about Kahneman and Tversky's Prospects from their 1979 Paper
#'
#' @return A data frame containing the questions used by Kahneman and Tversky in their 1979 paper
#' @export
#'
kt_problems <- function() {
  data.frame(
    prospect = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
    kt_problem = c(2, 3, 4, 7, 8, 3.1, 4.1, 7.1, 8.1),
    domain = c("positive", "positive", "positive", "positive", "positive", "negative", "negative", "negative", "negative"),
    significant = c("yes", "yes", "yes", "yes", "yes", "yes", "no", "yes", "yes"),
    option1_outcome_a = c(2500, 4000, 4000, 6000, 6000, -4000, -4000, -6000, -6000),
    option1_prob_a = c(0.33, 0.8, 0.2, 0.45, 0.001, 0.8, 0.2, 0.45, 0.001),
    option1_outcome_b = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    option1_prob_b = c(0.67, 0.2, 0.8, 0.55, 0.999, 0.2, 0.8, 0.55, 0.999),
    option2_outcome_a = c(2400, 3000, 3000, 3000, 3000, -3000, -3000, -3000, -3000),
    option2_prob_a = c(0.34, 1, 0.25, 0.9, 0.002, 1, 0.25, 0.9, 0.002),
    option2_outcome_b = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    option2_prob_b = c(0.66, 0, 0.75, 0.1, 0.998, 0, 0.75, 0.1, 0.998),
    chose_option1 = c(0.83, 0.2, 0.65, 0.14, 0.73, 0.92, 0.42, 0.92, 0.3),
    chose_option2 = c(0.17, 0.8, 0.35, 0.86, 0.27, 0.08, 0.58, 0.08, 0.7)
  )
}

#' Generate Subject Choices based on Proportion Data from Kahneman and Tversky
#'
#' @description This function expands the proportion of subjects that chose a
#'   given option from Kahneman and Tversky's 1979 paper into one-hundred
#'   simulated agents. The overall proportion of choices remains the same, but
#'   individual 'subjects' have a random distribution of choice behavior on the
#'   given prospects.
#'
#' @return A data frame with the prospect information (from Kahneman and
#'   Tversky's 1979 paper), and a binary variable indicating whether the
#'   'subject' chose option one or two. The overall proportion of choice
#'   behavior is the same as described in Kahneman and Tversky, but the
#'   subject-specific behavior may differ.
#' @export
#'
#' @examples
#'
#' kt_proportion_choice_data()
kt_proportion_choice_data <- function() {

  kt_problems() %>%
    nest(rep_data = -c(prospect, chose_option1)) %>%
    mutate(
      data = purrr::map2(
        .x = rep_data,
        .y = chose_option1 * 100,
        ~ {
          dplyr::slice(.x, rep(1, .y)) %>%
            dplyr::mutate(chose_option1 = 1) %>%
            dplyr::bind_rows(
              dplyr::slice(.x, rep(1, (100 - .y))) %>%
                dplyr::mutate(chose_option1 = 0)
            )
        }
      )
    ) %>%
    select(prospect, data) %>%
    unnest(cols = data) %>%
    # Mix up the order so when we artificially add subjects, they don't all
    # choose the same.
    slice_sample(
      n = 100 * nrow(kt_problems())
    ) %>%
    group_by(
      prospect
    ) %>%
    mutate(subject = row_number()) %>%
    ungroup()

}
