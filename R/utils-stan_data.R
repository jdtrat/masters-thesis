#' Preprocess Choice Data for Stan
#'
#' @description Adapted from
#'   [hBayesDM](https://github.com/CCS-Lab/hBayesDM/blob/cf9a08f227cde90545943ba0e5be23eae3bd8d13/R/R/preprocess_funcs.R#L346)
#'    for dealing with choice data where each subject may have answered
#'   different numbers of prospects.
#'
#' @param choice_data A data frame with simulated or actual choice data for
#'   which to pass into Stan to for estimating CPUT parameters. Must contain
#'   columns "option1_outcome_a", "option1_prob_a", "option2_outcome_a",
#'   "option2_prob_a", "option1_outcome_b", "option1_prob_b",
#'   "option2_outcome_b", "option2_prob_b", "subject", and "chose_option1"
#'   (binary one if true, zero otherwise).
#'
#' @return A list with choice data for each subject processed in a manner usable
#'   by the Stan files for estimating CPUT parameters.
#' @export
#'
#' @examples
#'
#'  if (interactive()) {
#'    one_prospect_per_subj <- data.frame(
#'      subject = c(1, 2),
#'      option1_outcome_a = c(2500, 4000),
#'      option1_prob_a = c(0.33, 0.8),
#'      option1_outcome_b = c(0, 0),
#'      option1_prob_b = c(0.67, 0.2),
#'      option2_outcome_a = c(2400, 3000),
#'      option2_prob_a = c(0.34, 1),
#'      option2_outcome_b = c(0, 0),
#'      option2_prob_b = c(0.66, 0),
#'      chose_option1 = c(0, 0)
#'    )
#'
#'    preprocess_choice_data(
#'      choice_data = one_prospect_per_subj
#'    )
#'
#'  }
#'
#'
preprocess_choice_data <- function(choice_data) {

  subjects <- unique(choice_data$subject)
  num_subjects <- length(subjects)
  prospects_per_subj <- dplyr::count(choice_data, subject)$n
  max_prospects_across_subj <- max(prospects_per_subj)

  x1 <- array(0, c(num_subjects, max_prospects_across_subj))
  x2 <- array(0, c(num_subjects, max_prospects_across_subj))
  x4 <- array(0, c(num_subjects, max_prospects_across_subj))
  x3 <- array(0, c(num_subjects, max_prospects_across_subj))
  p1 <- array(0, c(num_subjects, max_prospects_across_subj))
  not_p1 <- array(0, c(num_subjects, max_prospects_across_subj))
  p2 <- array(0, c(num_subjects, max_prospects_across_subj))
  not_p2 <- array(0, c(num_subjects, max_prospects_across_subj))
  chose_option1 <- array(-1, c(num_subjects, max_prospects_across_subj))


  for (i in 1:num_subjects) {
    subj <- subjects[i]
    props <- prospects_per_subj[i]

    subj_choice_data <- choice_data[choice_data$subject == subj,]

    x1[i, 1:props] <- subj_choice_data$option1_outcome_a
    x2[i, 1:props] <- subj_choice_data$option1_outcome_b
    x4[i, 1:props] <- subj_choice_data$option2_outcome_a
    x3[i, 1:props] <- subj_choice_data$option2_outcome_b
    p1[i, 1:props] <- subj_choice_data$option1_prob_a
    not_p1[i, 1:props] <- subj_choice_data$option1_prob_b
    p2[i, 1:props] <- subj_choice_data$option2_prob_a
    not_p2[i, 1:props] <- subj_choice_data$option2_prob_b
    chose_option1[i, 1:props] <- subj_choice_data$chose_option1
  }

  data_list <- list(
    num_subjects = num_subjects,
    max_prospects = max_prospects_across_subj,
    prospects_per_subj = prospects_per_subj,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4,
    p1 = p1,
    not_p1 = not_p1,
    p2 = p2,
    not_p2 = not_p2,
    chose_option1 = chose_option1
  )

  return(data_list)
}
