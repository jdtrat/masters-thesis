#' Generate an Option
#'
#' @description Create a Prospect's Option
#'
#' @param outcome_a The primary outcome of choosing this option.
#' @param prob_a The primary probability of receiving `outcome_a` when choosing
#'   this option.
#' @param outcome_b The alternative outcome of choosing this option. Defaults to
#'   zero.
#' @param prob_b The probability of receiving `outcome_b` when choosing this
#'   option (defaults to `1 - prob_a`).
#'
#' @return A list object of with special class "option" that contains the this
#'   option's outcome/probability structure.
#' @export
#'
#' @examples
#'
#' gen_option(4000, 0.8)
#' gen_option(3000, 1)
#'
gen_option <- function(
  outcome_a, prob_a,
  outcome_b = 0, prob_b = (1 - prob_a)
  ) {

  if (!is.null(prob_b) && !all.equal((1 - prob_a), prob_b)) {
    prob_b <- 1 - prob_a
    cli::cli_alert_info(
      "{.arg prob_b} ({prob_b}) is not equal to {.code 1 - prob_a} ({1 - prob_a}).
      Setting {.arg prob_b} as {.val {prob_b}}"
      )
  }

  out <- list(
    outcome_a = outcome_a,
    prob_a = prob_a,
    outcome_b = outcome_b,
    prob_b = prob_b
  )

  class(out) <- c("list", "option")

  out
}

#' Generate Prospects
#'
#' @description Generate a single row of class `c("data.frame", "prospect")`
#'   that contains the option magnitude and probabilities for an individual
#'   prospect.
#'
#' @param option_1 A list with elements c("outcome_a", "prob_a", "outcome_b",
#'   "prob_b") to be included as a prospect's first option. Ideally, this is
#'   created with [gen_option()].
#' @param option_2 A list with elements c("outcome_a", "prob_a", "outcome_b",
#'   "prob_b") to be included as a prospect's second option. Ideally, this is
#'   created with [gen_option()].
#'
#' @return A data frame with names c("option1_outcome_a", "option1_prob_a",
#'   "option1_outcome_b", "option1_prob_b", "option2_outcome_a",
#'   "option2_prob_a", "option2_outcome_b", "option2_prob_b") representing a
#'   prospect.
#' @export
#'
#' @examples
#'
#' gen_prospct(
#'   option_1 = gen_option(4000, 0.8),
#'   option_2 = gen_option(3000, 1)
#' )
#'
gen_prospct <- function(option_1, option_2) {

  if (!inherits(option_1, "option") & inherits(option_2, "option")) {
    cli::cli_alert_info(
      "Generated prospects are best done on objects of class {.cls option} created with {.fn gen_option}."
      )
  }

  out <- cbind(
    as.data.frame(
      setNames(option_1, paste0("option1_", names(option_1)))
    ),
    as.data.frame(
      setNames(option_2, paste0("option2_", names(option_2)))
    )
  )

  class(out) <- c("data.frame", "prospect")

  out
}

#' Get the Counterfactual Utility of a Prospect
#'
#' @param prospect A data frame with columns c("option1_outcome_a",
#'   "option1_prob_a", "option1_outcome_b", "option1_prob_b",
#'   "option2_outcome_a", "option2_prob_a", "option2_outcome_b",
#'   "option2_prob_b"). Ideally, this is created from [gen_prospect()].
#' @param gamma A gamma value from which to determine this prospect's
#'   counterfactual utilities.
#'
#' @return A list with special class "CUs" that have the counterfactual utility
#'   for both options ("option_1" and "option_2")
#' @export
#'
#' @examples
#'
#' # Define a prospect (this is the third problem from Kahneman and Tversky's 1979 paper)
#' # Determine the counterfactual utilities of the prospect for different gamma values:
#' kt_problem_3 <- gen_prospct(
#'   option_1 = gen_option(4000, 0.8),
#'   option_2 = gen_option(3000, 1)
#' )
#'
#' # When gamma is equal to zero, this is the same as the expected utility
#' # (expected value)
#' get_cu(
#'   prospect = kt_problem_3,
#'   gamma = 0
#' )
#'
#' get_cu(
#'   prospect = kt_problem_3,
#'   gamma = 0.5
#' )
#'
#'
#'
get_cu <- function(prospect, gamma) {

  # x is shorter than prospect, makes reading this prettier (:
  x <- prospect

  option1_eu <- ((x$option1_prob_a * x$option1_outcome_a) + (x$option1_prob_b * x$option1_outcome_b))
  option2_eu <- ((x$option2_prob_a * x$option2_outcome_a) + (x$option2_prob_b * x$option2_outcome_b))

  x1 <- x$option1_outcome_a - (gamma * (x$option1_outcome_b + option2_eu))
  x2 <- x$option1_outcome_b - (gamma * (x$option1_outcome_a + option2_eu))
  x3 <- x$option2_outcome_b - (gamma * (x$option2_outcome_a + option1_eu))
  x4 <- x$option2_outcome_a - (gamma * (x$option2_outcome_b + option1_eu))

  option1_cu <- ((x$option1_prob_a * x1) + (x$option1_prob_b * x2))
  option2_cu <- ((x$option2_prob_a * x4) + (x$option2_prob_b * x3))

  out <- list(
    option_1 = option1_cu,
    option_2 = option2_cu
  )

  class(out) <- c("list", "CUs")

  out

}

#' Get the Probability of a Choice
#'
#' @description This function accepts a list (ideally generated with
#'   [get_cu_choice_prob()]) that has the counterfactual utilities for
#'   "option_1" and "option_2". It then implements a logit (softmax)
#'   transformation to translate the difference between utility values into the
#'   probability of choosing one option over another. By default, it will return
#'   the probability of choosing option one, however, this can be switched by
#'   specifying `option = "2"`. The parameter `epsilon` modulates the
#'   determinism of choices, which we set to one as default.
#'
#' @param utilities A list with elements "option_1" and "option_2" that
#'   represent the counterfactual utilities for a prospect's options. Ideally,
#'   this is generated with [get_cu_choice_prob()].
#' @param option A character "1" or "2" representing for which option we
#'   calculate the probability of choosing.
#' @param epsilon A temperature parameter in the logit transformation
#'   representing the determinism of a choice.
#'
#' @return A scalar representing the probability of choosing the specified option
#'   given the calculated utilities.
#' @export
#'
#' @examples


#' # Define a prospect (this is the third problem from Kahneman and Tversky's 1979 paper)
#' # Determine the counterfactual utilities of the prospect for different gamma values:
#' kt_problem_3 <- gen_prospct(
#'   option_1 = gen_option(4000, 0.8),
#'   option_2 = gen_option(3000, 1)
#' )
#'
#' # When gamma is equal to zero, this is the same as the expected utility
#' # (expected value)
#' expected_utilities <- get_cu(
#'   prospect = kt_problem_3,
#'   gamma = 0
#'   )
#'
#' counterfactual_utilities <- get_cu(
#'   prospect = kt_problem_3,
#'   gamma = 0.5
#' )
#'
#'
#' # The probability of choosing option one when gamma is zero (expected utilities)
#' # is very, very high
#' get_cu_choice_prob(
#'   utilities = expected_utilities,
#'   option = "1",
#'   epsilon = 1
#' )
#'
#' # The probability of choosing option one when gamma is 0.5 (counterfactual utilities)
#' # is very, very low
#' get_cu_choice_prob(
#'   utilities = counterfactual_utilities,
#'   option = "1",
#'   epsilon = 1
#' )
#'
#' # The probability of choosing option two, however, when gamma is 0.5 (counterfactual utilities)
#' # is very, very high
#' get_cu_choice_prob(
#'   utilities = counterfactual_utilities,
#'   option = "2",
#'   epsilon = 1
#' )
#'
get_cu_choice_prob <- function(utilities,
                               option = c("1", "2"),
                               epsilon = 1) {


  if (!inherits(utilities, "CUs")) {
    cli::cli_alert_danger(
      "Choice Probabilities are best found with objects of class {.cls option} created with {.fn get_cu}."
    )
  }

  desired <- paste0("option_", match.arg(option))

  alternative <- paste0("option_",
                        switch(
                          match.arg(option),
                          "1" = "2",
                          "2" = "1"
                          )
                        )

  value_diff <- utilities[[desired]] - utilities[[alternative]]

  1 / (1 + exp(-epsilon * value_diff))

}
