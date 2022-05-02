#' Calculate the Posterior Likelihood of Choosing an Option for a Given Prospect
#'
#' @description This function is adapted from [Nate Haines's
#'   post](http://haines-lab.com/post/2018-03-24-human-choice-and-reinforcement-learning-3/2018-03-24-human-choice-and-reinforcement-learning-3/)
#'    and performs Maximum a Posterior (MAP) Estimation. This this function
#'   accepts a data frame of prospects, a gamma value, and an observed (or
#'   simulated) choice. It then calculates the counterfactual utilities of a
#'   prospect for a specific gamma, and determines the probability (likelihood)
#'   that the option was selected according to a logit transformation with the
#'   function [get_cu_choice_prob()]. It then returns the sum of the logged
#'   likelihood * prior for all observed prospect/choices with a specific gamma
#'   value.
#'
#' @param prospects A data frame with the columns describing a prospect:
#'   c("option1_outcome_a", "option1_prob_a", "option1_outcome_b",
#'   "option1_prob_b", "option2_outcome_a", "option2_prob_a",
#'   "option2_outcome_b", "option2_prob_b").
#' @param gamma A gamma value for which to determine the posterior likelihood.
#' @param choice A numeric vector indicating whether option one was chosen (1)
#'   or option two was chosen (2) corresponding to the prospects.
#'
#' @return A scalar representing the sum of the logged likelihood * prior
#'   (proportional posterior) for the observed choices given specific gamma.
#' @export
#'
#' @examples
#'
#' #'
#' if (interactive()) {
#'   two_prospects <- data.frame(
#'     option1_outcome_a = c(2500, 4000),
#'     option1_prob_a = c(0.33, 0.8),
#'     option1_outcome_b = c(0, 0),
#'     option1_prob_b = c(0.67, 0.2),
#'     option2_outcome_a = c(2400, 3000),
#'     option2_prob_a = c(0.34, 1),
#'     option2_outcome_b = c(0, 0),
#'     option2_prob_b = c(0.66, 0)
#'   )
#'
#'   # Chose option 2 both times
#'   two_choices <- c(0,0)
#'
#'   # Create a data frame showing the log posterior for gamma values between zero
#'   # and one. In this case, it looks like gamma is most likely between 0.5 and 1.
#'   purrr::map_df(
#'     seq(0,1,0.1),
#'     ~ {
#'       data.frame(
#'         gamma = .x,
#'         log_posterior = calc_posterior_likelihood(
#'           prospects = two_prospects,
#'           gamma = .x,
#'           choice = two_choices
#'         )
#'       )
#'     }
#'   )
#' }
#'
calc_posterior_likelihood <- function(
  prospects,
  gamma,
  tau = 1,
  choice
  )  {

  # Sum of the log posterior distribution for a given prospect
  sum(
    vapply(
      X = 1:nrow(prospects),
      FUN = function(trial) {

        # Get the counterfactual utilities of a prospect for a specific gamma
        cus <- get_utility(
          prospect = prospects[trial,],
          gamma = gamma
        )

        # Based on a simulated choice for the specific prospect,
        # calculate the probability (likelihood) that option was selected
        likelihood <- get_softmax_choice_prob(
          utilities = cus,
          option = if (choice[trial] == 1) 1 else 2,
          tau = tau
        )

        # What is the likelihood of this gamma based on the prior distribution
        # (Uniform distribution)
        # prior <- dbeta(gamma, 2, 4)
        prior <- dunif(gamma, min = 0, max = 1)

        #Take the log of posterior (proportional to likelihood * prior)
        log(likelihood * prior)

      },
      numeric(1)
    )
  )

}

#' Sample the Posterior Distribution for CPUT's Gamma Parameter
#'
#' @description This function is adapted from [Nate Haines's
#'   post](http://haines-lab.com/post/2018-03-24-human-choice-and-reinforcement-learning-3/2018-03-24-human-choice-and-reinforcement-learning-3/)
#'    and lecture material by Staci Hepler at Wake Forest University. It
#'   determines the posterior distribution of gamma based on the observed
#'   prospects and choices using the Metropolis-Hastings algorithm.
#' @param num_iter The number of iterations to sample the Metropolis Hastings
#'   Algorithm
#' @inheritParams calc_posterior_likelihood
#' @param initial_gamma Set an initial guess for gamma. Defaults to 0.5.
#'
#' @return A numeric vector of length `num_iter` that corresponds to the
#'   posterior distribution of gamma.
#' @export
#'
#' @examples
#'
#'
#'
#'    if (interactive()) {
#'
#'      two_prospects <- data.frame(
#'        option1_outcome_a = c(2500, 4000),
#'        option1_prob_a = c(0.33, 0.8),
#'        option1_outcome_b = c(0, 0),
#'        option1_prob_b = c(0.67, 0.2),
#'        option2_outcome_a = c(2400, 3000),
#'        option2_prob_a = c(0.34, 1),
#'        option2_outcome_b = c(0, 0),
#'        option2_prob_b = c(0.66, 0)
#'      )
#'
#'      # Chose option 2 both times
#'      two_choices <- c(0,0)
#'
#'      gamma_post <- sample_posterior_gamma(
#'        num_iter = 5000,
#'        prospects = two_prospects,
#'        choices = two_choices,
#'        initial_gamma = 0.5
#'      )
#'
#'      # Preview posterior density
#'      ggplot2::qplot(
#'        gamma_post,
#'        geom = "density",
#'        fill = I("aliceblue")
#'        ) +
#'        ggplot2::xlim(0,1)
#'
#'    }
#'
sample_posterior_gamma <- function(
  num_iter, prospects,
  choices, initial_gamma = 0.5,
  tau = 1
  ) {

  gamma_curr <- initial_gamma

  foreach::foreach(n = 1:num_iter, .combine = "c") %do% {

    # Draw a candidate from the proposal distribution (uniform 0,1)
    gamma_proposal <- runif(1, min = 0, max = 1)

    # Compute the accpetance ratio -- the posterior evaluated at the candidate
    # value of gamma divided by the posterior evaluated at the current estimate of
    # gamma

    lk_prop <- calc_posterior_likelihood(
      prospects = prospects,
      gamma = gamma_proposal,
      choice = choices,
      tau = tau
    )

    lk_curr <- calc_posterior_likelihood(
      prospects = prospects,
      gamma = gamma_curr,
      choice = choices,
      tau = tau
    )

    acceptance <- exp(lk_prop) / exp(lk_curr)

    # Sample from a uniform distribution and make a decision to accept or reject
    # the proposal gamma value.
    # If accpetance is NA 0 / 0, accept proposal
    if (is.na(acceptance) || runif(1, min = 0, max = 1) <= acceptance) {
      gamma_curr <- gamma_proposal
    }

    # Return the current estimate of gamma
    gamma_curr

  }

}
