#' Simulate Choices on KT Prospects
#'
#' @param agents A data frame with columns 'subject' and 'gamma' from which to
#'   simulate choices on Kahneman and Tverky's prospects.
#'
#' @return A data frame with, for each agent's gamma, the prospect information
#'   (from Kahneman and Tversky's 1979 paper), the counterfactual utilities of
#'   options one and two ('cu_1', 'cu_2'), the probability of choosing option
#'   one ('prob_1'), and a simulated choice given `rbinom` ('chose_option_1').
#' @export
#'
#' @examples
#'
#' simulate_kt_prospects(
#'   agents = data.frame(
#'     subject = 1:2,
#'     gamma = c(0, 0.5)
#'   )
#' )
#'
simulate_kt_prospects <- function(
  agents
  ) {

  kt_prospects <- kt_problems() %>%
    dplyr::select(
      dplyr::starts_with(c("prospect", "option"))
      )

  purrr::pmap_df(
    expand.grid(.x = 1:nrow(agents),
                .y = 1:nrow(kt_prospects)
                ),
    ~ {

    cus <- get_cu(
      prospect = kt_prospects[.y,],
      gamma = agents[.x,]$gamma
      )

    prob_1 <- get_cu_choice_prob(
      utilities = cus,
      option = "1",
      epsilon = 1
      )

    dplyr::bind_cols(
      kt_prospects[.y,],
      tibble::tibble(
        subject = agents[.x, ]$subject,
        gamma = agents[.x, ]$gamma,
        cu_1 = cus$option_1,
        cu_2 = cus$option_2,
        prob_1 = prob_1,
        chose_option1 = rbinom(1, 1, prob_1)
      )
    )

  })

}

#' Simulate Choices on SBORG Prospects
#'
#' @param agents A data frame with columns 'subject' and 'gamma' from which to
#'   simulate choices.
#'
#' @return A data frame with, for each agent's gamma, the prospect information
#'   based on the possible prospects from the SBORG task, the counterfactual utilities of options one and two ('cu_1',
#'   'cu_2'), the probability of choosing option one ('prob_1'), and a simulated
#'   choice given `rbinom` ('chose_option_1').
#' @export
#'
#' @examples
#'
#' simulate_sbg_prospects(
#'   agents = data.frame(
#'     subject = 1:2,
#'     gamma = c(0, 0.5)
#'   )
#' )
#'
#'
simulate_sbg_prospects <- function(
  agents
) {

  prospect_grid <- expand_grid(
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

  purrr::pmap_df(
    expand.grid(.x = 1:nrow(agents),
                .y = 1:nrow(prospect_grid)
    ),
    ~ {

      cus <- get_cu(
        prospect = prospect_grid[.y,],
        gamma = agents[.x,]$gamma
      )

      prob_1 <- get_cu_choice_prob(
        utilities = cus,
        option = "1",
        epsilon = 1
      )

      dplyr::bind_cols(
        prospect_grid[.y,],
        tibble::tibble(
          subject = agents[.x, ]$subject,
          gamma = agents[.x, ]$gamma,
          cu_1 = cus$option_1,
          cu_2 = cus$option_2,
          prob_1 = prob_1,
          chose_option1 = rbinom(1, 1, prob_1)
        )
      )

    })

}
