

#' Generate Counterfactual Utility Plot Demoing SBORG Prospect for Gamma Range
#'
#' @return A ggplot
#' @export
#'
#' @examples
generate_sbg_demo_prospect_figure <- function() {
  sbg_demo_prospect <- gen_prospct(
    option_1 = gen_option(3, 1),
    option_2 = gen_option(2, 0.5, 5, 0.5)
  )

  sbg_demo_prospect_df <- purrr::map_df(seq(0,0.5,0.01), ~ {
    cu <- get_cu(
      prospect = sbg_demo_prospect,
      gamma = .x
    )

    tibble(
      cu_1 = cu$option_1,
      cu_2 = cu$option_2,
      gamma = .x
    )

  })


  ggplot(sbg_demo_prospect_df,
         aes(x = gamma)) +
    geom_line(aes(y = cu_1), color = "blue", size = 1) +
    geom_line(aes(y = cu_2), color = "red", size = 1) +
    labs(y = "Counterfactual Utility",
         x = expression(gamma),
         title = "Counterfactual Utilities of <span style = 'color: blue';>Sure Bet ($3)</span> and <span style = 'color: red'>Gamble ($2 or $5)</span>"
    ) +
    cowplot::theme_cowplot(
      font_size = 18,
      rel_small = 0.9,
      rel_large = 1
    ) +
    theme(
      plot.title = ggtext::element_markdown(),
      axis.title.x = element_text(size = 25, face = "bold")
    ) +
    coord_cartesian(
      ylim = c(0,5)
    )

}

#' Generate Softmax Probability Plot Demoing SBORG Prospect for Gamma Range
#'
#' @return A ggplot
#' @export
#'
#' @examples
generate_sbg_demo_softmax_figure <- function() {

  sbg_demo_prospect <- gen_prospct(
    option_1 = gen_option(3, 1),
    option_2 = gen_option(2, 0.5, 5, 0.5)
  )

  sbg_demo_prospect_df <- purrr::pmap_df(
    expand_grid(.x = seq(0,0.5,0.01),
                .y = c(0.5, 1, 5, 10, 20, 30)
                ),
    ~ {
      cu <- get_cu(
        prospect = sbg_demo_prospect,
        gamma = .x
      )

      prob_1 <- get_cu_choice_prob(
        utilities = cu,
        option = "1",
        epsilon = .y
      )

      tibble(
        cu_1 = cu$option_1,
        cu_2 = cu$option_2,
        prob_1,
        tau = .y,
        gamma = .x
      )

    })

  ggplot(
    sbg_demo_prospect_df,
    aes(x = gamma)
  ) +
    geom_line(aes(y = prob_1, color = as.factor(tau)), size = 1) +
    labs(y = "Probability",
         x = expression(gamma),
         color = expression(tau),
         title = "Probability of Choosing Option 1 (Sure Bet)"
    ) +
    cowplot::theme_cowplot(
      font_size = 18,
      rel_small = 0.9,
      rel_large = 1
    ) +
    theme(
      plot.title = ggtext::element_markdown(),
      axis.title.x = element_text(size = 25, face = "bold"),
      legend.direction = "horizontal",
      legend.background = element_rect(
        color = "black", size = 0.5
      ),
      legend.margin = margin(6,6,6,6),
      legend.text.align = 0.5,
      legend.justification = "center",
      legend.position = c(0.7, 0.2),
      legend.title = element_text(size = 22)
    ) +
    coord_cartesian(
      ylim = c(0,1)
    )
}

#' Generate a Plot of KT Posterior Distribution for CPUT with a Beta(1,1) fit.
#'
#' @param kt_posterior_draws
#'
#' @return A ggplot
#' @export
#'
generate_kt_post_approximation_plot <- function(kt_posterior_draws) {

  ggplot() +
    stat_function(
      geom = "density",
      outline.type = "upper",
      data = data.frame(x = c(0, 1)),
      aes(x = x),
      fun = dbeta,
      n = 101,
      args = list(
        shape1 = 1.1,
        shape2 = 1.1
      ),
      fill = "skyblue",
      alpha = 0.4,
      size = 0.75,
      color = scales::alpha("skyblue4", 0.8)
    ) +
    geom_density(
      data = data.frame(post = kt_posterior_draws),
      aes(x = post),
      color = "tomato3",
      size = 1.2
    ) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(
      x = expression(hat(gamma)),
      y = "Posterior Density",
      title = "<span style = 'color: tomato3';>Estimated Posterior Distribution</span> from Kahneman Tversky Choice Data",
      subtitle = "Approximated with a <span style = 'color: skyblue4'>Beta(1.1, 1.1) Distribution</span>"
    ) +
    cowplot::theme_cowplot(
      font_size = 18,
      rel_small = 0.9,
      rel_large = 1
    ) +
    theme(
      plot.title = ggtext::element_markdown(),
      plot.subtitle = ggtext::element_markdown(face = "bold"),
      axis.title.x = element_text(size = 25, face = "bold")
    )



}
