

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
