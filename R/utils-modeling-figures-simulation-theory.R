# Messy Code for Creating Simulation Figures (: ----------------------


# Plot Utilities ----------------------------------------------------------

# Utility to generate tau alpha gradient arrow!!!
tau_alpha_gradient_arrow <- function(
  min_x,
  max_x,
  y,
  .prob_type,
  include_tau = TRUE
) {

  out <- list(
    geom_segment(
      show.legend = FALSE,
      data = tibble(
        tau = c(seq(0, 5, 0.25), seq(5, 30, 0.5)),
        x = seq(min_x, max_x - 0.02, length.out = length(tau)),
        xend = x + 0.01,
        y = y,
        yend = y,
        prob_type = .prob_type
      ),
      aes(
        x = x, xend = xend,
        y = y, yend = yend,
        color = prob_type,
      ),
      size = 2
    ),
    geom_segment(
      show.legend = FALSE,
      data = tibble(
        tau = 30,
        x = max_x - 0.01,
        xend = max_x,
        y = y,
        yend = y,
        prob_type = .prob_type,
      ),
      aes(
        x = x, xend = xend,
        y = y, yend = yend,
        color = prob_type
      ),
      size = 2,
      arrow = arrow(length = unit(0.04, "npc")),
      lineend = "round",
      linejoin = "round"
    ),
    geom_text(
      inherit.aes = FALSE,
      show.legend = FALSE,
      # Repeated twice to get slightly bolded text
      data = data.frame(
        prob_type = .prob_type,
        x = rep(c(min_x - 0.03, max_x + 0.06), 2),
        y = y,
        label = rep(c("0", "30"), 2)
      ),
      aes(
        x = x,
        y = y,
        label = label
      ),
      size = 5
    )
  )

  if (include_tau) {
    c(
      out,
      list(
        geom_text(
          inherit.aes = FALSE,
          show.legend = FALSE,
          data = data.frame(
            prob_type = .prob_type,
            x = median(c(min_x, max_x)),
            y = y + 0.06,
            label = c("\U1D70F")
          ),
          aes(
            x = x,
            y = y,
            label = label
          ),
          size = 8,
          fontface = "bold"
        )
      )
    )
  } else {
    out
  }
}


# Main Function -----------------------------------------------------------

gen_sbg_utilities_to_likelihood_figure <- function(
  sure_bet_color = "deepskyblue3",
  gamble_color = "darkmagenta"
) {


  prospect <- gen_prospect(
    option_1 = gen_option(3, 1),
    option_2 = gen_option(2, 0.5, 5, 0.5)
  )

  # Utilities Panel ---------------------------------------------------------


  cus_df <- purrr::map_df(
    .x = seq(0, 1, 0.01),
    ~ {

      cu <- get_utility(
        prospect = prospect,
        gamma = .x,
      )

      tibble(
        cu_1 = cu$option_1,
        cu_2 = cu$option_2,
        gamma = .x
      )
    }
  )

  utilities_panel <- ggplot(cus_df,
                            aes(x = gamma)) +
    geom_line(aes(y = cu_1), color = sure_bet_color, size = 1.5) +
    geom_line(aes(y = cu_2), color = gamble_color, size = 1.5) +
    labs(y = "Counterfactual Utility",
         x = expression(gamma)
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
    scale_y_continuous(
      breaks = seq(-5, 5, 1),
      limits = c(-3.5, 3.5)
    ) +
    geom_textbox(
      data = data.frame(
        x = 0.425,
        y = -2.65,
        label =  glue::glue(
          "Counterfactual Utilities for <br></br><span style = 'color: {sure_bet_color}';>Sure Bet ($3)</span> and <span style = 'color: {gamble_color}'>Gamble ($2 or $5)</span>"
        )
      ),
      aes(
        x = x,
        y = y,
        label = label
      ),
      size = 5,
      fontface = "bold",
      width = 0.7,
      halign = 0.5,
      hjust = 0.5
    )



  # Transformation Label Panel ----------------------------------------------


  transformation_label_panel <- ggdraw() +
    geom_segment(
      data = tibble(
        x = 0.2,
        xend = 0.8,
        y = 0.5772,
        yend = 0.5772
      ),
      aes(
        x = x, xend = xend,
        y = y, yend = yend
      ),
      color = colorRampPalette(c(sure_bet_color, gamble_color))(3)[2],
      arrow = arrow(length = unit(0.04, "npc")),
      size = 1.5,
      lineend = "round",
      linejoin = "round"
    ) +
    geom_text(
      data = tibble(
        x = 0.5,
        y = c(
          0.5772 + 0.03,
          0.5772 - 0.03
        ),
        label = c("Softmax", "Transformation")
      ),
      aes(
        x = x, y = y,
        label = label
      ),
      size = 5
    )


  # Softmax Transformation Panel --------------------------------------------

  # Generate gamma tau grid
  gamma_tau_cu_df <- pmap_df(
    expand_grid(
      .gamma = seq(0, 1, 0.1),
      .tau = c(seq(0, 5, 0.25), seq(5, 30, 0.5))
    ),
    function(.gamma, .tau) {

      cu <- get_utility(
        prospect = prospect,
        gamma = .gamma,
      )

      prob_1 <- get_softmax_choice_prob(
        utilities = cu,
        option = 1,
        tau = .tau
      )

      prob_2 <- get_softmax_choice_prob(
        utilities = cu,
        option = 2,
        tau = .tau
      )

      tibble(
        cu_1 = cu$option_1,
        cu_2 = cu$option_2,
        gamma = .gamma,
        prob_1,
        prob_2,
        tau = .tau
      )

    }
  )

  softmax_transformation_panel <- gamma_tau_cu_df %>%
    pivot_longer(
      prob_1:prob_2,
      names_to = "prob_type",
      values_to = "prob"
    ) %>%
    ggplot(
      aes(
        x = gamma,
        group = tau,
        alpha = tau
      )
    ) +
    geom_line(
      aes(y = prob, color = prob_type),
      size = 1.5,
      show.legend = FALSE
    ) +
    labs(
      y = "Probability",
      x = expression(gamma)
    ) +
    facet_wrap(
      ~prob_type,
      labeller = as_labeller(
        c(
          "prob_1" = glue::glue(
            "<span style = 'color: {sure_bet_color}';>Sure Bet ($3)</span>"
          ),
          "prob_2" = glue::glue(
            "<span style = 'color: {gamble_color}'>Gamble ($2 or $5)</span>"
          )
        )
      )
    ) +
    scale_color_manual(
      values = c(
        "prob_1" = sure_bet_color,
        "prob_2" = gamble_color
      )
    ) +
    scale_alpha_continuous(
      range = c(0.2, 1)
    ) +
    cowplot::theme_cowplot(
      font_size = 18,
      rel_small = 0.9,
      rel_large = 1
    ) +
    theme(
      plot.title = ggtext::element_markdown(),
      axis.title.x = element_text(size = 25, face = "bold"),
      strip.background = element_blank(),
      strip.text = ggtext::element_textbox(
        size = 14,
        face = "bold",
        box.color = "#4A618C",
        linewidth = 0.6,
        halign = 0.5,
        linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
      )
    ) +
    tau_alpha_gradient_arrow(
      min_x = 0.88,
      max_x = 1.18,
      y = 0.175,
      .prob_type = "prob_1",
      include_tau = FALSE
    ) +
    tau_alpha_gradient_arrow(
      min_x = -0.25,
      max_x = 0.05,
      y = 0.0825,
      .prob_type = "prob_2",
      include_tau = TRUE
    ) +
    coord_cartesian(
      xlim = c(0, 1),
      clip = "off"
    ) +
    # Remove trailing zeros
    scale_x_continuous(
      n.breaks = 5,
      labels = function(x) ifelse(x == 0, "0", x)
    )


  # Outcome Panel -----------------------------------------------------------


  outcome_plot <- tibble(
    line_y_start = 0.5775,
    line_x_start = 0,
    line_y_end = c(
      line_y_start + 0.14,
      line_y_start - 0.14
    ),
    line_x_end = 0.52,
    y = c(0.8, 0.35),
    prob_type = c("prob_1", "prob_2"),
    x = 0.52,
    label = c(
      glue::glue(
        "<span style = 'color: {sure_bet_color}';>**Choose<br></br>Sure Bet**</span>"
      ),
      glue::glue(
        "<span style = 'color: {gamble_color}';>**Choose<br></br>Gamble**</span>"
      )
    )
  ) %>%
    ggplot() +
    ggtext::geom_textbox(
      aes(
        x = x, y = y,
        label = label
      ),
      size = 7,
      width = 0.5,
      halign = 0.5
    ) +
    theme_void() +
    coord_cartesian(
      ylim = c(0, 1),
      xlim = c(0,0.75)
    ) +
    geom_segment(
      aes(
        x = line_x_start, xend = line_x_end,
        y = line_y_start, yend = line_y_end,
        color = prob_type
      ),
      arrow = arrow(length = unit(0.04, "npc")),
      size = 1.5,
      lineend = "round",
      linejoin = "round"
    ) +
    scale_color_manual(
      values = c(
        "prob_1" = sure_bet_color,
        "prob_2" = gamble_color
      )
    ) +
    annotate(
      "text",
      x = 0.52,
      y = 0.5775,
      label = "Bernoulli Trial\n(Coin Flip)",
      size = 5
    ) +
    theme(
      legend.position = "none"
    )


  # Create Title ------------------------------------------------------------


  # Draw the title for either option
  overall_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Modeling Behavior: From Utilities to Probabilities and Choices",
      fontface = 'bold',
      x = 0,
      hjust = 0,
      size = 21
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(7, 0, 0, 7)
    )


  # Create Main Panel -------------------------------------------------------


  main_panel <- cowplot::plot_grid(
    utilities_panel +
      labs(title = NULL) +
      theme(
        axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 22)
      ),
    transformation_label_panel,
    softmax_transformation_panel +
      labs(title = NULL) +
      theme(
        axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 22)
      ),
    outcome_plot +
      coord_cartesian(
        xlim = c(0, 1),
        ylim = c(0, 1)
      ),
    nrow = 1,
    axis = "top",
    align = "hv",
    rel_widths = c(1, 0.3, 1.4, 0.5)
  )


  # Create Actual Plot ------------------------------------------------------

  cowplot::plot_grid(
    overall_title,
    main_panel,
    ncol = 1,
    rel_heights = c(0.05, 1)
  )




}
