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


# Utilities to Likelihood to Choice Figure -----------------------------------------------------------

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



# Likelihood of Each -----------------------------------------------------------------------


#' Generate likelihoods of Choices for SBORG Prospect
#'
#' @param prospect A prospect
#' @param gammas
#' @param tau
#'
#' @return
#' @export
#'
#' @examples
#'
#' generate_sbg_demo_lk_data(
#'     gen_prospect(
#'       option_1 = gen_option(3, 1),
#'       option_2 = gen_option(2, 0.5, 5, 0.5)
#'   )
#' )
#'
generate_sbg_demo_lk_data <- function(
  prospect,
  gammas = seq(0, 1, 0.01),
  tau = 1
) {

  lk_sure_bet <- purrr::map_df(
    gammas,
    ~ {
      get_utility(
        prospect = prospect,
        gamma = .x
      ) %>%
        get_softmax_choice_prob(
          option = 1,
          tau = tau
        ) %>%
        tibble(
          gamma = .x,
          lk_sure_bet = .
        )
    }
  )

  lk_gamble <- purrr::map_df(
    gammas,
    ~ {
      get_utility(
        prospect = prospect,
        gamma = .x
      ) %>%
        get_softmax_choice_prob(
          option = 2,
          tau = tau
        ) %>%
        tibble(
          gamma = .x,
          lk_gamble = .
        )
    }
  )

  bind_cols(
    prospect,
    left_join(
      lk_sure_bet,
      lk_gamble,
      by = "gamma"
    )
  )

}


#' Generate SBG Demo Fig Likelihood Plot
#'
#' @param sbg_likelihood_data The output of [generate_sbg_demo_lk_data()]
#' @param .choice_type Should only one of the choice types be returned in the
#'   plot? Must be "lk_sure_bet", "lk_gamble", "lk_joint".
#'
#' @return
#' @export
#'
#' @examples
gen_sbg_demo_fig_likelihood_plot <- function(
  sbg_likelihood_data,
  .choice_type = NULL,
  sure_bet_color = "deepskyblue3",
  gamble_color = "darkmagenta"
) {

  data <- sbg_likelihood_data %>%
    mutate(
      lk_sure_bet = lk_sure_bet / sum(lk_sure_bet),
      lk_gamble = lk_gamble / sum(lk_gamble),
      lk_joint = lk_sure_bet * lk_gamble
    ) %>%
    pivot_longer(
      cols = starts_with("lk"),
      names_to = "choice_type",
      values_to = "likelihood"
    )

  if (!is.null(.choice_type)) {
    data <- data %>%
      filter(
        choice_type == .choice_type
      )
  }

  cols <- colorRampPalette(c(sure_bet_color, gamble_color))(3)

  data %>%
    ggplot(
      aes(
        x = gamma,
        y = likelihood,
        color = choice_type
      )
    ) +
    geom_line(
      size = 1
    ) +
    # Remove trailing zeros
    scale_x_continuous(
      n.breaks = 5,
      labels = function(x) ifelse(x == 0, "0", x)
    ) +
    scale_color_manual(
      values = c(
        "lk_sure_bet" = cols[1],
        "lk_gamble" = cols[3],
        "lk_joint" = cols[2]
      )
    ) +
    facet_wrap(~choice_type,
               labeller = as_labeller(
                 c(
                   "lk_sure_bet" = glue::glue("Sure Bet (${unique(data$option1_outcome_a)})"),
                   "lk_gamble" = glue::glue("Gamble (${unique(data$option2_outcome_a)} or ${unique(data$option2_outcome_b)})"),
                   "lk_joint" = "Sure Bet, Gamble"
                 )
               )
    ) +
    labs(
      y = "Relative Likelihood",
      x = expression(gamma),
      title = expression(paste("Likelihood of Observing a Choice Given ", gamma))
    ) +
    cowplot::theme_cowplot(
      font_size = 18,
      rel_small = 0.9,
      rel_large = 1
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 25, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(),
      strip.text = ggtext::element_textbox(
        size = 12,
        face = "bold",
        box.color = "#4A618C",
        linewidth = 0.6,
        halign = 0.5,
        linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
      )
    )

}

#' Generate a Plot Showing How Likelihoods Multiply for SBORG Choices
#'
#' @param sbg_likelihood_data The output of [generate_sbg_demo_lk_data()]
#' @return
#' @export
#'
#' @examples
#'
#' sbg_demo_lk_fig_data <- generate_sbg_demo_lk_data(
#'     gen_prospect(
#'       option_1 = gen_option(3, 1),
#'       option_2 = gen_option(2, 0.5, 5, 0.5)
#'     )
#'   )
#'
#'
#' gen_sbg_demo_joint_likelihood_plot(sbg_demo_lk_fig_data)
gen_sbg_demo_joint_likelihood_plot <- function(
  sbg_likelihood_data,
  include_joint_titles = TRUE
) {

  # Draw a multiply sign
  multiply_plot <- cowplot::ggdraw() +
    cowplot::draw_label(
      "\u00D7",
      size = 60,
      y = 0.55
    )

  # Draw an equal sign
  equal_plot <- cowplot::ggdraw() +
    cowplot::draw_label(
      "\u003D",
      size = 60,
      y = 0.55
    )

  # Draw the title for either option
  either_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Likelihoods of Choosing Either Option",
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(7, 0, 0, 7)
    )

  # Draw title for joint likelihoods
  joint_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Likelihood of Choosing One of Each",
      # lineheight = 0.01,
      fontface = 'bold',
      x = 1,
      hjust = 1
    ) +
    theme(
      plot.margin = margin(7,10,0,0)
    )

  plot_titles <- cowplot::plot_grid(
    either_title,
    joint_title,
    ncol = 2
  )


  # Generate individual plot panels
  sure_bet_lk <- gen_sbg_demo_fig_likelihood_plot(
    sbg_likelihood_data = sbg_likelihood_data,
    .choice_type = "lk_sure_bet"
  )

  gamble_lk <- gen_sbg_demo_fig_likelihood_plot(
    sbg_likelihood_data = sbg_likelihood_data,
    .choice_type = "lk_gamble"
  )

  joint_lk <- gen_sbg_demo_fig_likelihood_plot(
    sbg_likelihood_data = sbg_likelihood_data,
    .choice_type = "lk_joint"
  )

  clean_individual_plot <- function(.plot) {
    .plot +
      labs(
        title = NULL,
        y = NULL
      ) +
      theme(
        axis.line.y = element_blank()
      )
  }

  plot_body <- cowplot::plot_grid(
    clean_individual_plot(
      sure_bet_lk
    ),
    multiply_plot,
    clean_individual_plot(
      gamble_lk
    ),
    equal_plot,
    clean_individual_plot(
      joint_lk
    ),
    nrow = 1,
    rel_widths = c(1, 0.1, 1, 0.2, 1)
  )

  cowplot::plot_grid(
    if (include_joint_titles) plot_titles else NULL,
    plot_body,
    ncol = 1,
    rel_heights = c(0.1, 1)
  )


}


# Nine Simulated Choices --------------------------------------------------

#' Get Simulated Prospect Likelihoods
#'
#' @description This function is used to create a tibble with the posterior
#'   distribution of gamma for each prospect based on (simulated) subjects'
#'   choice and a prior distribution (defaults to uniform). This can be used to
#'   demo how the posterior probability of gamma can be updated based on choice
#'   data by multiplying the posterior probabilities together.
#'
#' @param simulated_data Output of [simulate_sbg_prospects()]
#' @param gammas A numeric vector of the gammas for which to calculate the
#'   likelihood of a choice.
#' @param prior A numeric vector of the same length as `gammas`, which
#'   represents the prior probability of a gamma value for each prospect.
#' @param tau The softmax sensitivity parameter to include when calculating
#'   the likelihood of choosing an option given the difference in utilities
#'   between options.
#'
#' @return A tibble with, for each prospect and gamma combination, the
#'   likelihood of choosing option one for a given gamma value, along with the
#'   unscaled and scaled posterior probability of gamma given the observed
#'   choice behavior (`chose_option1`) for a specific prospect.
#' @export
#'
#' @examples
get_simulated_prospect_likelihoods <- function(
  simulated_data,
  gammas = seq(0, 1, 0.01),
  prior = dunif(gammas, min = 0, max = 1),
  tau = 1
) {

  purrr::map_df(
    1:nrow(simulated_data),
    function(prospect_number) {

      likelihood <- purrr::map_dbl(
        .x = gammas,
        ~ get_utility(
          prospect = simulated_data[prospect_number,],
          gamma = .x
        ) %>%
          get_softmax_choice_prob(
            option = if (simulated_data$chose_option1[prospect_number] == 1) 1 else 2,
            tau = tau
          )
      )

      posterior <- likelihood * prior

      tibble::tibble(
        prospect = prospect_number,
        gamma = gammas,
        unscaled_posterior = posterior,
        likelihood = likelihood,
        posterior = posterior / sum(likelihood)
      )

    }
  )
}


#' Generate Plot Data for Prospect Likelihoods
#'
#' @param simulated_prospect Prospect that was used to simulate likelihood
#' @param simulated_prospect_likelihoods
#'
#' @return
#' @export
#'
#' @examples
gen_nine_post_prospect_likelihood_plot_data <- function(
  simulated_prospect_likelihoods,
  sure_bet_color = "#00A4CD",
  gamble_color = "#842fe1"
) {

  posterior_plot_data <- simulated_prospect_likelihoods %>%
    group_by(
      prospect
    ) %>%
    nest() %>%
    ungroup() %>%
    mutate(
      # For each prospect, calculate the cumulative posterior distribution by
      # using a sliding window to create a list of each subset of prospect data
      # and mapping over it to multiply them together
      cum_posterior = purrr::map(
        slider::slide(
          data, ~ {.x},
          .before = Inf,
          .complete = TRUE
        ),
        function(windowed_data) {
          purrr::map(windowed_data, ~ {
            .x$unscaled_posterior
          }) %>%
            reduce(`*`)
        }
      ),
      # Prior is the most recent cumulative posterior distribution
      prior = lag(cum_posterior, default = list(rep(1, 101))),
      # Observe how many sure bet were made
      num_sure_bet_choices = cumsum(
        purrr::map_dbl(data, ~unique(.x$chose_option1))
      ),
      num_gamble_choices = prospect - num_sure_bet_choices
    ) %>%
    unnest(
      cols = c(
        data,
        prior,
        cum_posterior
      )
    ) %>%
    group_by(
      prospect
    ) %>%
    mutate(
      prior = prior / sum(prior),
      cum_posterior = cum_posterior / sum(cum_posterior)
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      sure_bet_info = glue::glue(
        "<span style = 'color: {sure_bet_color}';>**Sure Bet (${option1_outcome_a})**</span>"
      ),
      gamble_info = glue::glue(
        "<span style = 'color: {gamble_color}';>**Gamble (${option2_outcome_a} or ${option2_outcome_b})**</span>"
      ),
      prospect_info = glue::glue(
        "{sure_bet_info} **|** {gamble_info}"
      ),
      likelihood_info = glue::glue(
        "Most Plausible Parameter Distribution"
      ),
      num_choice_info = cli::pluralize(
        "<span style = 'color: {sure_bet_color}';>**{num_sure_bet_choices}** Sure Bet</span>, <span style = 'color: {gamble_color}';>**{num_gamble_choices}** Gamble</span>"
      )
    ) %>%
    ungroup() %>%
    select(
      trial = prospect,
      gamma,
      prior,
      posterior = cum_posterior,
      num_sure_bet_choices,
      starts_with("num_"),
      ends_with("info")
    ) %>%
    mutate(
      trial = as_factor(trial) %>%
        fct_relevel(
          as.character(
            1:length(unique(trial))
          )
        ),
      curve_type = "joint"
    )

  # Generate a color palette for sure bet to gamble with the same length as the number of trials
  joint_colors <- colorRampPalette(c(sure_bet_color, gamble_color))(50)

  posterior_plot_data %>%
    mutate(
      likelihood_info = case_when(
        curve_type == "joint" ~ "Parameter Distribution from Choice Behavior"
      ),
      ratio_sure_bet = round(
        50 * (num_sure_bet_choices / as.numeric(trial))
      ),
      ratio_sure_bet = case_when(
        ratio_sure_bet == 0 ~ 1,
        ratio_sure_bet == length(unique(posterior_plot_data$trial)) ~ 1,
        TRUE ~ ratio_sure_bet
      ),
      curve_color = case_when(
        curve_type == "joint" ~ rev(joint_colors)[ratio_sure_bet]
      ),
      prior_curve_color = lag(
        curve_color,
        n = 101,
        default = "black"
      )
    )

}



gen_nine_post_prespect_likelihood_plot <- function() {

  prospect <- gen_prospect(
    option_1 = gen_option(3, 1),
    option_2 = gen_option(2, 0.5, 5, 0.5)
  )

  # Simulate nine choices with seed of 7
  set.seed(7)
  prob_1 <- get_utility(
    prospect = prospect,
    gamma = 0.25
  ) %>%
    get_softmax_choice_prob(
      option = 1,
      tau = 2.2
    )

  sbg_ind_choices <- bind_cols(
    prospect,
    tibble(
      prob_1,
      chose_option1 = rbinom(n = 9, size = 1, prob = prob_1)
    )
  )

  # Get prospect likelihoods
  sbg_recovered_lks <- get_simulated_prospect_likelihoods(
    sbg_ind_choices,
    tau = 2.2
  )

  # Join the data!
  sbg_lk_plot_data <- left_join(
    sbg_recovered_lks,
    sbg_ind_choices %>%
      mutate(
        prospect = row_number()
      ),
    by = "prospect"
  )

  # Format it all for ggplot
  sbg_plot_data <- gen_nine_post_prospect_likelihood_plot_data(
    simulated_prospect_likelihoods = sbg_lk_plot_data,
    sure_bet_color = "deepskyblue3",
    gamble_color = "darkmagenta"
  )

  # And Plot!
  sbg_plot_data %>%
    ggplot(aes(x = gamma)) +
    geom_area(
      aes(y = prior,
          color = prior_curve_color,
          fill = prior_curve_color
      ),
      linetype = "dashed",
      alpha = 0.1,
      size = 1
    ) +
    geom_area(
      aes(
        y = posterior,
        color = curve_color,
        fill = curve_color
      ),
      position = "identity",
      alpha = 0.4,
      size = 1
    ) +
    coord_cartesian(
      ylim = c(0, 0.041)
    ) +
    geom_richtext(
      aes(
        x = 0.5,
        y = 0.039,
        label = num_choice_info
      ),
      size = 5
    ) +
    facet_wrap(
      ~trial
    ) +
    # Remove trailing zeros
    scale_x_continuous(
      n.breaks = 5,
      labels = function(x) ifelse(x == 0, "0", x)
    ) +
    labs(
      x = expression(gamma),
      y = "Relative Plausibility",
      title = "Bayes Rules!",
      subtitle = "Estimate Parameters by Multiplying the Likelihood of Observing Each Choice",
      caption = glue::glue(
        "Simulated Choices Between a {sbg_plot_data$sure_bet_info[1]} or {sbg_plot_data$gamble_info[1]} with \U03B3 = 0.25"
      )
    ) +
    scale_color_identity() +
    scale_fill_identity() +
    cowplot::theme_cowplot() +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 25, face = "bold"),
      plot.caption = element_markdown(size = 12),
      plot.subtitle = element_markdown(lineheight = 1.275),
      axis.text.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank()
    )

}
