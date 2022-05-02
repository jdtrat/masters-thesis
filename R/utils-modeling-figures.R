

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
                .y = c(0.5, 1, 3, 5, 10, 30)
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

#' Plot Parameter Recovery for SBORG Data
#'
#' @param sbg_posterior_summary_data The output of [sbg_summarize_simulation_data()]
#' @param model_type Either "Metropolis-Hastings Sampling" or  "Hamiltonian Monte Carlo (Stan) Sampling"
#'
#' @return A ggplot showing the parameter recovery status of simulated gamma for the supplied model type.
#' @export
#'
generate_parameter_recovery_plot <- function(
  sbg_posterior_summary_data,
  model_type
) {

  # Only want to plot 95% HDI
  stopifnot(c("q2.5", "q97.5") %in% colnames(sbg_posterior_summary_data))

  sbg_posterior_summary_data %>%
    filter(type == model_type) %>%
    ggplot(aes(y = subject)) +
    geom_linerange(
      aes(xmin = q2.5, xmax = q97.5)
    ) +
    geom_point(
      aes(x = sim_gamma,
          fill = ifelse(sim_gamma >= q2.5 & sim_gamma <= q97.5, "skyblue", "red")
      ),
      color = "black",
      size = 2,
      shape = 21
    ) +
    cowplot::theme_cowplot() +
    scale_fill_identity(guide = "legend",
                        labels = c(
                          "Parameter Recovery Outside HDI",
                          "Parameter Recovery Inside HDI"
                        )
    ) +
    labs(
      title = "95% Highest Density Interval of Gamma's Posterior Distribution",
      subtitle = paste0("for 50 Simulated Subjects via ", model_type),
      y = "Simulated Subject",
      x = "Gamma Value"
    ) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.025, 0.95)
    )
}

#' Plot Parameter Recovery for SBORG Data
#'
#' @param sbg_posterior_summary_data The output of
#'   [sbg_summarize_simulation_data()]
#'
#' @return A ggplot showing the parameter recovery status of simulated gamma for
#' both HMC and Metropolic-Hastings algorithm.
#' @export
#'
generate_sbg_sim_parameter_recovery_figure <- function(
  sbg_sim_posterior_summary_data
) {

  # Generate Individual Plots Overriding Theme Options for Combination with
  # Cowplot
  mh_plot <- sbg_sim_posterior_summary_data %>%
    generate_parameter_recovery_plot(
      model_type = "Metropolis-Hastings Sampling"
    ) +
    facet_wrap(~type) +
    labs(
      title = NULL,
      subtitle = NULL
    ) +
    theme(
      legend.position = "none",
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

  hmc_plot <- sbg_sim_posterior_summary_data %>%
    generate_parameter_recovery_plot(
      model_type = "Hamiltonian Monte Carlo (Stan) Sampling"
    ) +
    facet_wrap(~type) +
    labs(
      title = NULL,
      subtitle = NULL
    ) +
    theme(
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

  # Draw Title and Subtitle
  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "95% Highest Density Interval of Gamma's Posterior Distribution",
      fontface = 'bold',
      y = 0.65,
      x = 0,
      hjust = 0
    ) +
    cowplot::draw_label(
      "for 50 Simulated Subjects",
      fontface = 'bold',
      size = 12,
      x = 0,
      y = 0.25,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(4, 0, 4, 7)
    )

  # Copy Legend for Adding Beneath Combined Plot
  legend <- cowplot::get_legend(
    hmc_plot +
      theme(
        legend.box.margin = margin(15, 0, 0, 75),
        legend.box.spacing = unit(2, "in"),
        legend.justification = c(0, 0.5),
        legend.text.align = 1,
        legend.direction = "horizontal"
    )
  )

  param_plots <- cowplot::plot_grid(
    mh_plot,
    # Since I used this for creating the joint legend, I remove it here.
    hmc_plot +
      theme(
        legend.position = "none"
      ),
    nrow = 1
  )

  # Combine title, parameter plots, and legend
  # in one column
  cowplot::plot_grid(
    title,
    param_plots,
    legend,
    ncol = 1,
    rel_heights = c(0.1, 1, 0.05)
  )

}


#' Generate Population Level Posterior Estimates for a Model Fit
#'
#' @description This function accepts a cmdstanr model fit and plots the
#'   posterior density for all group-level posterior parameter samples (any
#'   parameter beginning with 'mu_'), excluding the mean (mu_pr) used in the
#'   standard normal hierarchical specification.
#' @param model_fit A cmdstanr model fit environment
#' @param mu_param_colors A named character vector corresponding to the 'mu_'
#'   parameters estimated with Stan. These will be used to ensure a consistent
#'   coloring of parameters caross model fits.
#'
#' @return A ggplot
#' @export
#'
generate_population_posterior_plot_individual <- function(
  model_fit,
  mu_param_colors = c(
    "mu_rho"= "#5DA5DAFF",
    "mu_tau" = "#FAA43AFF",
    "mu_gamma" = "#60BD68FF"
  )
) {

  stan_vars <- model_fit$metadata()$stan_variables
  mod_vars <- stan_vars[which(grepl("^mu_", stan_vars))]
  # Remove the mu_pr, we only want the actual group level params
  mod_vars <- mod_vars[which(mod_vars != "mu_pr")]

  model_fit_params <- purrr::map_df(
    mod_vars,
    ~ {
      tibble(
        parameter = .x,
        value = posterior::extract_variable(
          model_fit$draws(.x),
          .x
        )
      )
    }
  )

  ggplot(model_fit_params,
         aes(x = value, fill = parameter)) +
    geom_density(
      color = scales::alpha("black", 0.8)
    ) +
    cowplot::theme_cowplot() +
    scale_fill_manual(
      values = mu_param_colors
    ) +
    # Remove trailing zeros
    scale_x_continuous(
      n.breaks = 4,
      labels = function(x) ifelse(x == 0, "0", x)
    ) +
    theme(
      plot.title = element_text(
        size = 18,
        hjust = 0.5
      ),
      legend.position = "none",
      axis.line.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(
        size = 14
      ),
      axis.title.x = element_blank(),
      strip.background = element_blank(),
      strip.text = ggtext::element_textbox(
        size = 16,
        face = "bold",
        box.color = "black",
        linewidth = 0.6,
        halign = 0.5,
        linetype = 1, r = unit(5, "pt"), width = unit(0.5, "npc"),
        padding = margin(3, 3, 3, 3), margin = margin(3, 3, 3, 3)
      )
    ) +
    facet_wrap(~parameter,
               scales = "free",
               labeller = as_labeller(
                 function(x) {
                   name <- str_to_title(
                     str_remove(x, "mu_")
                   )
                   glue::glue("<span style='color: {mu_param_colors[x]};'> {name} </span>")
                 }
               )
    )

}


#' Generate Population Level Posterior Estimates for a Model Fit
#'
#' @description This function accepts the a named list of each cmdstanr model
#'   fit environment and plots the posterior density for all group-level
#'   posterior parameter samples (any parameter beginning with 'mu_'), excluding
#'   the mean (mu_pr) used in the standard normal hierarchical specification.
#' @param model_fit_list A list of the cmdstanr model fits. See examples.
#' @param model_colors A character vector to be used for coloring each model.
#'
#' @return A ggplot
#' @export
#'
#' @examples
#'
#'  generate_population_posterior_plot(
#'    model_fit_list = tibble::lst(
#'      sbg_fit_mcmc_sborg_tau_gamma,
#'      sbg_fit_mcmc_sborg_ra_tau,
#'      sbg_fit_mcmc_sborg_ra_tau_gamma,
#'    )
#'  )
#'
generate_population_posterior_plot <- function(
  model_fit_list,
  model_colors = paletteer::paletteer_d('yarrr::espresso', 3)
) {

  prep_pop_post_plot_data <- function(model_fit) {

    stan_vars <- model_fit$metadata()$stan_variables
    mod_vars <- stan_vars[which(grepl("^mu_", stan_vars))]
    # Remove the mu_pr, we only want the actual group level params
    mod_vars <- mod_vars[which(mod_vars != "mu_pr")]

    purrr::map_df(
      mod_vars,
      ~ {
        tibble(
          parameter = .x,
          value = posterior::extract_variable(
            model_fit$draws(.x),
            .x
          )
        )
      }
    )

  }

  model_color_vec = values = c(
    "CPUT + Softmax Sensitivity" = model_colors[1],
    "CPUT + Risk Sensitivity + Softmax Sensitivity" = model_colors[2],
    "EUT + Softmax Sensitivity" = model_colors[3]
  )


  purrr::map_df(
    model_fit_list,
    ~ prep_pop_post_plot_data(.x),
    .id = "model"
  ) %>%
    mutate(
      model = case_when(
        model == "sbg_fit_mcmc_sborg_tau_gamma" ~ "CPUT + Softmax Sensitivity",
        model == "sbg_fit_mcmc_sborg_ra_tau_gamma" ~ "CPUT + Risk Sensitivity + Softmax Sensitivity",
        model == "sbg_fit_mcmc_sborg_ra_tau" ~ "EUT + Softmax Sensitivity"
      )
    ) %>%
    ggplot(
      aes(
        x = value,
        color = model,
        fill = model
      )
    ) +
    ggdist::stat_slab(
      size = 0.75,
      normalize = "panels",
      slab_color = "black",
      slab_fill = "transparent"
    ) +
    ggdist::stat_slab(
      size = 0.75,
      normalize = "panels",
      slab_color = "black",
      alpha = 0.5
    ) +
    ggdist::stat_pointinterval(
      point_interval = "median_hdi",
      .width = c(0.5, 0.8, 0.95),
      position = position_dodge(
        width = 0.2,
        preserve = "single"
      ),
      point_color = "black",
      shape = 21,
      stroke = 0.75
    ) +
    cowplot::theme_cowplot() +
    facet_wrap(
      ~parameter,
      scales = "free_x",
      labeller = as_labeller(
        c(
          "mu_gamma" = "Gamma",
          "mu_tau" = "Tau",
          "mu_rho" = "Rho"
        )
      )
    ) +
    scale_fill_manual(
      values = model_color_vec
    ) +
    scale_color_manual(
      values = model_color_vec
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.justification = "center",
      legend.box.margin = margin(2.5,1,5,1),
      legend.text.align = 0.5,
      legend.margin = margin(1,6,1,1),
      legend.background = element_rect(color = "black"),
      strip.background = element_blank(),
      strip.text = ggtext::element_textbox(
        size = 14,
        face = "bold",
        box.color = "#4A618C",
        linewidth = 0.6,
        halign = 0.5,
        linetype = 1,
        r = unit(5, "pt"),
        width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0),
        margin = margin(3, 3, 3, 3)
      ),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18)
    ) +
    labs(
      x = "Value",
      y = "Relative Plausibility",
      title = "Group-Level Posterior Parameter Estimates"
    )

}
