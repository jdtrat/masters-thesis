
generate_model_table_df <- function() {
  tibble::tibble(
    model_name = c(
      "sborg_tau_gamma",
      "sborg_ra_tau_gamma",
      "sborg_ra_tau"
    ),
    model_title = c(
      "CPUT + Softmax Sensitivity",
      "CPUT + Risk Sensitivity + Softmax Sensitivity",
      "EUT + Softmax Sensitivity"
    ),
    parameters_fit = c(
      "\\gamma, \\tau",
      "\\gamma, \\tau, \\rho",
      "\\rho, \\tau"
    )
  )
}

generate_model_flextable <- function() {

  generate_model_table_df() %>%
    dplyr::select(
      -model_name
    ) %>%
    flextable::flextable() %>%
    flextable::compose(j = "parameters_fit",
                       value = flextable::as_paragraph(
                         flextable::as_equation(parameters_fit)
                       )
    ) %>%
    flextable::fontsize(j = "model_title", size = 14) %>%
    flextable::bold(part = "header") %>%
    flextable::fontsize(size = 16, part = "header") %>%
    flextable::set_header_labels(
      model_title = "Model Description",
      parameters_fit = "Estimated Parameters"
    ) %>%
    flextable::autofit()
}

#' Generate the Model Fit Summary Table for All Models
#'
#' @param pp_choice_accuracy_data The posterior predicted choice data generated
#'   with [generate_pp_choice_accuracy_table()].
#' @param elpd_looic_model_evidence_data The ELPD/LOO-IC metrics generated with
#'   [summarize_elpd_looic_evidence()].
#' @param marginal_likelihood_model_evidence_data The marginal likelihood model
#'   evidence data generated with
#'   [summarize_marginal_likelihood_model_evidence()].
#'
#' @return A flextable object
#' @export
#'
generate_model_fit_summary_table <- function(
  pp_choice_accuracy_data,
  elpd_looic_model_evidence_data,
  marginal_likelihood_model_evidence_data
) {

  left_join(
    generate_model_table_df(),
    pp_choice_accuracy_data,
    by = "model_name"
  ) %>%
    left_join(
      elpd_looic_model_evidence_data,
      by = "model_name"
    ) %>%
    left_join(
      marginal_likelihood_model_evidence_data,
      by = "model_name"
    ) %>%
    dplyr::select(
      -model_name
    ) %>%
    flextable::flextable() %>%
    flextable::compose(j = "parameters_fit",
                       value = flextable::as_paragraph(
                         flextable::as_equation(parameters_fit)
                       )
    ) %>%
    flextable::fontsize(j = c(
      "model_title",
      "pp_accuracy",
      "elpd",
      "looic",
      "model_evidence"
    ),
    size = 14) %>%
    flextable::bold(part = "header") %>%
    flextable::fontsize(size = 16, part = "header") %>%
    flextable::set_header_labels(
      model_title = "Model Description",
      parameters_fit = "Estimated Parameters",
      pp_accuracy = "Posterior Predictive Choice Accuracy",
      elpd = "ELPD Predictive Density",
      looic = "LOOIC",
      model_evidence = "Marginal Likelihood Model Evidence"
    ) %>%
    flextable::bg(
      i = ~ model_title == "CPUT + Softmax Sensitivity" | model_title == "EUT + Softmax Sensitivity",
      bg = "aliceblue"
    ) %>%
    flextable::autofit()

}

