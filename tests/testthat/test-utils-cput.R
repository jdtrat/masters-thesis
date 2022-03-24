test_that("`gen_prospect()`", {
  op1 <- gen_option(
    outcome_a = 4000,
    prob_a = 0.8
  )

  op2 <- gen_option(
    outcome_a = 3000,
    prob_a = 1
  )

  expect_equal(
    gen_prospct(
      option_1 = op1,
      option_2 = op2
    ),
    data.frame(
      option1_outcome_a = c(4000),
      option1_prob_a = c(0.8),
      option1_outcome_b = c(0),
      option1_prob_b = c(0.2),
      option2_outcome_a = c(3000),
      option2_prob_a = c(1),
      option2_outcome_b = c(0),
      option2_prob_b = c(0)
    )
  )


})

testthat::test_that("`get_cu()` canonical prospect, gamma 0", {
  prospect <- gen_prospct(
    gen_option(4000, 0.8),
    gen_option(3000, 1)
  )
  cus <- get_cu(prospect, gamma = 0)
  testthat::expect_equal(cus$option_1, 3200)
  testthat::expect_equal(cus$option_2, 3000)
})


