test_that("Test for positive coefficient of voveru", {
  expect_true(as.numeric(coef(cpi_fe1)[1]) > 0)
  expect_true(as.numeric(coef(cpi_fe2)[1]) > 0)
  expect_true(as.numeric(coef(cpi_fe3)[1]) > 0)
  expect_true(as.numeric(coef(wage_fe1)[1]) > 0)
  expect_true(as.numeric(coef(wage_fe2)[1]) > 0)
  expect_true(as.numeric(coef(wage_fe3)[1]) > 0)
})