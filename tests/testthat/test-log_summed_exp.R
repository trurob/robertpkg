test_that("Not infinite",{
  expect_equal(is.infinite(log_summed_exp(c(1:2000))),FALSE)
})
