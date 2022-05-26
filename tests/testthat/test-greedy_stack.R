test_that("Greedy stacking (and helper metrics) work", {
  expect_equal( rmse_stack( 1:10, 2:11), 1)
  expect_equal( rmse_stack( 1:10, (1:10)^2), 44.35, tolerance = 0.01 )
  expect_equal( mae_stack(1:10, 2:11), 1)
  expect_equal( mae_stack( 1:10, (1:10)^2), 33 )

  set.seed(1071)

  x <- matrix( rnorm(300), ncol = 3)
  y <- rpois(100, lambda = 1.5)

  expected_weights <- greedy_stacking( y, x, metric = rmse_stack)
  expect_equal( expected_weights, c(0.3564, 0.2871, 0.3564 ), tolerance = 0.01)
  expected_weights <- greedy_stacking( y, x, metric = mae_stack)
  expect_equal( expected_weights, c(0.3762, 0.2871, 0.3366), tolerance = 0.01)
})
