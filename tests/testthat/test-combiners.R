
test_model_list <- readRDS( "./test_data/combiner_data.rds" )

test_that("Combiner greedy_stacking works", {
  expect_equal( combiner_greedy_stacking(test_model_list),
                c(1, 0, 0)
                )
})
test_that("Combiner lm works", {
  expect_equal( combiner_lm(test_model_list),
                c(1.08755876402204, 0, -0.0875587640220436),
                tolerance = 0.01
  )
})

test_that("Combiner equal works", {
  expect_equal( combiner_equal(test_model_list),
                c(0.333333333333333, 0.333333333333333, 0.333333333333333),
                tolerance = 0.01
  )
})
test_that("Combiner custom works", {
  expect_equal( combiner_custom(test_model_list, c(4,0,2)),
                c(0.666666666666667, 0, 0.333333333333333),
                tolerance = 0.01
  )
  expect_equal( combiner_custom(test_model_list),
                c(0.333333333333333, 0.333333333333333, 0.333333333333333),
                tolerance = 0.01
                )
})
test_that("Combiner oracle works", {
  expect_equal( combiner_oracle(test_model_list, oracle_weights = c(6,2,3)),
                c(0.5454545, 0.1818182, 0.2727273),
                tolerance = 0.01
  )
})
