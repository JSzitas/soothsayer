test_that("Instantiating a new oracle works", {

  ets_oracle <- new_soothsayer_oracle( oracle_name = "always_ets",
                                       trained_oracle = NULL,
                         predict = function( object, features, ... ) {
                           return("ets")
                         },
                         emits = "models"
  )

  expect_equal( class(ets_oracle), c("soothsayer_oracle", "always_ets"))
})
