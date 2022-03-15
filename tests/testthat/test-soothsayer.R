test_that("Creating a model works", {
  soothsayer_model <- soothsayer(Value ~ rules() + oracle(random_oracle) + model_aliases(NULL) +
    feature_set(list(len = length)) + combiner(combiner_mean) +
    resolution(model = "all", rule_vs_oracle = "both") +
    second_value)
  expect_equal(class(soothsayer_model), c("mdl_defn", "R6"))
  expect_equal(
    names(soothsayer_model[["specials"]]),
    c(
      ".noestimate", "combiner", "feature_set", "model_aliases",
      "resolution", "xreg", "rules", "oracle"
    )
  )
})
