
mdls <- readRDS("test_data/model.rds")

test_that("fitted() + residual() works", {
  fitted_values <- fitted(mdls)
  expect_equal(
    dim(fitted_values),
    c(8544, 4)
  )
  expect_equal(
    class(fitted_values),
    c("tbl_ts", "tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    fitted_values[1:20, 4],
    structure(list(.fitted = c(
      19880.1000131915, 19821.0842495872,
      19467.57511356, 18631.3510504658, 19479.159363344, 20822.756972592,
      20157.147045408, 22589.1812223258, 20156.7259680731, 19855.9904761625,
      19659.369645452, 19963.0487735276, 18808.1372337927, 17416.0622958949,
      16462.5983359534, 15941.5384050591, 16619.001185808, 16051.9308213761,
      15727.3885153754, 16472.7743643479
    )), class = c(
      "tbl_df", "tbl",
      "data.frame"
    ), row.names = c(NA, -20L))
  )

  resids <- residuals(mdls)
  expect_equal(
    dim(resids),
    c(8544, 4)
  )
  expect_equal(
    class(resids),
    c("tbl_ts", "tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    resids[1:20, 4],
    structure(list(.resid = c(
      19.8999868085259, -521.08424958724,
      -1867.57511355996, 1768.6489495342, 3020.84063665599, -1622.75697259203,
      5542.85295459196, -5389.1812223258, -656.725968073115, -455.990476162503,
      740.630354548005, -2563.0487735276, -2908.13723379267, -1716.06229589487,
      -1762.59833595337, 1058.46159494095, -519.00118580804, -2151.93082137612,
      3472.61148462461, -1172.77436434795
    )), class = c(
      "tbl_df", "tbl",
      "data.frame"
    ), row.names = c(NA, -20L))
  )
})


test_that("forecasting works", {
  fcst <- forecast(mdls)
  expect_equal(
    dim(fcst),
    c(384L, 5L)
  )
  expect_equal(
    class(fcst),
    c("fbl_ts", "tbl_ts", "tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    fcst[1:20, 5],
    structure(list(.mean = c(1.08258777395275e-05, 2.14840054716735e-06,
                             -6.95028354393005e-06, -2.42609937977251e-06, 4.79186168094897e-06,
                             2.93564256185413e-06, -4.38036193825494e-05, -4.51344928033291e-05,
                             -4.55069921278661e-05, -4.44221216415251e-05, -4.98908500976988e-05,
                             -4.4754777815371e-05, -4.77911234487786e-05, -4.54098072470184e-05,
                             -4.29165234075329e-05, -4.41565824894765e-05, -4.61387502858717e-05,
                             -4.56229286059708e-05, -3.27969036509371e-05, -3.24337980300249e-05
    )), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA,
                                                                -20L))
  )
})

test_that("generating works", {
  skip("not yet implemented")
  # generate( fitted )
})

test_that("tidy and glance work", {
  skip("not yet implemented")
  # tidy()
  # glance
})
