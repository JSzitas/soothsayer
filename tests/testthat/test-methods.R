
mdls <- readRDS("test_data/model.rds")

test_that("fitted() + residual() works", {
  fitted_values <- fitted(mdls)
  expect_equal(
    dim(fitted_values),
    c(8560, 4)
  )
  expect_equal(
    class(fitted_values),
    c("tbl_ts", "tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    fitted_values[1:20, 4],
    structure(list(.fitted = c(19880.1000131915, 19821.0832137571,
                               19467.5724438959, 18631.3560842367, 19479.1622359199, 20822.7494339158,
                               20157.1539926293, 22589.1650231917, 20156.7288958398, 19855.9930089501,
                               19659.373054862, 19963.045553601, 18808.1384016263, 17416.0659797446,
                               16462.5986598796, 15941.5494114049, 16619.0085481804, 16051.9268368823,
                               15727.4083110999, 16472.7586577653)),
    class = c(
      "tbl_df", "tbl",
      "data.frame"
    ), row.names = c(NA, -20L))
  )

  resids <- residuals(mdls)
  expect_equal(
    dim(resids),
    c(8560, 4)
  )
  expect_equal(
    class(resids),
    c("tbl_ts", "tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    resids[1:20, 4],
    structure(list(.resid = c(19.8999868084735, -521.083213757089,
                              -1867.57244389594, 1768.64391576326, 3020.83776408006, -1622.74943391577,
                              5542.8460073707, -5389.16502319175, -656.728895839846, -455.99300895008,
                              740.626945138002, -2563.04555360097, -2908.13840162632, -1716.06597974464,
                              -1762.59865987959, 1058.4505885951, -519.008548180396, -2151.9268368823,
                              3472.59168890008, -1172.75865776529)),
              class = c("tbl_df", "tbl",
                        "data.frame"),
              row.names = c(NA, -20L))
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
    structure(list(.mean = c(-2.79253370514767e-06, -1.18991515442811e-05,
                             -7.37098147660542e-06, -1.47078364501187e-07, -2.00435147025834e-06,
                             -4.87833441478355e-05, -5.01152539349988e-05, -5.04882115182165e-05,
                             -4.94023735577929e-05, -5.48759494555262e-05, -4.97353800755616e-05,
                             -5.3966598160109e-05, -5.0934824460923e-05, -4.84392593235011e-05,
                             -4.96804642768967e-05, -5.16643495386164e-05, -5.11482103097527e-05,
                             -3.83107294427835e-05, -3.7947325713074e-05, -3.78443260468172e-05
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
