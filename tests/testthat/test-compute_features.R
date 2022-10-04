test_that("Generating a transformer works", {
  transformer <- generate_feature_transformer(list(mean, sd), "value")
  expect_equal(class(transformer), "function")
})

test_that("Computing features on a keyed tsibble works", {
  features <- compute_features(tsibbledata::aus_livestock[1:2000,])

  expect_equal( as.character(unlist(features[1,1:2])),
                c("Bulls, bullocks and steers",
                  "Australian Capital Territory" )
                )
  expect_equal(dim(features), c(4,64), tolerance = 0.05)
})

test_that("Computing features on an unkeyed tsibble works", {
  pelt <- tsibbledata::pelt
  pelt <- dplyr::select(pelt, Year, Lynx)

  features <- compute_features(pelt)

  expect_equal( dim(features), c(1,62))
  expect_equal(colnames(features),
               c(".entropy", ".lumpiness", ".nonlinearity", ".hurst", ".stability",
                 ".box_pierce", ".acf1", "acf10", ".diff1_acf1", "diff1_acf10",
                 ".diff2_acf1", "diff2_acf10", ".season_acf1", ".intermittent",
                 ".nonzero_squared_cv", ".zero_start_prop", ".zero_end_prop",
                 ".spectral", ".arch_stat", ".longest_flat_spot", ".n_crossing_points",
                 ".ljung_box", ".unitroot_kpss", ".ndiffs", ".nsdiffs", ".unitroot_pp",
                 ".shift_kl_max", ".shift_kl_index", ".shift_level_max", ".shift_level_index",
                 ".shift_var_max", ".shift_var_index", ".positive", ".negative",
                 ".zeros", ".continuous", ".count", ".length", ".period", ".boxcox_lambda_guerrero",
                 ".DN_HistogramMode_5", ".DN_HistogramMode_10", ".CO_f1ecac",
                 ".CO_FirstMin_ac", ".CO_HistogramAMI_even_2_5", ".CO_trev_1_num",
                 ".MD_hrv_classic_pnn40", ".SB_BinaryStats_mean_longstretch1",
                 ".SB_TransitionMatrix_3ac_sumdiagcov", ".PD_PeriodicityWang_th0_01",
                 ".CO_Embed2_Dist_tau_d_expfit_meandiff", ".IN_AutoMutualInfoStats_40_gaussian_fmmi",
                 ".FC_LocalSimple_mean1_tauresrat", ".DN_OutlierInclude_p_001_mdrmd",
                 ".DN_OutlierInclude_n_001_mdrmd", ".SP_Summaries_welch_rect_area_5_1",
                 ".SB_BinaryStats_diff_longstretch0", ".SB_MotifThree_quantile_hh",
                 ".SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1", ".SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1",
                 ".SP_Summaries_welch_rect_centroid", ".FC_LocalSimple_mean3_stderr"
               ))

  features_w_specified_val_col <- compute_features(pelt, values_from = "Lynx")
  expect_equal( features, features_w_specified_val_col, tolerance = 0.05 )
})
