test_that("Generating a transformer works", {
  transformer <- generate_feature_transformer(list(mean, sd), "value")
  expect_equal(class(transformer), "function")
})

test_that("Computing features on a keyed tsibble works", {
  features <- compute_features(tsibbledata::aus_livestock)
  features_row_1 <- structure(list(
    Animal = structure(1L, .Label = c(
      "Bulls, bullocks and steers",
      "Calves", "Cattle (excl. calves)", "Cows and heifers", "Lambs",
      "Pigs", "Sheep"
    ), class = "factor"), State = structure(1L, .Label = c(
      "Australian Capital Territory",
      "New South Wales", "Northern Territory", "Queensland", "South Australia",
      "Tasmania", "Victoria", "Western Australia"
    ), class = "factor"),
    .entropy = 0.380324456320051, .lumpiness = 0.134676020083208,
    .nonlinearity = 1.3121121699299, .hurst = 0.998286598688578,
    .stability = 0.904786854405912, .box_pierce = 446.270483928651,
    .acf1 = 0.935435815310693, acf10 = 7.41229579761327, .diff1_acf1 = -0.204330607674713,
    diff1_acf10 = 0.117230446451437, .diff2_acf1 = -0.50172178878826,
    diff2_acf10 = 0.306413645837756, .season_acf1 = 0.829456128874994,
    .intermittent = 77.5, .nonzero_squared_cv = 0.0754095577520125,
    .zero_start_prop = 0, .zero_end_prop = 0.598039215686274,
    .spectral = 0.380324456320051, .arch_stat = 0.535749700627402,
    .longest_flat_spot = 305, .n_crossing_points = 7, .ljung_box = 448.900761829998,
    .unitroot_kpss = 5.20782358368463, .ndiffs = 3, .nsdiffs = 0,
    .unitroot_pp = -62.1962378742169, .shift_kl_max = 38.7358437646511,
    .shift_kl_index = 205, .shift_level_max = 2666.66666666667,
    .shift_level_index = 206, .shift_var_max = 3137500, .shift_var_index = 210,
    .positive = 1, .negative = 0, .zeros = 1, .continuous = 1,
    .count = 1, .length = 510, .period = 9, .boxcox_lambda_guerrero = 0.871014413543004,
    .DN_HistogramMode_5 = -0.401169312572142, .DN_HistogramMode_10 = -0.579116922370265,
    .CO_f1ecac = 69, .CO_FirstMin_ac = 7, .CO_HistogramAMI_even_2_5 = 0.638372695932082,
    .CO_trev_1_num = -0.0396121462351495, .MD_hrv_classic_pnn40 = 0.369351669941061,
    .SB_BinaryStats_mean_longstretch1 = 103, .SB_TransitionMatrix_3ac_sumdiagcov = 0.166666666666667,
    .PD_PeriodicityWang_th0_01 = 24, .CO_Embed2_Dist_tau_d_expfit_meandiff = 0.290151120086991,
    .IN_AutoMutualInfoStats_40_gaussian_fmmi = 6, .FC_LocalSimple_mean1_tauresrat = 0.00588235294117647,
    .DN_OutlierInclude_p_001_mdrmd = -0.433333333333333, .DN_OutlierInclude_n_001_mdrmd = 0.394117647058823,
    .SP_Summaries_welch_rect_area_5_1 = 0.932572764360831, .SB_BinaryStats_diff_longstretch0 = 4,
    .SB_MotifThree_quantile_hh = 1.05701030475471, .SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 = 0.130434782608696,
    .SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 = 0.847826086956522,
    .SP_Summaries_welch_rect_centroid = 0.0122718463030859, .FC_LocalSimple_mean3_stderr = 0.364755092938913,
    ...27 = NA_real_
  ), row.names = 1L, class = "data.frame")
  features_row_last <- structure(list(
    Animal = structure(7L, .Label = c(
      "Bulls, bullocks and steers",
      "Calves", "Cattle (excl. calves)", "Cows and heifers", "Lambs",
      "Pigs", "Sheep"
    ), class = "factor"), State = structure(8L, .Label = c(
      "Australian Capital Territory",
      "New South Wales", "Northern Territory", "Queensland", "South Australia",
      "Tasmania", "Victoria", "Western Australia"
    ), class = "factor"),
    .entropy = 0.68012629334534, .lumpiness = 0.13243720689628,
    .nonlinearity = 0.102114931758945, .hurst = 0.997528213048149,
    .stability = 0.576121677457582, .box_pierce = 397.844204503245,
    .acf1 = 0.844382878406661, acf10 = 2.27683736048223, .diff1_acf1 = 0.0629876674019543,
    diff1_acf10 = 0.540344618802788, .diff2_acf1 = -0.435795624915287,
    diff2_acf10 = 0.494660645361525, .season_acf1 = 0.692924977712636,
    .intermittent = 0, .nonzero_squared_cv = 0.211416304149337,
    .zero_start_prop = 0, .zero_end_prop = 0, .spectral = 0.68012629334534,
    .arch_stat = 0.470104484631444, .longest_flat_spot = 8, .n_crossing_points = 89,
    .ljung_box = 399.986991960175, .unitroot_kpss = 3.70526182486598,
    .ndiffs = 3, .nsdiffs = 0, .unitroot_pp = -125.671918465692,
    .shift_kl_max = 6.10729245550109, .shift_kl_index = 343,
    .shift_level_max = 167875, .shift_level_index = 11, .shift_var_max = 16525269015.1515,
    .shift_var_index = 5, .positive = 1, .negative = 0, .zeros = 0,
    .continuous = 1, .count = 0, .length = 558, .period = 12,
    .boxcox_lambda_guerrero = 0.247961343578616, .DN_HistogramMode_5 = -0.488064341251526,
    .DN_HistogramMode_10 = -0.224851736531793, .CO_f1ecac = 5,
    .CO_FirstMin_ac = 7, .CO_HistogramAMI_even_2_5 = 0.252261788600711,
    .CO_trev_1_num = -0.0101336539189059, .MD_hrv_classic_pnn40 = 0.9245960502693,
    .SB_BinaryStats_mean_longstretch1 = 24, .SB_TransitionMatrix_3ac_sumdiagcov = 0.0555555555555556,
    .PD_PeriodicityWang_th0_01 = 11, .CO_Embed2_Dist_tau_d_expfit_meandiff = 0.349972864938218,
    .IN_AutoMutualInfoStats_40_gaussian_fmmi = 6, .FC_LocalSimple_mean1_tauresrat = 0.0222222222222222,
    .DN_OutlierInclude_p_001_mdrmd = -0.362007168458781, .DN_OutlierInclude_n_001_mdrmd = 0.675627240143369,
    .SP_Summaries_welch_rect_area_5_1 = 0.862169111895202, .SB_BinaryStats_diff_longstretch0 = 7,
    .SB_MotifThree_quantile_hh = 1.84438113642833, .SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 = 0.152173913043478,
    .SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 = 0.869565217391304,
    .SP_Summaries_welch_rect_centroid = 0.147262155637031, .FC_LocalSimple_mean3_stderr = 0.711550838356364,
    ...27 = NA_real_
  ), row.names = 54L, class = "data.frame")

  expect_equal(features[1, ], features_row_1, tolerance = 0.05)
  expect_equal(features[54, ], features_row_last, tolerance = 0.05)
})

test_that("Computing features on an unkeyed tsibble works", {
  pelt <- tsibbledata::pelt
  pelt <- dplyr::select(pelt, Year, Lynx)

  features <- compute_features(pelt)
  expected <- structure(list(
    .entropy = 0.706583107025807, .lumpiness = 0.242960275179409,
    .nonlinearity = 0.175880583162613, .hurst = 0.985716059317091,
    .stability = 0.10367038279862, .box_pierce = 54.4593873885474,
    .acf1 = 0.773598608108594, acf10 = 2.47437850000159, .diff1_acf1 = 0.543446904897402,
    diff1_acf10 = 1.79047621510189, .diff2_acf1 = 0.015646804618152,
    diff2_acf10 = 0.283571922936066, .season_acf1 = 0.587332343222113,
    .intermittent = 0, .nonzero_squared_cv = 0.443193745539475,
    .zero_start_prop = 0, .zero_end_prop = 0, .spectral = 0.706583107025808,
    .arch_stat = 0.475619736928952, .longest_flat_spot = 4, .n_crossing_points = 18,
    .ljung_box = 56.274700301499, .unitroot_kpss = 0.0453980228052238,
    .ndiffs = 3, .nsdiffs = 0, .unitroot_pp = -34.9374916907255,
    .shift_kl_max = 1.14520835504965, .shift_kl_index = 23, .shift_level_max = 22153.5,
    .shift_level_index = 5, .shift_var_max = 646693288.888889,
    .shift_var_index = 15, .positive = 1, .negative = 0, .zeros = 0,
    .continuous = 1, .count = 1, .length = 91, .period = 10,
    .boxcox_lambda_guerrero = 0.182351816187663, .DN_HistogramMode_5 = -0.929291932720436,
    .DN_HistogramMode_10 = -1.13115296490138, .CO_f1ecac = 2,
    .CO_FirstMin_ac = 5, .CO_HistogramAMI_even_2_5 = 0.150069414038293,
    .CO_trev_1_num = -0.221360343874331, .MD_hrv_classic_pnn40 = 0.922222222222222,
    .SB_BinaryStats_mean_longstretch1 = 7, .SB_TransitionMatrix_3ac_sumdiagcov = 0.00777777777777778,
    .PD_PeriodicityWang_th0_01 = 9, .CO_Embed2_Dist_tau_d_expfit_meandiff = 0.188425015043199,
    .IN_AutoMutualInfoStats_40_gaussian_fmmi = 2, .FC_LocalSimple_mean1_tauresrat = 1,
    .DN_OutlierInclude_p_001_mdrmd = -0.0659340659340659, .DN_OutlierInclude_n_001_mdrmd = 0.0329670329670331,
    .SP_Summaries_welch_rect_area_5_1 = 0.269175107533045, .SB_BinaryStats_diff_longstretch0 = 6,
    .SB_MotifThree_quantile_hh = 1.91853800012346, .SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 = 0.235294117647059,
    .SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 = 0.558823529411765,
    .SP_Summaries_welch_rect_centroid = 0.638136007760469, .FC_LocalSimple_mean3_stderr = 1.09001747295146
  ), class = "data.frame", row.names = c(
    NA,
    -1L
  ))

  features_w_specified_val_col <- compute_features(pelt, values_from = "Lynx")

  expect_equal( features, expected, tolerance = 0.05 )
  expect_equal( features, features_w_specified_val_col, tolerance = 0.05 )
})
