# qs::qread("experiments/accuracies_1_2021-10-25_1.qs") -> accuracies1
# qs::qread("experiments/accuracies_2_2021-10-25_1.qs") -> accuracies2
# qs::qread("data/M_all_features.qs") -> feature_df
#
# accuracies <- dplyr::bind_rows(accuracies1, accuracies2) %>%
#   as.data.frame()
#
# rmse_resample <- accuracies %>%
#   dplyr::filter(!is.na(RMSE))

# linear
rmse_resample_linear <- rmse_resample %>%
  dplyr::group_by(key) %>%
  dplyr::mutate(weight = 1 / RMSE) %>%
  dplyr::mutate(weight = weight / sum(weight)) %>%
  dplyr::select(key, weight, .model)
# logit
rmse_resample_logit <- rmse_resample %>%
  dplyr::group_by(key) %>%
  dplyr::mutate(inv_rmse = 1 / RMSE) %>%
  dplyr::mutate(weight = exp(inv_rmse) / (1 + sum(exp(-inv_rmse)))) %>%
  dplyr::select(key, weight, .model)
# standardised
rmse_resample_standardised <- rmse_resample %>%
  dplyr::group_by(key) %>%
  dplyr::mutate(inv_rmse = 1 / RMSE) %>%
  dplyr::mutate(standardised_rmse = (inv_rmse - mean(inv_rmse)) / sd(inv_rmse)) %>%
  dplyr::mutate(weight = pnorm(standardised_rmse) / sum(pnorm(standardised_rmse))) %>%
  dplyr::select(key, weight, .model)

# rmse_resample %>%
#   as.data.frame() %>%
#   dplyr::filter(key == dplyr::first(key)) %>%
#   dplyr::select(-c("key", ".type")) %>%
#   tidyr::pivot_longer(cols = c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "RMSSE", "ACF1")) %>%
#   tidyr::pivot_wider(names_from = ".model") %>%
#   dplyr::select(-name) %>%
#   dplyr::filter(!is.nan(ets)) %>%
#   as.matrix() %>%
#   tsutils::nemenyi(plottype = "mcb")


# resampling scheme
resampled_best_model <- rmse_resample_standardised %>%
  dplyr::filter(!is.na(weight)) %>%
  dplyr::group_by(key) %>%
  dplyr::mutate(best_model = list(sample(.model,
    size = 25,
    replace = TRUE,
    prob = weight
  ))) %>%
  dplyr::select(key, best_model) %>%
  tidyr::unnest(cols = "best_model") %>%
  dplyr::ungroup()

# no resampling scheme
# resampled_best_model <- rmse_resample_standardised %>%
#   dplyr::filter( !is.na(weight) ) %>%
#   dplyr::group_by( key ) %>%
#   dplyr::filter( weight == max(weight) ) %>%
#   dplyr::mutate( best_model =  .model) %>%
#   dplyr::select(key, best_model) %>%
#   dplyr::ungroup()



# soothsayer_data <- resampled_best_model %>%
#   dplyr::full_join(feature_df, by = c("key" = "keys")) %>%
#   dplyr::select_if(~ !all(is.na(.x))) %>%
#   dplyr::select_if(~ !all(.x == .x[1])) %>%
#   tidyr::drop_na() %>%
#   dplyr::mutate(sample_type = sample(c("train", "valid", "test"),
#     length(best_model),
#     replace = TRUE,
#     prob = c(0.34, 0.33, 0.33)
#   ))
#
# soothsayer_train <- soothsayer_data %>%
#   dplyr::filter(sample_type == "train") %>%
#   dplyr::select(-sample_type) %>%
#   dplyr::select(-key)
# soothsayer_test <- soothsayer_data %>%
#   dplyr::filter(sample_type == "test") %>%
#   dplyr::select(-sample_type)
#
# ranger_x <- soothsayer_train %>%
#   dplyr::select(-best_model) %>%
#   as.matrix()
# ranger_y <- soothsayer_train %>%
#   dplyr::select(best_model) %>%
#   unlist() %>%
#   as.factor()
#
# remove( accuracies, accuracies1,
#         accuracies2, feature_df,
#         resampled_best_model, rmse_resample,
#         rmse_resample_linear, rmse_resample_logit,
#         rmse_resample_standardised, soothsayer_data)

soothsayer_model <- ranger::ranger(
  x = ranger_x,
  y = ranger_y,
  num.trees = 650,
  probability = TRUE,
  splitrule = "extratrees",
  num.random.splits = 3
)

preds <- predict(soothsayer_model, soothsayer_test %>%
  dplyr::select(-c("best_model", "key")) %>%
  as.matrix(), type = "response")

responses <- cbind(
  soothsayer_test %>%
    dplyr::select(key, best_model),
  preds[["predictions"]]
) %>%
  tidyr::pivot_longer(cols = c(
    "add_theta", "ar",
    "arima", "croston",
    "ets", "mult_theta",
    "nnetar"
  ))
confusion <- responses %>%
  dplyr::group_by(key) %>%
  dplyr::filter(value == max(value)) %>%
  dplyr::ungroup() %>%
  dplyr::select(best_model, name) %>%
  table()


