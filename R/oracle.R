#' Defne a soothsayer oracle
#'
#' @description Create a new soothsayer oracle
#' @param oracle_name = NULL,
#' @param feature_data = NULL, # either the full feature data,
#' @param series_data = NULL, # or series data
#' @param feature_set = soothsayer_feature_set, # always a feature set - this is necessary as
#' this will be checked against
#' @param forecast_accuracies = NULL, # you can have forecast accuracies
#' @param forecaster = soothsayer_forecaster, # use our default forecastng engine
#' @param train The training function for your oracle.
#' @param predict The prediction for your oracle.
#' @param ... Additional arguments (currently unimplemented).
#' @return A soothsayer oracle
#' @export
new_soothsayer_oracle <- function( oracle_name = NULL,
                                   feature_data = NULL, # either the full feature data,
                                   series_data = NULL, # or series data
                                   feature_set = soothsayer_feature_set, # always a feature set - this is necessary as
                                   # this will be checked against
                                   forecast_accuracies = NULL, # you can have forecast accuracies
                                   forecaster = soothsayer_forecaster, # use our default forecastng engine
                                   train = function(accuracies, features, ...){ # how we train the oracle
                                     rlang::abort("This oracle must have a training method.")
                                   },
                                   predict = function(...){ # how the oracle generates labels
                                     rlang::abort("This oracle must have a prediction method.")
                                   },
                                   emits = "models", # or "weights"
                                   ... ) {

  fail_if_not_cond( is.null(feature_data) & is.null(series_data),
                    "Must provide feature or series data, but both are null.")
  fail_if_not_cond( is.null(forecast_accuracies) & is.null(forecaster),
                    "Either forecast accuracies or a forecaster must be provided.")
  fail_if_not_cond( is.null(forecaster) & is.null(forecast_accuracies),
                    "Both forecast accuracies and forecaster cannot be missng at the same time - provide either of them." )
  fail_if_not_cond( is.null(train), "A training function for the oracle must be provided." )
  fail_if_not_cond( is.null(predict), "A prediction function for the oracle must be provided.")

  # finally, if we did not fail, we get to compute the features
  if( is.null(feature_data)){
    feature_data <- compute_features( series_data, feature_set )
  }
  if( is.null(forecast_accuracies) ) {
    forecast_accuracies <- forecaster( series_data )[["accuracies"]]
  }
  structure( list( feature_set = feature_set,
                   feature_data = feature_data,
                   forecast_accuracies = forecast_accuracies,
                   fitted_oracle = NULL,
                   train = train,
                   predict = predict,
                   emit_type = emits),
             class = c( "soothsayer_oracle", oracle_name))
}
#' @importFrom generics fit
#' @export
fit.soothsayer_oracle <- function( object, ... ) {
  object[["fitted_oracle"]] <- object$train( object[["forecast_accuracies"]],
                                             object[["feature_data"]]  )
  return(object)
}
#' @importFrom stats predict
#' @export
predict.soothsayer_oracle <-function( object, features, ... ) {
  object$predict( object$fitted_oracle, features )
}
# add more generics - e.g. for cv
