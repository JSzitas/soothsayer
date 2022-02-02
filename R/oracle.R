#' Define a soothsayer oracle
#'
#' @description Create a new soothsayer oracle
#' @param oracle_name = NULL,
#' @param trained_oracle A trained oracle (if needed) for making predictions.
#' @param predict The prediction for your oracle.
#' @param emits What the model emits (either **"models"** or **"weights"**) where **models**
#' is a variable length character vector, e.g. **c("ets","ar")**, or a named vector of weights,
#' e.g. **c("ets" = 0.3, "ar" = 0.7)**.
#' @param ... Additional arguments (currently unimplemented).
#' @param object The soothsayer oracle to fit/use for prediction.
#' @param features The features to use for the predict function (analogous to newx/newdata in
#' other predict functions).
#' @return A soothsayer oracle
#' @export
#' @rdname soothsayer_oracles
new_soothsayer_oracle <- function( oracle_name = NULL,
                                   trained_oracle = NULL,
                                   predict = function(...){ # how the oracle generates labels
                                     rlang::abort("This oracle must have a prediction method.")
                                   },
                                   emits = "models", # or "weights"
                                   ... ) {

  fail_if_cond( is.null(predict), "A prediction function for the oracle must be provided.")
  fail_if_cond( !(emits %in% c("models","weights")), "Emit must be either 'models' or 'weights'." )

  structure( list( trained_oracle = trained_oracle,
                   predict = predict,
                   emit_type = emits),
             class = c( "soothsayer_oracle", oracle_name))
}

#' @importFrom stats predict
#' @export
#' @rdname soothsayer_oracles
predict.soothsayer_oracle <-function( object, features, ... ) {
  object$predict( object$trained_oracle, features )
}
