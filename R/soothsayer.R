train_soothsayer <- function(.data, specials, ...) {

  feature_set <- specials$feature_set[[1]]

  target <- tsibble::measured_vars(.data)
  feature_df <- compute_features(.data, feature_set, values_from = target)

  aliases <- specials$model_aliases[[1]]
  rules <- specials$rules[[1]]
  oracle <- specials$oracle[[1]]

  rule_models <- purrr::map(rules, rlang::f_lhs)
  rule_rhs <- purrr::map(rules, rlang::f_rhs)

  matched_models <- NULL
  if( !is.null(rules) ) {
    fit_rules <- purrr::map( rule_rhs,
                             ~ rlang::eval_tidy( .x, data = feature_df))
    names(fit_rules) <- rlang::eval_bare(rule_models)
    matched_models <- unlist(fit_rules)
    matched_models <- names(matched_models[ which(matched_models) ])
  }
  if( !is.null(oracle)) {
    matched_models <- c( matched_models, predict(oracle, feature_df))
  }
  matched_models <- unique(matched_models)
  models_to_fit <- aliases[ matched_models ]
  model_defs <- purrr::map( models_to_fit, ~ .x( !!rlang::sym(target)))

  model_fits <- fabletools::model(.data,
                                  !!!model_defs, .safely = TRUE)
  model_weights <- if( length(model_fits) > 1 ) {
    specials$combiner[[1]](model_fits)
  } else {
   1
  }

  structure(
    list( features = feature_df,
          rules = fit_rules,
          models = model_fits,
          model_weights = model_weights),
    class = "soothsayer"
  )
}

specials_soothsayer <- fabletools::new_specials(
  rules = function(...) {
    rules <- rlang::dots_list(...)
    if (is.null(rules)) {
      return(NULL)
    }
    return(rules)
  },
  oracle = function( trained_oracle = random_oracle ) {
    return(trained_oracle)
  },
  model_aliases = function( aliases = soothsayer_alias_set ) {
    if( is.null(aliases) ) return( soothsayer_alias_set )
    aliases
  },
  feature_set = function(features = NULL) {
    if( is.null(features) ) return(soothsayer_feature_set)
    features
  },
  combiner = function(combiner = combiner_mean, prior_weights = NULL, metric = rmse, ...) {
    purrr::partial( .f = combiner, prior_weights = prior_weights, metric = metric)
  },
  xreg = function(...) {
    # This model doesn't support exogenous regressors, time to error.
    stop("Exogenous regressors aren't supported by `soothsayer()`")
  },
  .required_specials = c("feature_set", "model_aliases", "combiner")
)
#' Soothsayer model
#'
#' @description The main function of soothsayer - creates soothsayer models trainable and usable within fable and fabletools.
#' @param formula A soothsayer model formula (see details).
#' @param ... Additional arguments (see details).
#' @return A soothsayer model, analogous to other model objects within fable/fabletools.
#' @details Accepts and parses several model specials - notably rules, oracle, feature_set, model_aliases, combinator
#' @note Maybe some other day.
#' @export
soothsayer <- function(formula, ...) {
  # Create a model class which combines the training method, specials, and data checks
  model_soothsayer <- fabletools::new_model_class("soothsayer",
    # The training method (more on this later)
    train = train_soothsayer,
    # The formula specials (the next section)
    specials = specials_soothsayer,
    # Any checks of the unprocessed data, like gaps, ordered, regular, etc.
    check = function(.data) {
      if (!tsibble::is_regular(.data)) stop("Data must be regular")
    }
  )
  # Return a model definition which stores the user's model specification
  fabletools::new_model_definition(model_soothsayer, !!rlang::enquo(formula), ...)
}
