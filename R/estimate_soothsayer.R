# check_if_oracle_needed <- function( specials ) {
#   !is.null(specials$oracle[[1]])
# }
#
# check_if_rules_needed <- function( specials ) {
#   rules <- specials$rules[[1]]
#   if( is.null(rules)) {
#     return(FALSE)
#   }
#   rule_models <- purrr::map(rules, rlang::f_lhs)
#   rule_rhs <- purrr::map(rules, rlang::f_rhs)
#
#   is_true <- purrr::safely( .f = isTRUE, otherwise = FALSE )
#   # if all rules are TRUE - as would be if the user is using soothsayer just to
#   # ensemble, and they KNOW what models they want
#   all_rules_true <- all( purrr::map_lgl(rule_rhs, ~is_true(.x)[["result"]]) )
#   if(all_rules_true) {
#     return(FALSE)
#   }
#   return(TRUE)
# }
#
# check_if_features_needed <- function( specials ) {
#   check_if_rules_needed(specials) || check_if_oracle_needed(specials)
# }

# resolve_models <- function( .data, specials, ... ) {
#
#   if( check_if_features_needed(specials) ) {
#     feature_set <- specials$feature_set[[1]]
#     target <- tsibble::measured_vars(.data)
#     feature_df <- compute_features(.data, feature_set, values_from = target)
#   }
#
#   rule_models <- purrr::map(rules, rlang::f_lhs)
#   rule_rhs <- purrr::map(rules, rlang::f_rhs)
#
#   if (!is.null(rules)) {
#     fit_rules <- purrr::map(
#       rule_rhs,
#       ~ rlang::eval_tidy(.x, data = feature_df)
#     )
#     names(fit_rules) <- rlang::eval_bare(rule_models)
#     matched_models <- unlist(fit_rules)
#     matched_models_rule <- names(matched_models[which(matched_models)])
#   }
#   # we have to set this to NULL by default
#   oracle_weights <- NULL
#   matched_models_oracle <- NULL
#   if (!is.null(oracle)) {
#     matched_models_oracle <- ifelse(!is.null(oracle),
#                                     predict(oracle, feature_df),
#                                     NULL
#     )
#     if (oracle[["emit_type"]] == "weights") {
#       oracle_weights <- c(matched_models_oracle)
#       matched_models_oracle <- names(matched_models_oracle)
#     }
#   }
#   matched_models <- model_resolver(
#     rule_models = matched_models_rule,
#     oracle_models = matched_models_oracle,
#     choose_first = resolution[["which_model"]],
#     resolution = resolution[["precedence"]]
#   )
#   models_to_fit <- aliases[matched_models]
# }

# fit_soothsayer_models <- function( models_to_fit, target, .data, specials, oracle_weights = NULL ) {
#   # convert models to model definitions - basically partial application
#   target <- tsibble::measured_vars(.data)
#   model_defs <- purrr::map(models_to_fit, ~ .x(!!rlang::sym(target)))
#   # then proceed to fit models
#   model_fits <- fabletools::model(.data,
#                                   !!!model_defs,
#                                   .safely = TRUE
#   )
#   # finally, determine model weights
#   model_weights <- if (length(model_fits) > 1) {
#     specials$combiner[[1]](model_fits, oracle_weights)
#   } else {
#     1
#   }
#   # and compute the fitted values
#   fitted_vals <- purrr::map(
#     model_fits,
#     function(model_fit) {
#       as.data.frame(stats::fitted(model_fit[[1]]))[[".fitted"]]
#     }
#   )
#   # the weighed fitted values
#   weighed_fits <- purrr::map2(
#     fitted_vals, model_weights,
#     function(fit, weight) {
#       fit * weight
#     }
#   )
#   fit_val <- purrr::reduce( weighed_fits, .f = `+` )
#   # and, at long last, the residuals
#   resid <- unlist(as.data.frame(.data)[[target]]) - fit_val
#   # and return
#   return( list( model_fits = model_fits,
#                 model_weights = model_weights,
#                 fitted = fit_val,
#                 residuals = resid)
#   )
# }



estimate_soothsayer <- function( .data, specials, ... ) {

  feature_set <- specials$feature_set[[1]]
  target <- tsibble::measured_vars(.data)
  feature_df <- compute_features(.data, feature_set, values_from = target)

  aliases <- specials$model_aliases[[1]]
  rules <- specials$rules[[1]]
  oracle <- specials$oracle[[1]]
  resolution <- specials$resolution[[1]]
  # add any potential xreg
  .data <- dplyr::bind_cols(.data, specials$xreg[[1]])

  rule_models <- purrr::map(rules, rlang::f_lhs)
  rule_rhs <- purrr::map(rules, rlang::f_rhs)

  if (!is.null(rules)) {
    fit_rules <- purrr::map(
      rule_rhs,
      ~ rlang::eval_tidy(.x, data = feature_df)
    )
    names(fit_rules) <- rlang::eval_bare(rule_models)
    matched_models <- unlist(fit_rules)
    matched_models_rule <- names(matched_models[which(matched_models)])
  }
  # we have to set this to NULL by default
  oracle_weights <- NULL
  matched_models_oracle <- NULL
  if (!is.null(oracle)) {
    matched_models_oracle <- predict(oracle, feature_df)
    if (oracle[["emit_type"]] == "weights") {
      oracle_weights <- c(matched_models_oracle)
      matched_models_oracle <- names(matched_models_oracle)
    }
  }
  matched_models <- model_resolver(
    rule_models = matched_models_rule,
    oracle_models = matched_models_oracle,
    choose_first = resolution[["which_model"]],
    resolution = resolution[["precedence"]]
  )

  models_to_fit <- purrr::compact(aliases[matched_models])

  if( specials[[".noestimate"]][[1]] ) {
    return( c(list(
      model_fits = NULL,
      fit_rules = models_to_fit,
      model_weights = oracle_weights,
      residuals = NULL,
      fitted = NULL),
      specials = list(specials)
    ))
  }
  else {
    model_defs <- purrr::map(models_to_fit, ~ .x(!!rlang::sym(target)))
    model_fits <- fabletools::model(.data,
                                    !!!model_defs,
                                    .safely = TRUE
    )
    model_weights <- if (length(model_fits) > 1) {
      specials$combiner[[1]](model_fits, oracle_weights)
    } else {
      1
    }
    fitted_vals <- purrr::map(
      model_fits,
      function(model_fit) {
        as.data.frame(stats::fitted(model_fit[[1]]))[[".fitted"]]
      }
    )
    weighed_fits <- purrr::map2(
      fitted_vals, model_weights,
      function(fit, weight) {
        fit * weight
      }
    )
    fit_val <- purrr::reduce( weighed_fits, .f = `+` )
    resid <- unlist(as.data.frame(.data)[[target]]) - fit_val
  }
  return(list( model_fits = model_fits,
               fit_rules = models_to_fit,
               model_weights = model_weights,
               fitted = fit_val,
               residuals = resid ))
}
