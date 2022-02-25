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
    matched_models_oracle <- ifelse(!is.null(oracle),
                                    predict(oracle, feature_df),
                                    NULL
    )
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

  models_to_fit <- aliases[matched_models]
  model_defs <- purrr::map(models_to_fit, ~ .x(!!rlang::sym(target)))

  if( specials[[".noestimate"]][[1]] ) {
    return( c(list(
      model_fits = NULL,
      fit_rules = models_to_fit,
      model_weights = oracle_weights,
      resid = NULL,
      fitted_vals = NULL),
      specials = list(specials)
    ))
  }
  else {
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
               fitted_vals = fit_val,
               resid = resid ))
}


train_soothsayer <- function(.data, specials, ...) {

  structure(
    estimate_soothsayer(.data, specials),
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
  oracle = function(trained_oracle = random_oracle) {
    return(trained_oracle)
  },
  model_aliases = function(...) {
    aliases <- list(...)
    if ( length(aliases) < 1) {
      return(soothsayer_alias_set)
    }
    aliases
  },
  feature_set = function(features = NULL) {
    if (is.null(features)) {
      return(soothsayer_feature_set)
    }
    features
  },
  combiner = function(combiner = combiner_mean, prior_weights = NULL, metric = rmse, ...) {
    purrr::partial(.f = combiner, prior_weights = prior_weights, metric = metric, ...)
  },
  resolution = function(model = "all", rule_vs_oracle = "both") {
    # add argument checking
    fail_if_cond(
      !(model %in% c("first", "all")),
      "resolution model must be 'all' or 'first'."
    )
    fail_if_cond(
      !(rule_vs_oracle %in% c("rule", "oracle", "both")),
      "resolution rule_vs_oracle must be 'rule' or 'oracle' or 'both'."
    )
    list(
      which_model = model,
      precedence = rule_vs_oracle
    )
  },
  xreg = function(...) {
    dots <- rlang::enexprs(...)
    env <- purrr::map(rlang::enquos(...), rlang::get_env)
    env[purrr::map_lgl(env, purrr::compose(rlang::is_empty, rlang::env_parents))] <- NULL
    env <- if (!rlang::is_empty(env))
      rlang::get_env(env[[1]])
    else base_env()
    constants <- purrr::map_lgl(dots, inherits, "numeric")
    constant_given <- any(purrr::map_lgl(dots[constants], `%in%`,
                                         -1:1))
    model_formula <- rlang::new_formula(lhs = NULL, rhs = purrr::reduce(dots,
                                                                        function(.x, .y) rlang::call2("+", .x, .y)))
    xreg <- stats::model.frame(model_formula, data = env, na.action = stats::na.pass)
    list(xreg = if (NCOL(xreg) == 0) NULL else xreg)
  },
  .noestimate = function(no_estimate = FALSE){ return(no_estimate) },
  .required_specials = c("feature_set", "model_aliases", "combiner", "resolution",".noestimate")
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
