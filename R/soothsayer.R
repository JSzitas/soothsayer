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
  combiner = function(combiner = combiner_equal, prior_weights = NULL, metric = rmse, ...) {
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
    else rlang::base_env()
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
