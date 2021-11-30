#' Soothsayer model aliases
#'
#' @description A set of soothsayer model aliases (for purposes of matching).
#' @rdname soothsayer_model_aliases
#' @export
soothsayer_alias_set <- list( "fable::AR" = fable::AR,
                              "AR" = fable::AR,
                              "ar" = fable::AR,
                              "fable::ARIMA" = fable::ARIMA,
                              "ARIMA" = fable::ARIMA,
                              "arima" = fable::ARIMA,
                              "fable::CROSTON" = fable::CROSTON,
                              "CROSTON" = fable::CROSTON,
                              "croston" = fable::CROSTON,
                              "fable::ETS" = fable::ETS,
                              "ETS" = fable::ETS,
                              "ets" = fable::ETS,
                              "fable::MEAN" = fable::MEAN,
                              "MEAN" = fable::MEAN,
                              "mean" = fable::MEAN,
                              "fable::NAIVE" = fable::NAIVE,
                              "NAIVE" = fable::NAIVE,
                              "naive" = fable::NAIVE,
                              "fable::NNETAR" = fable::NNETAR,
                              "NNETAR" = fable::NNETAR,
                              "nnetar" = fable::NNETAR,
                              "fable::RW" = fable::RW,
                              "RW" = fable::RW,
                              "rw" = fable::RW,
                              "fable::SNAIVE" = fable::SNAIVE,
                              "SNAIVE" = fable::SNAIVE,
                              "snaive" = fable::SNAIVE,
                              "fable::THETA" = fable::THETA,
                              "THETA" = fable::THETA,
                              "theta" = fable::THETA,
                              "fable::TSLM" = fable::TSLM,
                              "TSLM" = fable::TSLM,
                              "tslm" = fable::TSLM,
                              "fable::VAR" = fable::VAR,
                              "VAR" = fable::VAR,
                              "var" = fable::VAR
                              )

train_soothsayer <- function(.data, specials, ...) {

  feature_set <- specials$feature_set[[1]]

  target <- tsibble::measured_vars(.data)
  feature_df <- compute_features(.data, feature_set, values_from = target)

  aliases <- specials$model_aliases[[1]]
  rules <- specials$rules[[1]]
  oracle <- specials$oracle[[1]]

  rule_models <- purrr::map(rules, rlang::f_lhs)
  rule_rhs <- purrr::map(rules, rlang::f_rhs)

  if( !is.null(rules) ) {
    fit_rules <- purrr::map( rule_rhs,
                             ~ rlang::eval_tidy( .x, data = feature_df))
    names(fit_rules) <- rlang::eval_bare(rule_models)
    matched_models <- unlist(fit_rules)
    matched_models <- names(matched_models[ which(matched_models) ])
    models_to_fit <- aliases[ matched_models ]
  }

  model_defs <- purrr::map( models_to_fit, ~ .x( !!rlang::sym(target)))

  model_fits <- fabletools::model(.data,
                                  !!!model_defs, .safely = TRUE)
  structure(
    list( features = feature_df,
          rules = fit_rules,
          models = model_fits ),
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
  combinator = function(combinator = mean) {
    combinator
  },
  xreg = function(...) {
    # This model doesn't support exogenous regressors, time to error.
    stop("Exogenous regressors aren't supported by `soothsayer()`")
  },
  .required_specials = c("feature_set", "model_aliases", "combinator")
)
#' @importFrom generics generate
#' @export
generate.soothsayer <- function (x, new_data = NULL, h = NULL, specials = NULL, times = 1, bootstrap = FALSE, seed = NULL,
                       ...)
{
  combinator <- specials$combinator[[1]]
  generated_distrs <- purrr::imap( x[["models"]],
                                  function(model, name) {
                                    dplyr::bind_cols(
                                      generate(
                                      model[[1]],
                                      new_data = new_data,
                                      h = h,
                                      times = times,
                                      bootstrap = bootstrap,
                                      seed = seed,
                                      ...),
                                      model = name)
  })
  dplyr::bind_rows( purrr::map( generated_distrs,
                                dplyr::as_tibble ) ) %>%
    dplyr::group_by(.data$Month) %>%
    dplyr::summarise( .sim = combinator(.data$.sim),
                      .rep = unique(.data$.rep)
                      ) %>%
    tsibble::as_tsibble(index = "Month")

}
# merge_distribution_forecasts <- function( dist_fcsts, weights = NULL ) {
#
#   if(is.null(weights)) {
#     weights <- rep(1, length(dist_fcsts))/length(dist_fcsts)
#   }
#   ind_var <- tsibble::index_var(dist_fcsts[[1]])
#   dates <- dist_fcsts[[1]][[ind_var]]
#
#   distrs <- purrr::map( dist_fcsts,  )
#
#
#
#
#   distributional::dist_mixture()
#
#
#
# }

#' @importFrom fabletools forecast
#' @export
forecast.soothsayer <- function( object,
                                 new_data = NULL,
                                 specials = NULL,
                                 bootstrap = FALSE,
                                 times = 100,
                                 ... ) {
  purrr::map( object[["models"]],
              function(.x) {
                fabletools::forecast(
                .x[[1]],
                new_data = new_data,
                bootstrap = bootstrap,
                times = times, ...)
              }
              )[[1]]
  # global_res_fcst <<- res
  # dplyr::bind_rows(res)#[[1]]
}
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
