
oracle_random <- function(...) {
  exprs <- rlang::dots_list(...)
  sample(exprs, 1)
}


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
  # return(aliases)
  rules <- specials$rules[[1]]

  rule_models <- purrr::map(rules, rlang::f_lhs)
  rule_rhs <- purrr::map(rules, rlang::f_rhs)

  if( !is.null(rules) ) {
    fit_rules <- dplyr::transmute( feature_df, !!!rule_rhs )
    colnames(fit_rules) <- rlang::eval_bare(rule_models)
    matched_models <- unlist(fit_rules)
    matched_models <- names(matched_models[ which(matched_models) ])
    models_to_fit <- aliases[ matched_models ]
  }

  model_defs <- purrr::map( models_to_fit, ~ .x( !!rlang::sym(target)))

  model_fits <- fabletools::model(.data,
                                  !!!model_defs, .safely = TRUE)


  return(model_fits)





  # Create S3 model object
  # It should be small, but contain everything needed for methods below
  structure(
    list( features = feature_df,
          rules = fit_rules ),
    class = "model_soothsayer"
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
  oracle = function(...,
                    fitted_oracle = oracle_random,
                    certainty = 0.1,
                    mix = FALSE) {
    # models <- rlang::dots_list(...)
    #
    # fitted_oracle(models)
  },
  model_aliases = function( aliases = soothsayer_alias_set ) {
    if( is.null(aliases) ) return( oothsayer_alias_set )
    aliases
  },
  feature_set = function(features = NULL) {
    if( is.null(features) ) return(soothsayer_feature_set)
    features
  },
  xreg = function(...) {
    # This model doesn't support exogenous regressors, time to error.
    # stop("Exogenous regressors aren't supported by `soothsayer()`")
  },
  .required_specials = c("feature_set", "model_aliases")
)

#' Soothsayer model
#'
#' Add the rest of your documentation here.
#' Typically this includes a "Specials" section
#'
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

soothsayer_model <-
  soothsayer(Value ~ # rule(.period > 12 -> AR) +
  # rule(.length < 50 -> ETS) +
  # alternatively just
  rules(
    AR ~ .period > 12,
    ETS ~ .length < 50
  ) +
    # for readability?
    oracle(ETS, AR, ARIMA,
      fitted_oracle = some_oracle,
      certainty = 0.1,
      mix = FALSE
    ) +
    model_aliases(NULL) )

ex_data <- tsibbledata::aus_livestock %>%
  as.data.frame() %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(count = sum(Count)) %>%
  dplyr::ungroup() %>%
  # dplyr::mutate(key = "aggreg") %>%
  tsibble::as_tsibble(index = "Month")#, key = "key")


fabletools::model(
  ex_data,
  soothsayer(count ~ rules(
    AR ~ .length > 12,
    ETS ~ .period < 12
  ))
) -> fitted
