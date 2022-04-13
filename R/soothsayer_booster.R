#' @importFrom rlang `:=`
train_soothsayer_booster <- function(.data, specials, ...) {

  .data <- dplyr::bind_cols(.data, specials$xreg[[1]])
  models <- specials$models[[1]]
  target_val <- tsibble::measured_vars(.data)

  original_vals <- .data[[target_val]]
  model_names <- purrr::map_chr( models, ~.x[["model"]] )

  n_models <- length(models)
  est_models <- list()
  index <- 1
  repeat{

    model_def <- models[[index]]
    new_model <- fabletools::model( .data,
                                    model = model_def,
                                    .safely = TRUE )
    .data <- dplyr::mutate(.data, !!rlang::sym(target_val) := residuals( new_model )[[".resid"]])
    est_models[[index]] <- new_model
    index <- index + 1
    if( index > n_models ) {
      break;
    }
  }
  names(est_models) <- model_names

  fitted_vals <-  purrr::reduce( purrr::map( est_models, ~fitted(.)[[".fitted"]] ), `+` )
  resids <- original_vals - fitted_vals

  structure(
    list( model_fits = est_models,
          fitted = fitted_vals,
          residuals = resids),
    class = "soothsayer_booster"
  )
}

specials_soothsayer_booster <- fabletools::new_specials(
  models = function(...) {
    e <- rlang::env(
      rlang::caller_env(),
      `%>%` = function( left, right ) { list(left, right ) } )
    eval( rlang::expr(...) , envir = e)
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
  .required_specials = c("models")
)
#' Soothsayer boosting model
#'
#' @description Boosting - stacking multiple models by modelling the residuals of the first model by the second one.
#' @param formula A soothsayer model formula (see details).
#' @param ... Additional arguments (see details).
#' @return A soothsayer model, analogous to other model objects within fable/fabletools.
#' @details Accepts and parses several model specials.
#' @note Maybe some other day.
#' @export
soothsayer_booster <- function(formula, ...) {
  # Create a model class which combines the training method, specials, and data checks
  model_soothsayer_booster <- fabletools::new_model_class("soothsayer_booster",
                                                  # The training method (more on this later)
                                                  train = train_soothsayer_booster,
                                                  # The formula specials (the next section)
                                                  specials = specials_soothsayer_booster,
                                                  # Any checks of the unprocessed data, like gaps, ordered, regular, etc.
                                                  check = function(.data) {
                                                    if (!tsibble::is_regular(.data)) stop("Data must be regular")
                                                  }
  )
  # Return a model definition which stores the user's model specification
  fabletools::new_model_definition(model_soothsayer_booster, !!rlang::enquo(formula), ...)
}
