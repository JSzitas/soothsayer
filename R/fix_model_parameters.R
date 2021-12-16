#' Create a fable model with fixed parameters
#'
#' @description Fix some parameters of a fable model
#' @param model The model to use - typically a **fable** model such as \link[fable]{AR}
#' @param ... The arguments to pass, the RHS of the formula to be passed to the underlying model.
#' @return A New, fable compatible model, with the parameters fixed.
#' @export
fix_model_parameters <- function( model, ... ) {
  new_model <- function( target ) {
    formula <- rlang::new_formula(lhs = NULL, rhs = rlang::expr(...))
    rlang::f_lhs(formula) <- rlang::enexpr( target )
    model(!!formula)
  }
}
