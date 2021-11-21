# I refuse to add a checkmate dependency for something this trivial.
fail_if_not_cond <- function( cond, msg, type = "input_field_null" ) {
  if( cond ){
    rlang::abort(message = msg, class = type)
  }
}
