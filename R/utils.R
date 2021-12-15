# I refuse to add a checkmate dependency for something this trivial.
fail_if_not_cond <- function( cond, msg, type = "input_field_null" ) {
  if( cond ){
    rlang::abort(message = msg, class = type)
  }
}
# this is a bit awful, but restructuring is hard
model_resolver <- function( rule_models = NULL,
                            oracle_models = NULL,
                            which_model = "all",
                            resolution = "both", ... ) {
  if( resolution == "both" ) {
    # if all, perform union
    if( which_model == "all" ) {
      return(unique(c(rule_models, oracle_models)))
    }
    # otherwise find the intersection and throw the first model
    return( intersect( rule_models, oracle_models)[1] )
  }
  else if(resolution == "rule") {
    models <- rule_models
  }
  else {
    models <- oracle_models
  }
  if(which_model == "first") {
    models <- models[1]
  }
  return(models)
}
