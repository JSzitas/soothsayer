
# soothsayer models should be easy to define (i.e. it should be easy to wrap an
# existing thing in such a way that we can use it for metalearning)
add_rule <- function( ruleset, rule ) {
  c( ruleset, rlang::enexpr( rule ))
}

replace_rule <- function( ruleset, new_rule, which_rule ) {
  ruleset[[which_rule]] <- rlang::enexpr( new_rule )
}
remove_rule <- function( ruleset, which_rule ) {
  if( is.character(which_rule) ) {
    which_rule <- which( names(ruleset) == which_rule)
  }
  ruleset[ -which_rule ]
}


# ruleset enables bonobos to use "rules" rather than learning
ruleset <- function( ... ) {
  rule_list <- rlang::enexprs(...)
  return(rule_list)
}
