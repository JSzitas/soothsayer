#' Create soothsayer from existing models
#'
#' @description Add a soothsayer to an existing **mable** wthout reestimating models which are available.
#' @param mable The mable to add soothsayer to.
#' @param ... The soothsayer to add - supports multiple soothsayers.
#' @return A mable.
#' @details This is intended to steamline adding soothsayer to existing mables - either as a model selector,
#' as ensembler, or anything else you wish. The models are not reestimated - but the missing models should be (although they
#' are only added to soothsayer currently, not the baseline mable).
#' @note Might be broken for multiple soothsayer/time series combinations.
#' @export
#'
add_soothsayer <- function(mable, ...) {
  .data <- extract_keyed_data(mable)

  soothsayers <- list(...)
  fixed_soothsayers <- purrr::map(soothsayers, function(soothsayer) {
    fml <- suppressWarnings(as.character(soothsayer[["formula"]]))
    fml[2] <- paste0(fml[2], " + .noestimate(TRUE)")
    fml <- paste0(fml[1], "(", fml[2], ")", collapse = "")

    soothsayer[["formula"]] <- rlang::as_quosure(stats::formula(fml))

    return(soothsayer)
  })

  new_mable <- fabletools::model(.data, !!!fixed_soothsayers)
  keys <- key_vars( new_mable )

  estimated_soothsayers <- list()
  # add fitted models to new_mable
  for ( sooth_mdl in fabletools::mable_vars(new_mable)) {
    #get fitting rules (ie the models to use)
    fit_rules <- new_mable[[sooth_mdl]][[1]][["fit"]][["fit_rules"]]
    # fit whatever isnt available yet
    model_fits <- resolve_models(fit_rules, mable, .data, ...)

    all_keys <- dplyr::full_join(model_fits, new_mable[,c(keys,sooth_mdl)], by = tsibble::key_vars(model_fits) )
    # split mable to individual rows - by key
    all_key_mables <- row_split(all_keys)
    # rebuild all soothsayers of this soothsayer model kind
    rebuilt_soothsayers <- purrr::map( all_key_mables, rebuild_soothsayer )
    final_soothsayers <- dplyr::bind_rows(rebuilt_soothsayers)
    final_soothsayers <- final_soothsayers[,c(keys,sooth_mdl)]
    final_soothsayers <- fabletools::mable( final_soothsayers, key = keys, model = sooth_mdl )
    estimated_soothsayers[[sooth_mdl]] <- final_soothsayers
  }
  fixed_new_mable <- purrr::reduce( estimated_soothsayers,
                                    dplyr::full_join,
                                    by = keys)

  return(dplyr::full_join(mable, fixed_new_mable, by = keys))
}

resolve_models <- function(fit_rules, models, .data, ...) {

  fitted_model_names <- intersect( names(models), names(fit_rules))
  keys <- tsibble::key_vars(models)

  already_fitted_models <- models[, c(keys, fitted_model_names)]
  unfitted_models <- names(fit_rules)[!(names(fit_rules) %in% names(models))]
  if( length(unfitted_models) > 0 ) {
    unfitted_models <- fit_rules[unfitted_models]

    target <- tsibble::measured_vars(.data)
    model_defs <- purrr::map(unfitted_models, ~ .x(!!rlang::sym(target)))

    model_fits <- fabletools::model(.data,
                                    !!!model_defs,
                                    .safely = TRUE
    )
    # bind old and new models, and reorder them so they appear in the same order
    # as they would if fitting was done right away.
    mdls <- dplyr::left_join(already_fitted_models, model_fits, by = keys)
  }
  else {
    mdls <- already_fitted_models
  }
  # convert to mable - note that the model variable seems redundant (as mdls is
  # named) but is nonetheless necessary (not using it results in obscure bugs)
  return( fabletools::mable( mdls, model = setdiff( colnames(mdls), keys), key = keys))
}
row_split <- function(df) {
  purrr::map( seq_len(nrow(df)), ~ df[.x,] )
}

extract_keyed_data <- function( mable ) {

  first_model <- rlang::sym( fabletools::mable_vars(mable)[1] )
  res <- dplyr::mutate( mable,
                        .extracted_data = !!first_model )

  data <- res[".extracted_data"][[1]]
  index <- tsibble::index_var(data[[1]][["data"]])
  keys <- mable[,tsibble::key_vars(mable)]
  keys <- row_split(keys)
  data <- purrr::map2(data, keys, ~tsibble::as_tibble(dplyr::bind_cols( .x[["data"]], .y)))
  data <- dplyr::bind_rows(data)
  tsibble::as_tsibble( data, key = tsibble::key_vars(mable), index = index )
}

rebuild_soothsayer <- function( new_mable ) {

  which_soothsayers <- purrr::map_lgl( new_mable, ~ class(.x[[1]][[1]]) == "soothsayer" )
  sooth_model <- new_mable[,which_soothsayers][[1]][[1]]

  mdls_to_use <- names(sooth_model[["fit"]][["fit_rules"]])
  mdls_to_use <- new_mable[mdls_to_use]
  .data <- sooth_model[["data"]]

  combiner <- sooth_model[["fit"]][["specials"]][["combiner"]]
  # resolve model weights
  model_weights <- if (length(mdls_to_use) > 1) {
    # if these are not NULL they are oracle weights
    oracle_weights <- sooth_model[["fit"]][["model_weights"]]
    combiner[[1]](mdls_to_use, oracle_weights)
  } else {
    1
  }
  # get fitted values and compute weighed fit
  fitted_vals <- purrr::map(
    mdls_to_use,
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
  fit_val <- purrr::reduce(weighed_fits, .f = `+`)
  # compute residuals of weighed fit
  resid <- unlist(as.data.frame(.data)[[tsibble::measured_vars(.data)]]) - fit_val

  new_mable[,which_soothsayers][[1]][[1]][["fit"]] <- structure(
    list(
      model_fits = mdls_to_use,
      fit_rules = sooth_model[["fit"]][["fit_rules"]],
      model_weights = model_weights,
      fitted_vals = fit_val,
      resid = resid
    ),
    class = "soothsayer"
  )
  return( new_mable )
}
