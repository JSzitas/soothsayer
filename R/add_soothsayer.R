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
  .data <- mable[[1]][[1]][["data"]]

  soothsayers <- list(...)
  fixed_soothsayers <- purrr::map(soothsayers, function(soothsayer) {
    fml <- suppressWarnings(as.character(soothsayer[["formula"]]))
    fml[2] <- paste0(fml[2], " + .noestimate(TRUE)")
    fml <- paste0(fml[1], "(", fml[2], ")", collapse = "")

    soothsayer[["formula"]] <- rlang::as_quosure(stats::formula(fml))

    return(soothsayer)
  })

  new_mable <- fabletools::model(.data, !!!soothsayers)

  # add fitted models to new_mable
  for (col in colnames(new_mable)) {
    #get fitting rules (ie the models to use)
    fit_rules <- new_mable[[col]][[1]][["fit"]][["fit_rules"]]
    # fit whatever isnt available yet
    model_fits <- resolve_models(fit_rules, mable, .data, ...)
    # and access the combiner function
    combiner <- new_mable[[col]][[1]][["fit"]][["specials"]][["combiner"]]

    # resolve model weights
    model_weights <- if (length(model_fits) > 1) {
      # if these are not NULL they are oracle weights
      oracle_weights <- new_mable[[col]][[1]][["fit"]][["model_weights"]]
      combiner[[1]](model_fits, oracle_weights)
    } else {
      1
    }
    # get fitted values and compute weighed fit
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
    fit_val <- purrr::reduce(weighed_fits, .f = `+`)
    # compute residuals of weighed fit
    resid <- unlist(as.data.frame(.data)[[tsibble::measured_vars(.data)]]) - fit_val

    new_mable[[col]][[1]][["fit"]] <- structure(
      list(
        model_fits = model_fits,
        fit_rules = fit_rules,
        model_weights = model_weights,
        fitted_vals = fit_val,
        resid = resid
      ),
      class = "soothsayer"
    )
  }
  return(dplyr::bind_cols(mable, new_mable))
}

resolve_models <- function(fit_rules, models, .data, ...) {
  already_fitted_models <- models[, which(names(models) %in% names(fit_rules))]
  unfitted_models <- names(fit_rules)[!(names(fit_rules) %in% names(models))]
  unfitted_models <- fit_rules[unfitted_models]

  target <- tsibble::measured_vars(.data)
  model_defs <- purrr::map(unfitted_models, ~ .x(!!rlang::sym(target)))
  model_fits <- fabletools::model(.data,
    !!!model_defs,
    .safely = TRUE
  )
  # bind old and new models, and reorder them so they appear in the same order
  # as they would if fitting was done right away.
  mdls <- dplyr::bind_cols(already_fitted_models, model_fits)[names(fit_rules)]
  # convert to mable - note that the model variable seems redundant (as mdls is
  # named) but is nonetheless necessary (not using it results in obscure bugs)
  return( fabletools::as_mable(mdls, model = names(mdls)))
}
