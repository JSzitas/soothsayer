# nocov start
#' @rdname soothsayer_features
#' @param x A vector to apply measures to.
#' @export
entropy <- function(x) {
  spec <- try(stats::spec.ar(stats::na.contiguous(stats::as.ts(x)),
    plot = FALSE,
    method = "burg",
    n.freq = ceiling(length(x) / 2 + 1)
  ))
  fx <- c(rev(spec$spec[-1]), spec$spec) / length(x)
  fx <- fx / sum(fx)
  prior.fx <- rep(1 / length(fx), length = length(fx))
  prior.weight <- 0.001
  fx <- (1 - prior.weight) * fx + prior.weight * prior.fx
  entropy <- pmin(1, -sum(fx * log(fx, base = length(x))))
  return(c(.entropy = entropy))
}
#' @rdname soothsayer_features
#' @export
lumpiness <- function(x) {
  width = ifelse(stats::frequency(x) > 1, stats::frequency(x), 10)
  x <- as.numeric(scale(as.matrix(x), center = TRUE, scale = TRUE))
  nr <- length(x)
  lo <- seq(1, nr, by = width)
  up <- seq(width, nr + width, by = width)
  nsegs <- nr / width
  varx <- purrr::map_dbl(seq_len(nsegs), function(idx) {
    stats::var(x[lo[idx]:up[idx]],
      na.rm = TRUE
    )
  })
  if (length(x) < 2 * width) {
    lumpiness <- 0
  } else {
    lumpiness <- stats::var(varx, na.rm = TRUE)
  }
  return(c(.lumpiness = lumpiness))
}
#' @rdname soothsayer_features
#' @export
stability <- function(x) {
  width = ifelse(stats::frequency(x) > 1, stats::frequency(x), 10)
  x <- as.numeric(scale(x, center = TRUE, scale = TRUE))
  nr <- length(x)
  lo <- seq(1, nr, by = width)
  up <- seq(width, nr + width, by = width)
  nsegs <- nr / width
  meanx <- purrr::map_dbl(seq_len(nsegs), function(idx) {
    mean(x[lo[idx]:up[idx]],
      na.rm = TRUE
    )
  })
  if (length(x) < 2 * width) {
    stability <- 0
  } else {
    stability <- stats::var(meanx, na.rm = TRUE)
  }
  return(c(.stability = stability))
}
#' @rdname soothsayer_features
#' @export
nonlinearity <- function(x) {
  if( requireNamespace("tseries", quietly = TRUE) ) {
    X2 <- tryCatch(tseries::terasvirta.test(stats::as.ts(x),
                                            type = "Chisq")$statistic,
                   error = function(e) NA
    )
    return(c(.nonlinearity = 10 * unname(X2) / length(x)))

  }
  NA
}
#' @rdname soothsayer_features
#' @export
hurst <- function(x) {
  if(requireNamespace("fracdiff",quietly = TRUE)) {
    return(c(.hurst = suppressWarnings(fracdiff::fracdiff(
      stats::as.ts(stats::na.contiguous(unlist(x))),
      0, 0
    )[["d"]] + 0.5)))
  }
  NA
}
#' @rdname soothsayer_features
#' @export
unitroot_pp <- function(x) {
  if(requireNamespace("urca", quietly = TRUE)) {
    result <- urca::ur.pp(x,
                          type = "Z-alpha",
                          model = "trend",
                          lags = "short"
    )
    return(c(.unitroot_pp = result@teststat))
  }
  NA
}
#' @rdname soothsayer_features
#' @export
box_pierce <- function(x) {
  out <- stats::Box.test(x,
    lag = 1, type = "Box-Pierce",
    fitdf = 0
  )
  c(.box_pierce = unname(out$statistic))
}
#' @rdname soothsayer_features
#' @export
unitroot_kpss <- function (x)
{
  if(requireNamespace("urca", quietly = TRUE)) {
    result <- urca::ur.kpss(x, type = "mu", lags = "short")
    return(c(.unitroot_kpss = result@teststat))
  }
  NA
}
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
#' @rdname soothsayer_features
#' @export
longest_flat_spot <- function (x)
{
  cutx <- cut(x, breaks = 10, include.lowest = TRUE, labels = FALSE)
  rlex <- rle(cutx)
  return(c(.longest_flat_spot = max(rlex$lengths)))
}
#' @rdname soothsayer_features
#' @export
n_crossing_points <- function (x)
{
  midline <- stats::median(x, na.rm = TRUE)
  ab <- x <= midline
  lenx <- length(x)
  p1 <- ab[1:(lenx - 1)]
  p2 <- ab[2:lenx]
  cross <- (p1 & !p2) | (p2 & !p1)
  c(.n_crossing_points = sum(cross, na.rm = TRUE))
}
#' @rdname soothsayer_features
#' @export
ljung_box <- function (x)
{
  lag = 1
  dof = 0
  out <- stats::Box.test(x, lag = lag, type = "Ljung-Box",
                         fitdf = dof)
  c(.ljung_box = unname(out$statistic))
}
#' @rdname soothsayer_features
#' @export
acf_features <- function(x) {
  period <- period(x)
  lag_max <- max( period, 10L)
  acfx <- stats::acf(x, lag.max = lag_max ,
                     plot = FALSE, na.action = stats::na.pass )
  acfdiff1x <- stats::acf(diff(x, differences = 1), lag.max = lag_max,
                          plot = FALSE, na.action = stats::na.pass)
  acfdiff2x <- stats::acf(diff(x, differences = 2), lag.max = lag_max,
                          plot = FALSE, na.action = stats::na.pass)
  acf_1 <- acfx$acf[2L]
  sum_of_sq_acf10 <- sum((acfx$acf[2L:11L])^2)
  diff1_acf1 <- acfdiff1x$acf[2L]
  diff1_acf10 <- sum((acfdiff1x$acf[-1L])^2)
  diff2_acf1 <- acfdiff2x$acf[2L]
  diff2_acf10 <- sum((acfdiff2x$acf[-1L])^2)
  output <- c(
    .acf1 = unname(acf_1), acf10 = unname(sum_of_sq_acf10),
    .diff1_acf1 = unname(diff1_acf1), diff1_acf10 = unname(diff1_acf10),
    .diff2_acf1 = unname(diff2_acf1), diff2_acf10 = unname(diff2_acf10)
  )
  if (period > 1) {
    output <- c(output, .season_acf1 = unname(acfx$acf[period +
      1L]))
  }
  return(output)
}
#' @rdname soothsayer_features
#' @export
intermittent <- function(x) {
  rle <- rle(x)
  nonzero <- x[x != 0]
  c(.intermittent = if (length(nonzero) == length(x)) {
    0
  } else {
    mean(rle$lengths[rle$values ==
      0])
  }, .nonzero_squared_cv = (stats::sd(nonzero, na.rm = TRUE) / mean(nonzero,
    na.rm = TRUE
  ))^2, .zero_start_prop = if (rle$values[1] !=
    0) {
    0
  } else {
    rle$lengths[1] / length(x)
  }, .zero_end_prop = if (rle$values[length(rle$values)] !=
    0) {
    0
  } else {
    rle$lengths[length(rle$lengths)] / length(x)
  })
}
#' @rdname soothsayer_features
#' @export
arch_stat <- function (x)
{
  if (length(x) <= 13) {
    return(c(arch_stat = NA_real_))
  }
  # use demean as default
  x <- x - mean(x, na.rm = TRUE)
  mat <- stats::embed(x^2, 13)
  fit <- stats::lm(mat[, 1] ~ mat[, -1])
  arch_stat <- suppressWarnings(summary(fit)$r.squared)
  c( .arch_stat = if (is.nan(arch_stat)) 1 else arch_stat)
}
#' @rdname soothsayer_features
#' @export
spectral <- function (x)
{
  period <- period(x)
  spec <- try(stats::spec.ar(na.contiguous(stats::ts(x, frequency = period)),
                             plot = FALSE, method = "burg", n.freq = ceiling(length(x)/2 +
                                                                               1)))
  if (inherits(spec, "try-error")) {
    entropy <- NA
  }
  else {
    fx <- c(rev(spec$spec[-1]), spec$spec)/length(x)
    fx <- fx/sum(fx)
    prior.fx = rep(1/length(fx), length = length(fx))
    prior.weight = 0.001
    fx <- (1 - prior.weight) * fx + prior.weight * prior.fx
    entropy <- pmin(1, -sum(fx * log(fx, base = length(x))))
  }
  return(c(.spectral = entropy))
}
#' @rdname soothsayer_features
#' @export
catch22_feat <- function( x ) {
  if(requireNamespace("Rcatch22",quietly = TRUE) && length(x) > 10) {
    res <- Rcatch22::catch22_all(x)
    return( stats::setNames( res[["values"]], paste0(".",res[["names"]])))
  }
  NA
}
#' @rdname soothsayer_features
#' @export
unitroot_ndiffs <- function( x ) {
  differences <- 0:3
  diff <- function(x, differences) {
    if (differences == 0)
      return(x)
    base::diff(x, differences = differences)
  }
  keep <- purrr::map_lgl(differences, function(.x) {
    dx <- diff(x, differences = .x)
    !all(is.na(dx))
  })
  differences <- differences[keep]
  keep <- purrr::map_lgl(differences[-1] - 1, function(.x) {
    unitroot_kpss(diff(x, differences = .x)) < 0.05
  })
  c(.ndiffs = max(differences[c(TRUE, keep)], na.rm = TRUE))
}
`%||%` <- function (x, y)
{
  if (is.null(x))
    y
  else x
}
estimate_stl <- function (y, trend.args, season.args, lowpass.args, iterations = 2 )
{
  if (any(is.na(y))) {
    rlang::abort("STL decomposition does not support series with missing values.")
  }
  deseas <- y
  season.args <- Filter(function(x) x[["period"]] > 1, season.args)
  period <- purrr::map_dbl(season.args, function(x) x[["period"]])
  season.args <- purrr::map2(season.args, 7 + 4 * order(period), function(x,
                                                                   default_window) {
    x$s.window <- x$s.window %||% default_window
    x
  })
  season.args <- season.args[order(period)]
  seas <- stats::setNames(as.list(rep(0, length(season.args))),
                          sprintf("season_%s",
                                  names(season.args) %||% purrr::map(season.args, function(x) x[["period"]])))
  if (length(season.args) > 0) {
    for (j in seq_len(iterations)) {
      for (i in seq_along(season.args)) {
        deseas <- stats::ts(deseas + seas[[i]], frequency = season.args[[i]][[1]])
        fit <- rlang::eval_tidy(rlang::expr(stats::stl(deseas, !!!c(trend.args,
                                               season.args[[i]][-1], lowpass.args))))
        seas[[i]] <- as.numeric(fit$time.series[, "seasonal"])
        deseas <- deseas - seas[[i]]
      }
    }
    trend <- fit$time.series[, "trend"]
  }
  else {
    trend <- stats::supsmu(seq_along(y), y)$y
  }
  trend <- as.numeric(trend)
  deseas <- as.numeric(deseas)
  rlang::list2(trend = trend, !!!seas, remainder = deseas - trend,
        season_adjust = deseas)
}
#' @rdname soothsayer_features
#' @export
feat_stl <- function( x ) {
  .period <- period(x)
  s.window <- 11

  season.args <- if (length(x) <= .period * 2) {
    list()
  }
  else {
    `:=` <- NULL
    rlang::list2(`:=`(!!(names(.period) %||% as.character(.period)),
               list(period = .period, s.window = s.window)))
  }
  rle_na <- rle(!is.na(x))
  if (any(!rle_na$values)) {
    rle_window <- which(rle_na$values)[which.max(rle_na$lengths[rle_na$values])]
    rle_idx <- cumsum(rle_na$lengths)
    rle_window <- c(if (rle_window == 1) 1 else rle_idx[max(1,
                                                            rle_window - 1)] + (rle_window > 1), rle_idx[rle_window])
    x <- x[seq(rle_window[1], rle_window[2])]
  }
  else {
    rle_window <- c(1, length(x))
  }
  dcmp <- rlang::eval_tidy(rlang::quo(estimate_stl(x, trend.args = list(),
                                     season.args = season.args, lowpass.args = list())))
  trend <- dcmp[["trend"]]
  remainder <- dcmp[["remainder"]]
  season_adjust <- dcmp[["season_adjust"]]
  seasonalities <- dcmp[seq_len(length(dcmp) - 3) + 1]
  names(seasonalities) <- sub("season_", "", names(seasonalities))
  var_e <- stats::var(remainder, na.rm = TRUE)
  n <- length(x)
  d <- (remainder - mean(remainder, na.rm = TRUE))^2
  var_loo <- (var_e * (n - 1) - d)/(n - 2)
  spikiness <- stats::var(var_loo, na.rm = TRUE)
  tren.coef <- stats::coef(stats::lm(trend ~ stats::poly(seq(n), degree = 2L)))[2L:3L]
  linearity <- tren.coef[[1L]]
  curvature <- tren.coef[[2L]]
  trend_strength <- max(0, min(1, 1 - var_e/stats::var(season_adjust,
                                                na.rm = TRUE)))
  seasonal_strength <- purrr::map_dbl(seasonalities, function(seas) {
    max(0, min(1, 1 - var_e/stats::var(remainder + seas, na.rm = TRUE)))
  })
  names(seasonal_strength) <- sprintf("seasonal_strength_%s",
                                      names(seasonalities))
  seasonal_peak <- purrr::map_dbl(seasonalities, function(seas) {
    (which.max(seas) + rle_window[1] - 1)%%.period
  })
  names(seasonal_peak) <- sprintf("seasonal_peak_%s", names(seasonalities))
  seasonal_trough <- purrr::map_dbl(seasonalities, function(seas) {
    (which.min(seas) + rle_window[1] - 1)%%.period
  })
  names(seasonal_trough) <- sprintf("seasonal_trough_%s", names(seasonalities))
  acf_resid <- stats::acf(remainder, lag.max = max(c(10, .period)),
                          plot = FALSE, na.action = stats::na.pass)$acf
  c( .trend_strength = trend_strength,
     .seasonal_strength = seasonal_strength,
     .seasonal_peak = seasonal_peak,
     .seasonal_trough = seasonal_trough,
     .spikiness = spikiness,
     .linearity = linearity,
     .curvature = curvature,
     .stl_e_acf1 = acf_resid[2L],
     .stl_e_acf10 = sum((acf_resid[2L:11L])^2))
}
 #' @rdname soothsayer_features
 #' @export
unitroot_nsdiffs <- function( x ) {

  .period <- period(x)
  alpha <- 0.05
  differences <- 0:3

  if (.period == 1)
    return(c(nsdiffs = min(differences)))
  diff <- function(x, differences, ...) {
    if (differences == 0)
      return(x)
    base::diff(x, differences = differences, ...)
  }
  keep <- purrr::map_lgl(differences, function(.x) {
    dx <- diff(x, lag = .period, differences = .x)
    !all(is.na(dx))
  })
  differences <- differences[keep]
  keep <- purrr::map_lgl(differences[-1] - 1, function(.x) {

    diff_x <- diff(x, lag = .period, differences = .x)
    feat_stl(diff_x)[2] < alpha
  })
  c(.nsdiffs = max(differences[c(TRUE, keep)], na.rm = TRUE))
}
#' @rdname soothsayer_features
#' @export
shift_kl_max <- function( x ) {
  .period <- period(x)
  .size <- ifelse(.period == 1, 10, .period)

  gw <- 100
  xgrid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
               length = gw)
  grid <- xgrid[2L] - xgrid[1L]
  tmpx <- x[!is.na(x)]
  bw <- stats::bw.nrd0(tmpx)
  lenx <- length(x)
  if (lenx <= (2 * .size)) {
    abort("length of `x` is too short for `.size`.")
  }
  densities <- purrr::map(xgrid, function(xgrid) stats::dnorm(xgrid,
                                                       mean = x, sd = bw))
  densities <- purrr::map(densities, pmax, stats::dnorm(38))
  rmean <- purrr::map(densities, function(x) slider::slide_dbl(x,
                                                               mean,
                                                               .before = .size - 1,
                                                               na.rm = TRUE))
  rmean <- purrr::map( purrr::transpose(rmean), unlist)
  kl <- purrr::map2_dbl( rmean[seq_len(lenx - .size)],
                         rmean[seq_len(lenx - .size) + .size],
                         function(x, y) sum(x * (log(x) - log(y)) * grid, na.rm = TRUE))
  diffkl <- diff(kl, na.rm = TRUE)
  if (length(diffkl) == 0L) {
    diffkl <- 0
    maxidx <- NA_real_
  }
  else {
    maxidx <- which.max(diffkl) + 1L
  }
  return(c(.shift_kl_max = max(diffkl, na.rm = TRUE), .shift_kl_index = maxidx))
}
#' @rdname soothsayer_features
#' @export
shift_level_max <- function( x ) {
  .period <- period(x)
  .size <- ifelse(.period == 1, 10, .period)

  rollmean <- slider::slide_dbl(x, mean, .before = .size -
                                  1, na.rm = TRUE)
  means <- abs(diff(rollmean, .size))
  if (length(means) == 0L) {
    maxmeans <- 0
    maxidx <- NA_real_
  }
  else if (all(is.na(means))) {
    maxmeans <- NA_real_
    maxidx <- NA_real_
  }
  else {
    maxmeans <- max(means, na.rm = TRUE)
    maxidx <- which.max(means) + 1L
  }
  return(c(.shift_level_max = maxmeans, .shift_level_index = maxidx))
}
#' @rdname soothsayer_features
#' @export
shift_var_max <- function( x ) {
  .period <- period(x)
  .size <- ifelse(.period == 1, 10, .period)

  rollvar <- slider::slide_dbl(x, stats::var, .before = .size - 1,
                               na.rm = TRUE)
  vars <- abs(diff(rollvar, .size))
  if (length(vars) == 0L) {
    maxvar <- 0
    maxidx <- NA_real_
  }
  else if (all(is.na(vars))) {
    maxvar <- NA_real_
    maxidx <- NA_real_
  }
  else {
    maxvar <- max(vars, na.rm = TRUE)
    maxidx <- which.max(vars) + 1L
  }
  return(c(.shift_var_max = maxvar, .shift_var_index = maxidx))
}
#' @rdname soothsayer_features
#' @export
positive <- function(x) {
  c(.positive = any(x > 0))
}
#' @rdname soothsayer_features
#' @export
negative <- function(x) {
  c(.negative = any(x < 0))
}
#' @rdname soothsayer_features
#' @export
zeros <- function(x) {
  c(.zeros = any(x == 0))
}
#' @rdname soothsayer_features
#' @export
continuous <- function( x ) {
  c(.continuous = !is.logical(x) & !is.integer(x) & !is.character(x) & !is.factor(x))
}
#' @rdname soothsayer_features
#' @export
count <- function( x ) {
  c( .count = isTRUE( all.equal( as.integer(x), x) ))
}
#' @rdname soothsayer_features
#' @export
period <- function(x)
{
  n <- length(x)
  spec <- stats::spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  c(.period = period)
}
#' @rdname soothsayer_features
#' @export
tslength <- function(x){
  c(.length = length(x))
}
#' @rdname soothsayer_features
#' @export
# use guerreros algorithm to find best lambda
boxcox_lambda <- function( x ) {
  .period <- period(x)
  # taken from feasts
  lambda_coef_var <- function (lambda, x, .period = 2)
  {
    if (all(x == x[1]))
      return(1)
    x <- split(x, (seq_along(x) - 1)%/%.period)
    mu_h <- sapply(x, mean, na.rm = TRUE)
    sig_h <- sapply(x, stats::sd, na.rm = TRUE)
    rat <- sig_h/mu_h^(1 - lambda)
    stats::sd(rat, na.rm = TRUE)/mean(rat, na.rm = TRUE)
  }
  c(.boxcox_lambda_guerrero = stats::optimise(
    lambda_coef_var,
    c(-0.9, 2),
    x = x,
    .period = max(.period, 2))$minimum)
}

#' Features used within soothsayer
#'
#' @description All of the features in soothsayer - most taken from other sources,
#' see **details**
#' @rdname soothsayer_features
#' @export
soothsayer_feature_set <- list( entropy,
                                lumpiness,
                                nonlinearity,
                                hurst,
                                stability,
                                box_pierce,
                                acf_features,
                                intermittent,
                                spectral,
                                arch_stat,
                                longest_flat_spot,
                                n_crossing_points,
                                ljung_box,
                                unitroot_kpss,
                                unitroot_ndiffs,
                                unitroot_nsdiffs,
                                unitroot_pp,
                                shift_kl_max,
                                shift_level_max,
                                shift_var_max,
                                positive,
                                negative,
                                zeros,
                                continuous,
                                count,
                                tslength,
                                period,
                                boxcox_lambda,
                                catch22_feat
)
# nocov end
