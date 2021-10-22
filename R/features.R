
entropy <- function(x) {
  spec <- try(stats::spec.ar(stats::na.contiguous(as.ts(x)),
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
  return(c(entropy = entropy))
}
lumpiness <- function(x, width = ifelse(frequency(x) > 1, frequency(x), 10)) {
  x <- as.numeric(scale(as.matrix(x), center = TRUE, scale = TRUE))
  nr <- length(x)
  lo <- seq(1, nr, by = width)
  up <- seq(width, nr + width, by = width)
  nsegs <- nr / width
  varx <- purrr::map_dbl(seq_len(nsegs), function(idx) {
    var(x[lo[idx]:up[idx]],
      na.rm = TRUE
    )
  })
  if (length(x) < 2 * width) {
    lumpiness <- 0
  } else {
    lumpiness <- var(varx, na.rm = TRUE)
  }
  return(c(lumpiness = lumpiness))
}
stability <- function(x, width = ifelse(frequency(x) > 1, frequency(x), 10)) {
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
    stability <- var(meanx, na.rm = TRUE)
  }
  return(c(stability = stability))
}
# terasvirta <- function (x, lag = 1, scale = TRUE, ...)
# {
#   DNAME <- deparse(substitute(x))
#   t <- length(x)
#   if (scale)
#     x <- scale(x)
#   y <- embed(x, lag + 1)
#   xnam <- paste("y[,", 2:(lag + 1), "]", sep = "")
#   fmla <- as.formula(paste("y[,1]~", paste(xnam, collapse = "+")))
#   rr <- lm(fmla)
#   u <- residuals(rr)
#   ssr0 <- sum(u^2)
#   xnam2 <- NULL
#   m <- 0
#   for (i in (1:lag)) {
#     for (j in (i:lag)) {
#       xnam2 <- c(xnam2, paste("I(y[,", i + 1, "]*y[,",
#                               j + 1, "])", sep = ""))
#       m <- m + 1
#     }
#   }
#   xnam2 <- paste(xnam2, collapse = "+")
#   xnam3 <- NULL
#   for (i in (1:lag)) {
#     for (j in (i:lag)) {
#       for (k in (j:lag)) {
#         xnam3 <- c(xnam3, paste("I(y[,", i + 1, "]*y[,",
#                                 j + 1, "]*y[,", k + 1, "])", sep = ""))
#         m <- m + 1
#       }
#     }
#   }
#   xnam3 <- paste(xnam3, collapse = "+")
#   fmla <- as.formula(paste("u~", paste(paste(xnam, collapse = "+"),
#                                        xnam2, xnam3, sep = "+")))
#   rr <- lm(fmla)
#   v <- residuals(rr)
#   ssr <- sum(v^2)
#     STAT <- t * log(ssr0/ssr)
#     PVAL <- 1 - pchisq(STAT, m)
#     PARAMETER <- m
#     names(STAT) <- "X-squared"
#     names(PARAMETER) <- "df"
#   METHOD <- "Teraesvirta Neural Network Test"
#   ARG <- c(lag, scale)
#   names(ARG) <- c("lag", "scale")
#   structure(list(statistic = STAT, parameter = PARAMETER, p.value = PVAL,
#                  method = METHOD, data.name = DNAME, arguments = ARG),
#             class = "htest")
#   return(STAT)
# }
nonlinearity <- function(x) {
  X2 <- tryCatch(tseries:::terasvirta.test.ts(as.ts(x))$statistic,
    error = function(e) NA
  )
  c(nonlinearity = 10 * unname(X2) / length(x))
}
hurst <- function(x) {
  require("fracdiff")
  return(c(hurst = suppressWarnings(fracdiff::fracdiff(
    as.ts(na.contiguous(unlist(x))),
    0, 0
  )[["d"]] + 0.5)))
}
unitroot_pp <- function(x, type = c("Z-tau", "Z-alpha"), model = c(
                          "constant",
                          "trend"
                        ), lags = c("short", "long"), ...) {
  require("urca")
  result <- urca::ur.pp(x,
    type = match.arg(type), model = match.arg(model),
    lags = match.arg(lags), ...
  )
  pval <- stats::approx(result@cval[1, ], as.numeric(sub(
    "pct",
    "", colnames(result@cval)
  )) / 100,
  xout = result@teststat[1],
  rule = 2
  )$y
  c(pp_stat = result@teststat)
}
box_pierce <- function(x, lag = 1, dof = 0, ...) {
  out <- stats::Box.test(x,
    lag = lag, type = "Box-Pierce",
    fitdf = dof
  )
  c(bp_stat = unname(out$statistic))
}
unitroot_kpss <- function (x, type = c("mu", "tau"), lags = c("short", "long",
                                             "nil"), ...)
{
  require_package("urca")
  result <- urca::ur.kpss(x, type = type, lags = lags, ...)
  # pval <- stats::approx(result@cval[1, ], as.numeric(sub("pct",
  #                                                        "", colnames(result@cval)))/100, xout = result@teststat[1],
  #                       rule = 2)$y
  c(kpss_stat = result@teststat)#, kpss_pvalue = pval)
}
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
longest_flat_spot <- function (x)
{
  cutx <- cut(x, breaks = 10, include.lowest = TRUE, labels = FALSE)
  rlex <- rle(cutx)
  return(c(longest_flat_spot = max(rlex$lengths)))
}
n_crossing_points <- function (x)
{
  midline <- median(x, na.rm = TRUE)
  ab <- x <= midline
  lenx <- length(x)
  p1 <- ab[1:(lenx - 1)]
  p2 <- ab[2:lenx]
  cross <- (p1 & !p2) | (p2 & !p1)
  c(n_crossing_points = sum(cross, na.rm = TRUE))
}
ljung_box <- function (x, lag = 1, dof = 0, ...)
{
  out <- stats::Box.test(x, lag = lag, type = "Ljung-Box",
                         fitdf = dof)
  c(lb_stat = unname(out$statistic))
}
acf_features <- function(x, .period = 1, lag_max = NULL, ...) {
  acfx <- stats::acf(x, lag.max = lag_max %||% max(
    .period,
    10L
  ), plot = FALSE, na.action = stats::na.pass, ...)
  acfdiff1x <- stats::acf(diff(x, differences = 1), lag.max = lag_max %||%
    10L, plot = FALSE, na.action = stats::na.pass)
  acfdiff2x <- stats::acf(diff(x, differences = 2), lag.max = lag_max %||%
    10L, plot = FALSE, na.action = stats::na.pass)
  acf_1 <- acfx$acf[2L]
  sum_of_sq_acf10 <- sum((acfx$acf[2L:11L])^2)
  diff1_acf1 <- acfdiff1x$acf[2L]
  diff1_acf10 <- sum((acfdiff1x$acf[-1L])^2)
  diff2_acf1 <- acfdiff2x$acf[2L]
  diff2_acf10 <- sum((acfdiff2x$acf[-1L])^2)
  output <- c(
    acf1 = unname(acf_1), acf10 = unname(sum_of_sq_acf10),
    diff1_acf1 = unname(diff1_acf1), diff1_acf10 = unname(diff1_acf10),
    diff2_acf1 = unname(diff2_acf1), diff2_acf10 = unname(diff2_acf10)
  )
  if (.period > 1) {
    output <- c(output, season_acf1 = unname(acfx$acf[.period +
      1L]))
  }
  return(output)
}
intermittent <- function(x) {
  rle <- rle(x)
  nonzero <- x[x != 0]
  c(zero_run_mean = if (length(nonzero) == length(x)) {
    0
  } else {
    mean(rle$lengths[rle$values ==
      0])
  }, nonzero_squared_cv = (sd(nonzero, na.rm = TRUE) / mean(nonzero,
    na.rm = TRUE
  ))^2, zero_start_prop = if (rle$values[1] !=
    0) {
    0
  } else {
    rle$lengths[1] / length(x)
  }, zero_end_prop = if (rle$values[length(rle$values)] !=
    0) {
    0
  } else {
    rle$lengths[length(rle$lengths)] / length(x)
  })
}
arch_stat <- function (x, lags = 12, demean = TRUE)
{
  if (length(x) <= lags + 1) {
    return(c(arch_stat = NA_real_))
  }
  if (demean) {
    x <- x - mean(x, na.rm = TRUE)
  }
  mat <- embed(x^2, lags + 1)
  fit <- lm(mat[, 1] ~ mat[, -1])
  arch_stat <- suppressWarnings(summary(fit)$r.squared)
  c( arch_stat = if (is.nan(arch_stat)) 1 else arch_stat)
}
spectral <- function (x, .period = 1, ...)
{
  spec <- try(stats::spec.ar(na.contiguous(ts(x, frequency = .period)),
                             plot = FALSE, method = "burg", n.freq = ceiling(length(x)/2 +
                                                                               1)), ...)
  if (class(spec) == "try-error") {
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
  return(c(spectral_entropy = entropy))
}
catch22_feat <- function( x ) {
  res <- Rcatch22::catch22_all(x)
  setNames( res[["values"]], res[["names"]])
}
# stl_feat <- function (x, .period, s.window = 11, ...)
# {
#   dots <- dots_list(...)
#   dots <- dots[names(dots) %in% names(formals(stats::stl))]
#   season.args <- if (length(x) <= .period * 2) {
#     list()
#   }
#   else {
#     list2(`:=`(!!(names(.period) %||% as.character(.period)),
#                list(period = .period, s.window = s.window)))
#   }
#   rle_na <- rle(!is.na(x))
#   if (any(!rle_na$values)) {
#     rle_window <- which(rle_na$values)[which.max(rle_na$lengths[rle_na$values])]
#     rle_idx <- cumsum(rle_na$lengths)
#     rle_window <- c(if (rle_window == 1) 1 else rle_idx[max(1,
#                                                             rle_window - 1)] + (rle_window > 1), rle_idx[rle_window])
#     x <- x[seq(rle_window[1], rle_window[2])]
#   }
#   else {
#     rle_window <- c(1, length(x))
#   }
#   dcmp <- eval_tidy(quo(estimate_stl(x, trend.args = list(),
#                                      season.args = season.args, lowpass.args = list(), !!!dots)))
#   trend <- dcmp[["trend"]]
#   remainder <- dcmp[["remainder"]]
#   season_adjust <- dcmp[["season_adjust"]]
#   seasonalities <- dcmp[seq_len(length(dcmp) - 3) + 1]
#   names(seasonalities) <- sub("season_", "", names(seasonalities))
#   var_e <- var(remainder, na.rm = TRUE)
#   n <- length(x)
#   d <- (remainder - mean(remainder, na.rm = TRUE))^2
#   var_loo <- (var_e * (n - 1) - d)/(n - 2)
#   spikiness <- var(var_loo, na.rm = TRUE)
#   tren.coef <- coef(lm(trend ~ poly(seq(n), degree = 2L)))[2L:3L]
#   linearity <- tren.coef[[1L]]
#   curvature <- tren.coef[[2L]]
#   trend_strength <- max(0, min(1, 1 - var_e/var(season_adjust,
#                                                 na.rm = TRUE)))
#   seasonal_strength <- map_dbl(seasonalities, function(seas) {
#     max(0, min(1, 1 - var_e/var(remainder + seas, na.rm = TRUE)))
#   })
#   names(seasonal_strength) <- sprintf("seasonal_strength_%s",
#                                       names(seasonalities))
#   seasonal_peak <- map_dbl(seasonalities, function(seas) {
#     (which.max(seas) + rle_window[1] - 1)%%.period
#   })
#   names(seasonal_peak) <- sprintf("seasonal_peak_%s", names(seasonalities))
#   seasonal_trough <- map_dbl(seasonalities, function(seas) {
#     (which.min(seas) + rle_window[1] - 1)%%.period
#   })
#   names(seasonal_trough) <- sprintf("seasonal_trough_%s", names(seasonalities))
#   acf_resid <- stats::acf(remainder, lag.max = max(c(10, .period)),
#                           plot = FALSE, na.action = stats::na.pass, ...)$acf
#   c(trend_strength = trend_strength, seasonal_strength, seasonal_peak,
#     seasonal_trough, spikiness = spikiness, linearity = linearity,
#     curvature = curvature, stl_e_acf1 = acf_resid[2L], stl_e_acf10 = sum((acf_resid[2L:11L])^2))
# }


