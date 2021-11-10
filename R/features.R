
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
  return(c(.entropy = entropy))
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
  return(c(.lumpiness = lumpiness))
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
  return(c(.stability = stability))
}

nonlinearity <- function(x) {
  X2 <- tryCatch(tseries:::terasvirta.test.ts(as.ts(x))$statistic,
    error = function(e) NA
  )
  c(.nonlinearity = 10 * unname(X2) / length(x))
}
hurst <- function(x) {
  require("fracdiff")
  return(c(.hurst = suppressWarnings(fracdiff::fracdiff(
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
  c(.unitroot_pp = result@teststat)
}
box_pierce <- function(x, lag = 1, dof = 0, ...) {
  out <- stats::Box.test(x,
    lag = lag, type = "Box-Pierce",
    fitdf = dof
  )
  c(.box_pierce = unname(out$statistic))
}
unitroot_kpss <- function (x, type = c("mu", "tau"), lags = c("short", "long",
                                             "nil"), ...)
{
  require("urca")
  result <- urca::ur.kpss(x, type = type, lags = lags, ...)
  c(.unitroot_kpss = result@teststat)
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
  return(c(.longest_flat_spot = max(rlex$lengths)))
}
n_crossing_points <- function (x)
{
  midline <- median(x, na.rm = TRUE)
  ab <- x <= midline
  lenx <- length(x)
  p1 <- ab[1:(lenx - 1)]
  p2 <- ab[2:lenx]
  cross <- (p1 & !p2) | (p2 & !p1)
  c(.n_crossing_points = sum(cross, na.rm = TRUE))
}
ljung_box <- function (x, lag = 1, dof = 0, ...)
{
  out <- stats::Box.test(x, lag = lag, type = "Ljung-Box",
                         fitdf = dof)
  c(.ljung_box = unname(out$statistic))
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
    .acf1 = unname(acf_1), acf10 = unname(sum_of_sq_acf10),
    .diff1_acf1 = unname(diff1_acf1), diff1_acf10 = unname(diff1_acf10),
    .diff2_acf1 = unname(diff2_acf1), diff2_acf10 = unname(diff2_acf10)
  )
  if (.period > 1) {
    output <- c(output, .season_acf1 = unname(acfx$acf[.period +
      1L]))
  }
  return(output)
}
intermittent <- function(x) {
  rle <- rle(x)
  nonzero <- x[x != 0]
  c(.intermittent = if (length(nonzero) == length(x)) {
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
  c( .arch_stat = if (is.nan(arch_stat)) 1 else arch_stat)
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
  return(c(.spectral = entropy))
}
catch22_feat <- function( x ) {
  res <- Rcatch22::catch22_all(x)
  setNames( res[["values"]], paste0(".",res[["names"]]))
}

positive <- function(x) {
  c(.positive = any(x > 0))
}
negative <- function(x) {
  c(.negative = any(x < 0))
}
zeros <- function(x) {
  c(.zeros = any(x == 0))
}
continuous <- function( x ) {
  c(.continuous = !is.logical(x) & !is.integer(x) & !is.character(x) & !is.factor(x))
}
count <- function( x ) {
  c( .count = isTRUE( all.equal( as.integer(x), x) ))
}

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

tslength <- function(x){
  c(.length = length(x))
}
# use guerreros algorithm to find best lambda
boxcox_lambda <- function( x, lower = -0.9, upper = 2, .period = 2L ) {
  # taken from feasts
  lambda_coef_var <- function (lambda, x, .period = 2)
  {
    if (all(x == x[1]))
      return(1)
    x <- split(x, (seq_along(x) - 1)%/%.period)
    mu_h <- purrr::map_dbl(x, mean, na.rm = TRUE)
    sig_h <- purrr::map_dbl(x, sd, na.rm = TRUE)
    rat <- sig_h/mu_h^(1 - lambda)
    sd(rat, na.rm = TRUE)/mean(rat, na.rm = TRUE)
  }
  c(.boxcox_lambda_guerrero = optimise(
    lambda_coef_var,
    c(lower, upper),
    x = x,
    .period = max(.period, 2))$minimum)
}




