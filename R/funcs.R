pred_gam <- function(mod_in, dat_in, eval = 'totalp', const = 'depthm', fac = 'Invasive', lev = 'none', qconst = 0.5, n = 100, ...){

  # sanity checks
  dat_in <- data.frame(dat_in)
  stopifnot(const %in% names(dat_in))
  chklev <- levels(dat_in[, fac, drop = TRUE]) %in% lev
  stopifnot(any(chklev))
  
  # get values for prediction
  consts <- as.numeric(quantile(dat_in[, const], qconst, na.rm = T))
  evalrn <- range(dat_in[, eval], na.rm = TRUE)
  evals <- seq(evalrn[1], evalrn[2], length = n)

  # format prediction values as df
  toeval <- data.frame(consts, evals, lev)
  names(toeval) <- c(const, eval, fac)

  # get prediction, append to prediction grid, including quantile level
  res <- predict(mod_in, newdata = toeval, ...)
  out <- toeval[, !names(toeval) %in% const]
  out <- data.frame(out, qconst = paste0(const, ' q', qconst), eval = eval)
  names(out)[names(out) %in% eval] <- 'var'
  out <- data.frame(res, out)
  return(out)
  
}