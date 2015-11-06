## Climate indices involving temperature timeseries
## -- introduced by R. Posselt (MeteoSwiss), July 2015

#' Min/Max of mean n-day Min/Avg/Max.-Temperature
#' 
#' This functions compute the climdex indeces Txndaymax, Txndaymin, 
#'                                            Tnndaymax, Tnndaymin, 
#'                                            Tmndaymax, Tmndaymin.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index T[x|m|n]nday[min|max]: min/max of mean n-day [max|avg|min] temperature.
#' 
#' @param ci Object of type climdexInput (representing daily Tmax/Tavg/Tmin).
#' @param ndays number of days to consider (default=5).
#' @param freq Time frequency to aggregate to (default="monthly").
#' @param center.mean.on.last.day Whether to center the n-day running mean on
#' the last day of the window, instead of the center day.
#' 
#' @template generic_seealso_references
#' 
#' @templateVar cdxvar txndaymax
#' @templateVar cdxdescription a time series of the max of average maximum n-day temperature.
#' @template get_generic_example
#' 
#' @name climdex.tnday
NULL

#' 
#' @rdname climdex.tnday
#' @export
#' 
climdex.txndaymax <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tmax))
  return(nday.consec.temp.mean(ci@data$tmax, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="max", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tmax) 
}

#' 
#' @rdname climdex.tnday
#' @export
#' 
climdex.txndaymin <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tmax))
  return(nday.consec.temp.mean(ci@data$tmax, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="min", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tmax) 
}

#' 
#' @rdname climdex.tnday
#' @export
#' 
climdex.tnndaymax <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tmin))
  return(nday.consec.temp.mean(ci@data$tmin, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="max", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tmin) 
}

#' 
#' @rdname climdex.tnday
#' @export
#' 
climdex.tnndaymin <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tmin))
  return(nday.consec.temp.mean(ci@data$tmin, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="min", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tmin) 
}

#' 
#' @rdname climdex.tnday
#' @export
#' 
climdex.tmndaymax <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tavg))
  return(nday.consec.temp.mean(ci@data$tavg, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="max", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tavg) 
}

#' 
#' @rdname climdex.tnday
#' @export
#' 
climdex.tmndaymin <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tavg))
  return(nday.consec.temp.mean(ci@data$tavg, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="min", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tavg) 
}

#' Percent of Values Above nth Percentile Daily Maximum Temperature
#' 
#' This function computes the climdex index TXnp.
#' 
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values above/below the nth percentile of baseline
#' daily maximum temperature.
#' 
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @param quant Quantile to used (must be stated and calculated within \code{\link{climdexInput.raw}}). Default: 0.9
#' @param op Operator to use for comparison. Default: ">"
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' 
#' @export
#' 
climdex.txnp <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), quant=0.9, op=">") {
  quant.str <- paste0("q",as.integer(quant*100)) 
  stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax) && !is.null(ci@quantiles$tmax$outbase[[quant.str]]))   
  return(percent.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], 
                                   ci@quantiles$tmax$outbase[[quant.str]], ci@quantiles$tmax$inbase[[quant.str]], ci@base.range, op, 
                                   ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax) 
}

#' Percent of Values Above nth Percentile Daily Minimum Temperature
#' 
#' This function computes the climdex index TNnp.
#' 
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values above the 90th percentile of baseline
#' daily minimum temperature.
#' 
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @param quant Quantile to used (must be stated and calculated within \code{\link{climdexInput.raw}}). Default: 0.9
#' @param op Operator to use for comparison. Default: ">"
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' 
#' @export
#' 
climdex.tnnp <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), quant=0.9, op=">") {
  quant.str <- paste0("q",as.integer(quant*100)) 
  stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin) && !is.null(ci@quantiles$tmin$outbase[[quant.str]]))   
  return(percent.days.op.threshold(ci@data$tmin, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], 
                                   ci@quantiles$tmin$outbase[[quant.str]], ci@quantiles$tmin$inbase[[quant.str]], ci@base.range, op, 
                                   ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmin) 
}