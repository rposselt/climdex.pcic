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
#' @template generic_seealso_references
#' @templateVar cdxvar txndaymax
#' @templateVar cdxdescription min/max of mean n-day [max|avg|min] temperature.
#' @template get_generic_example
#' @name climdex.tnday
NULL

#' @rdname climdex.tnday
#' @export
climdex.txndaymax <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tmax))
  return(nday.consec.temp.mean(ci@data$tmax, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="max", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tmax) 
}
#' @rdname climdex.tnday
#' @export
climdex.txndaymin <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tmax))
  return(nday.consec.temp.mean(ci@data$tmax, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="min", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tmax) 
}
#' @rdname climdex.tnday
#' @export
climdex.tnndaymax <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tmin))
  return(nday.consec.temp.mean(ci@data$tmin, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="max", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tmin) 
}
#' @rdname climdex.tnday
#' @export
climdex.tnndaymin <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tmin))
  return(nday.consec.temp.mean(ci@data$tmin, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="min", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tmin) 
}
#' @rdname climdex.tnday
#' @export
climdex.tmndaymax <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tavg))
  return(nday.consec.temp.mean(ci@data$tavg, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="max", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tavg) 
}
#' @rdname climdex.tnday
#' @export
climdex.tmndaymin <- function(ci, ndays=5, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$tavg))
  return(nday.consec.temp.mean(ci@data$tavg, ci@date.factors[[match.arg(freq)]], 
                               ndays=ndays, freq.fun="min", center.mean.on.last.day) * 
           ci@namasks[[match.arg(freq)]]$tavg) 
}