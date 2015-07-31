

#' @title Number of days (less than, greater than, etc) a threshold
#' 
#' @description
#' Produces means of values that exceed (or are below) the specified threshold.
#' 
#' @details
#' This function takes a data series, the number of days in the running window,
#' a date factor to aggregate by, and an optional modifier parameter
#' (center.mean.on.last.day). It computes the n-day running mean of
#' temperature and returns the mean n-day temperature per unit
#' time, as defined by \code{date.factor}.
#' 
#' @param daily.temp Daily timeseries of temperature (tmax, tavg or tmin).
#' @param date.factor Factor to aggregate by.
#' @param ndays Number of days in the running window.
#' @param center.mean.on.last.day Whether to center the n-day running mean on
#' the last day of the series, instead of the middle day.
#' @return A vector consisting of the mean n-day temp per
#' time interval.
#' @keywords ts climate
#' @examples
#' library(PCICt)
#' 
#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year",
#' "jday")]), format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year",
#' "jday")]), format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year",
#' "jday")]), format="%Y %j", cal="gregorian")
#' 
#' ## Load the data in.
#' ci <- climdexInput.raw(ec.1018935.tmax$MAX_TEMP,
#' ec.1018935.tmin$MIN_TEMP, ec.1018935.prec$ONE_DAY_PRECIPITATION,
#' tmax.dates, tmin.dates, prec.dates, base.range=c(1971, 2000))
#' 
#' ## Compute txnday on a monthly basis.
#' tx5daymax <- nday.consec.temp.mean(ci@@data$tmax, ci@@date.factors$monthly, freq.fun="max", ndays=5)
#' 
#' @export
nday.consec.temp.mean <- function(daily.temp, date.factor, ndays, freq.fun="max", center.mean.on.last.day=FALSE) {
  if(ndays == 1) {
    return(suppressWarnings(tapply.fast(daily.temp, date.factor, mean, na.rm=TRUE)))
  }
  ## Ends of the data will be de-emphasized (padded with zero precip data); NAs replaced with 0
  daily.temp[is.na(daily.temp)] <- 0
  temp.runmean <- runmean(daily.temp, k=ndays, endrule="NA")
  temp.runmean[is.na(temp.runmean)] <- 0
  if(center.mean.on.last.day) {
    k2 = ndays %/% 2
    temp.runmean <- c(rep(0, k2), temp.runmean[1:(length(temp.runmean) - k2)])
  }
  return(tapply.fast(temp.runmean, date.factor, match.fun(freq.fun)))
}