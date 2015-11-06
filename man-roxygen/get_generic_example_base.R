#' @examples
#' library(PCICt)
#' 
#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]), format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]), format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]), format="%Y %j", cal="gregorian")
#' 
#' ## Load the data in.
#' ci <- climdexInput.raw(tmax=ec.1018935.tmax$MAX_TEMP,
#'                        tmin=ec.1018935.tmin$MIN_TEMP, 
#'                        prec=ec.1018935.prec$ONE_DAY_PRECIPITATION,
#'                        tmax.dates=tmax.dates, 
#'                        tmin.dates=tmin.dates, 
#'                        prec.dates=prec.dates, 
#'                        base.range=c(1971, 2000))
#'
