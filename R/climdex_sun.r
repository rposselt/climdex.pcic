## Climate indices involving sun timeseries
## -- introduced by R. Posselt (MeteoSwiss), July 2015

#' Sunshine duration
#' 
#' This function computes the climdex index SUN_DURATION.
#' 
#' This function takes a climdexInput object as input and computes the 
#' SUN_DURATION index: The sum of the sunshine duration hours within a period.
#' 
#' @param ci Object of type climdexInput (representing the daily sunshine duration in [h])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of sunshine duration.
#' 
#' @template generic_seealso_references 
#' 
#' @export
climdex.sun_duration <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$sun))
  return(tapply.fast(ci@data$sun, ci@date.factors[[match.arg(freq)]], sum, na.rm=TRUE) * ci@namasks[[match.arg(freq)]]$sun)
}

#' Relative sunshine duration
#' 
#' This function computes the climdex index SUN_RELMEAN.
#' 
#' This function takes a climdexInput object as input and computes the 
#' SUN_RELMEAN index: The mean of the relative sunshine duration within a period.
#' 
#' @param ci Object of type climdexInput (representing the daily relative sunshine duration in [\%])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of the relative sunshine duration.
#' 
#' @template generic_seealso_references 
#' 
#' @export
climdex.sun_relmean <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$sun_rel))
  return(tapply.fast(ci@data$sun_rel, ci@date.factors[[match.arg(freq)]], mean, na.rm=TRUE) * ci@namasks[[match.arg(freq)]]$sun_rel)
}

#' Mostly cloudy days
#' 
#' This function computes the climdex index SUN_CLOUDY.
#' 
#' This function takes a climdexInput object as input and computes the 
#' SUN_CLOUDY index: The number of mostly cloudy days (relative sunshine duration < 20 \%) within a period.
#' 
#' @param ci Object of type climdexInput (representing the daily relative sunshine duration in [\%])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the number of mostly cloudy days.
#' 
#' @template generic_seealso_references 
#' 
#' @export
climdex.sun_cloudy <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$sun_rel))
  return(number.days.op.threshold(temp=ci@data$sun_rel, date.factor=ci@date.factors[[match.arg(freq)]], 
                                  threshold=20, op="<") * ci@namasks[[match.arg(freq)]]$sun_rel)
}

#' Mostly sunny days
#' 
#' This function computes the climdex index SUN_SUNNY.
#' 
#' This function takes a climdexInput object as input and computes the 
#' SUN_SUNNY index: The number of mostly sunny days (relative sunshine duration > 80 \%) within a period.
#' 
#' @param ci Object of type climdexInput (representing the daily relative sunshine duration in [\%])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the number of mostly sunny days.
#' 
#' @template generic_seealso_references 
#' 
#' @export
climdex.sun_sunny <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$sun))
  return(number.days.op.threshold(temp=ci@data$sun, date.factor=ci@date.factors[[match.arg(freq)]], 
                                  threshold=80, op="<") * ci@namasks[[match.arg(freq)]]$sun)
}