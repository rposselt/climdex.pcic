## Climate indices involving snow timeseries
## -- introduced by R. Posselt (MeteoSwiss), July 2015

#' Number of snow days
#' 
#' This function computes the climdex index SNOW_DAYS.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index SNOW_DAYS: The number of days with a snow depth > Y cm.
#' 
#' @param ci Object of type climdexInput (representing the daily snow depth timeseries in [cm]).
#' @param threshold Snow depth threshhold [in cm]. (Default: 1)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing an annual timeseries of the number of snow days.
#' @template generic_seealso_references
#' @templateVar cdxvar snowtot
#' @templateVar cdxdescription an annual timeseries the number of snow days.
#' @template get_generic_example
#' 
#' @export
#' 
climdex.snow_days <- function(ci,threshold=1,freq=c("annual","halfyear","seasonal")) { 
  stopifnot(!is.null(ci@data$snow))
  return(number.days.op.threshold(temp=ci@data$snow, date.factor=ci@date.factors[[match.arg(freq)]], 
                                  threshold=threshold, op=">=") * ci@namasks[[match.arg(freq)]]$snow) 
}

#' Maximum snow depth
#' 
#' This function computes the climdex index SNOW_MAX.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index SNOW_MAX: The maximum snow depth measured within a year.
#' 
#' @param ci Object of type climdexInput (representing the daily snow depth timeseries in [cm]).
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing an annual timeseries of the maximum snow depth.
#' @template generic_seealso_references
#' @templateVar cdxvar snow
#' @templateVar cdxdescription an annual timeseries of the maximum snow depth.
#' @template get_generic_example
#' 
#' @export
#' 
climdex.snow_max <- function(ci,freq=c("annual","halfyear","seasonal")) { 
  stopifnot(!is.null(ci@data$snow))
  return(tapply.fast(ci@data$snow, ci@date.factors[[match.arg(freq)]], max, na.rm=TRUE) * 
           ci@namasks[[match.arg(freq)]]$snow)
}

#' Mean snow depth
#' 
#' This function computes the climdex index SNOW_MEAN.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index SNOW_MEAN: The mean snow depth measured within a period.
#' 
#' @param ci Object of type climdexInput (representing the daily snow depth timeseries in [cm]).
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing the timeseries of the mean snow depth.
#' @template generic_seealso_references
#' @templateVar cdxvar snow
#' @templateVar cdxdescription The mean snow depth measured within a period.
#' @template get_generic_example
#' 
#' @export
#' 
climdex.snow_mean <- function(ci,freq=c("annual","halfyear","seasonal")) { 
  stopifnot(!is.null(ci@data$snow))
  return(tapply.fast(ci@data$snow, ci@date.factors[[match.arg(freq)]], mean, na.rm=TRUE) * 
           ci@namasks[[match.arg(freq)]]$snow)
}

#' Number of new snow days
#' 
#' This function computes the climdex index SNOW_DAYSNEW.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index SNOW_DAYSNEW: The number of days with a (new) snowfall > Y cm.
#' 
#' @param ci Object of type climdexInput (representing the daily snowfall timeseries in [cm]).
#' @param threshold Snow depth threshhold [in cm]. (Default: 1)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing the timeseries of the number of new snow days.
#' @template generic_seealso_references
#' @templateVar cdxvar snow_new
#' @templateVar cdxdescription an annual timeseries the number of new snow days.
#' @template get_generic_example
#' 
#' @export
#' 
climdex.snow_daysnew <- function(ci,threshold=1,freq=c("annual","halfyear","seasonal")) { 
  stopifnot(!is.null(ci@data$snow_new))
  return(number.days.op.threshold(temp=ci@data$snow_new, date.factor=ci@date.factors[[match.arg(freq)]], 
                                  threshold=threshold, op=">=") * ci@namasks[[match.arg(freq)]]$snow_new) 
}

#' Maximum new snow
#' 
#' This function computes the climdex index SNOW_MAXNEW.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index SNOW_MAXNEW: The maximum snowfall measured within a year.
#' 
#' @param ci Object of type climdexInput (representing the daily snowfall timeseries in [cm]).
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing the timeseries of the maximum snowfall.
#' @template generic_seealso_references
#' @templateVar cdxvar snow_new
#' @templateVar cdxdescription an annual timeseries of the maximum snowfall.
#' @template get_generic_example
#' 
#' @export
#' 
climdex.snow_maxnew <- function(ci,freq=c("annual","halfyear","seasonal")) { 
  stopifnot(!is.null(ci@data$snow_new))
  return(tapply.fast(ci@data$snow_new, ci@date.factors[[match.arg(freq)]], max, na.rm=TRUE) * 
           ci@namasks[[match.arg(freq)]]$snow_new)
}

#' New snow sum
#' 
#' This function computes the climdex index SNOW_SUMNEW.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index SNOW_SUMNEW: The sum of all snowfall measured within a year.
#' 
#' @param ci Object of type climdexInput (representing the daily snowfall timeseries in [cm]).
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing the timeseries of the snowfall sum.
#' @template generic_seealso_references
#' @templateVar cdxvar snow_new
#' @templateVar cdxdescription an annual timeseries of the snowfall sum.
#' @template get_generic_example
#' 
#' @export
#' 
climdex.snow_sumnew <- function(ci,freq=c("annual","halfyear","seasonal")) { 
  stopifnot(!is.null(ci@data$snow_new))
  return(tapply.fast(ci@data$snow_new, ci@date.factors[[match.arg(freq)]], sum, na.rm=TRUE) * 
           ci@namasks[[match.arg(freq)]]$snow_new)
}