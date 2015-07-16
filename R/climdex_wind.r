## Climate indices involving wind timeseries
## -- introduced by R. Posselt (MeteoSwiss), July 2015

#' Mean wind
#' 
#' This function computes the climdex index WIND_MEAN.
#' 
#' This function takes a climdexInput object as input and computes the 
#' WIND_MEAN index: The mean wind speed measured within a period.
#' 
#' @param ci Object of type climdexInput (representing the daily mean wind speed in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of mean wind speed.
#' @template generic_seealso_references 
#' @templateVar cdxvar wind
#' @templateVar cdxdescription The mean wind speed measured within a period.
#' @template get_generic_example
#' 
#' @export
climdex.wind_mean <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$wind))
  return(tapply.fast(ci@data$wind, ci@date.factors[[match.arg(freq)]], mean, na.rm=TRUE) * ci@namasks[[match.arg(freq)]]$wind)
}

#' Calm days
#' 
#' This function computes the climdex index WIND_CALMDAYS.
#' 
#' This function takes a climdexInput object as input and computes the 
#' WIND_CALMDAYS index: The number of days with mean wind lower than or equal 2 m/s.
#' 
#' @param ci Object of type climdexInput (representing the daily mean wind speed in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the number of calm days.
#' @template generic_seealso_references 
#' @templateVar cdxvar wind
#' @templateVar cdxdescription The number of days with mean wind lower than or equal 2 m/s.
#' @template get_generic_example
#' 
#' @export
climdex.wind_calmdays <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$wind))
  return(number.days.op.threshold(temp=ci@data$wind, date.factor=ci@date.factors[[match.arg(freq)]], 
                                  threshold=2, op="<=") * ci@namasks[[match.arg(freq)]]$wind)
}

#' Windy days
#' 
#' This function computes the climdex index WIND_WINDYDAYS.
#' 
#' This function takes a climdexInput object as input and computes the 
#' WIND_WINDYDAYS index: The number of days with mean wind greater than or equal 10.8 m/s (~6 Bft).
#' 
#' @param ci Object of type climdexInput (representing the daily mean wind speed in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the number of calm days.
#' @template generic_seealso_references 
#' @templateVar cdxvar wind
#' @templateVar cdxdescription The number of days with mean wind greater than or equal 10.8 m/s (~6 Bft).
#' @template get_generic_example
#' 
#' @export
climdex.wind_windydays <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$wind))
  return(number.days.op.threshold(temp=ci@data$wind, date.factor=ci@date.factors[[match.arg(freq)]], 
                                  threshold=10.8, op=">=") * ci@namasks[[match.arg(freq)]]$wind)
}

#' Storm days
#' 
#' This function computes the climdex index WIND_STORMDAYS.
#' 
#' This function takes a climdexInput object as input and computes the 
#' WIND_STORMDAYS index: The number of days with wind gusts greater than or equal 20.8 m/s (75 km/h).
#' 
#' @param ci Object of type climdexInput (representing the daily maximum wind gust in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the number of storm days.
#' @template generic_seealso_references 
#' @templateVar cdxvar wind_gust
#' @templateVar cdxdescription The number of days with wind gusts greater than or equal 20.8 m/s (75 km/h).
#' @template get_generic_example
#' 
#' @export
climdex.wind_stormdays <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$wind_gust))
  return(number.days.op.threshold(temp=ci@data$wind_gust, date.factor=ci@date.factors[[match.arg(freq)]], 
                                  threshold=20.8, op=">=") * ci@namasks[[match.arg(freq)]]$wind_gust)
}

#' Maximum wind gust
#' 
#' This function computes the climdex index WIND_MAXGUST.
#' 
#' This function takes a climdexInput object as input and computes the 
#' WIND_MAXGUST index: The maximum of the maximum daily wind gust.
#' 
#' @param ci Object of type climdexInput (representing the daily maximum wind gust in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the maximum of the maximum daily wind gust.
#' @template generic_seealso_references 
#' @templateVar cdxvar wind_gust
#' @templateVar cdxdescription The maximum of the maximum daily wind gust.
#' @template get_generic_example
#' 
#' @export
climdex.wind_maxgust <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$wind_gust))
  return(tapply.fast(ci@data$wind_gust, ci@date.factors[[match.arg(freq)]], max, na.rm=TRUE) * 
           ci@namasks[[match.arg(freq)]]$wind_gust)
}

#' Northerly winds
#' 
#' This function computes the climdex index WIND_NORTHERLY.
#' 
#' This function takes a climdexInput object as input and computes the 
#' WIND_NORTHERLY index: Days with northerly wind (-45° (315°) < wind_dir <= 45°).
#' 
#' @param ci Object of type climdexInput (representing the daily mean wind direction in [°] with 0° being wind from north)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of days with northerly winds.
#' @template generic_seealso_references 
#' @templateVar cdxvar wind
#' @templateVar cdxdescription Days with northerly wind.
#' @template get_generic_example
#' 
#' @export
climdex.wind_northerly <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$wind_dir))
  wind_northerly <- (number.days.op.threshold(temp=ci@data$wind_dir, date.factor=ci@date.factors[[match.arg(freq)]], 
                                              threshold=315, op=">") +
                       number.days.op.threshold(temp=ci@data$wind_dir, date.factor=ci@date.factors[[match.arg(freq)]], 
                                                threshold=45, op="<=")) * ci@namasks[[match.arg(freq)]]$wind_dir
  return(wind_northerly)
}

#' easterly winds
#' 
#' This function computes the climdex index WIND_EASTERLY.
#' 
#' This function takes a climdexInput object as input and computes the 
#' WIND_EASTERLY index: Days with easterly wind (45° < wind_dir <= 135°).
#' 
#' @param ci Object of type climdexInput (representing the daily mean wind direction in [°] with 90° being wind from east)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of days with easterly winds.
#' @template generic_seealso_references 
#' @templateVar cdxvar wind
#' @templateVar cdxdescription Days with easterly wind.
#' @template get_generic_example
#' 
#' @export
climdex.wind_easterly <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$wind_dir))
  wind_easterly <- (number.days.op.threshold(temp=ci@data$wind_dir, date.factor=ci@date.factors[[match.arg(freq)]], 
                                              threshold=45, op=">") -
                       number.days.op.threshold(temp=ci@data$wind_dir, date.factor=ci@date.factors[[match.arg(freq)]], 
                                                threshold=135, op=">")) * ci@namasks[[match.arg(freq)]]$wind_dir
  return(wind_easterly)
}

#' southerly winds
#' 
#' This function computes the climdex index WIND_SOUTHERLY.
#' 
#' This function takes a climdexInput object as input and computes the 
#' WIND_SOUTHERLY index: Days with southerly wind (135° < wind_dir <= 225°).
#' 
#' @param ci Object of type climdexInput (representing the daily mean wind direction in [°] with 180° being wind from south)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of days with southerly winds.
#' @template generic_seealso_references 
#' @templateVar cdxvar wind
#' @templateVar cdxdescription Days with southerly wind.
#' @template get_generic_example
#' 
#' @export
climdex.wind_southerly <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$wind_dir))
  wind_southerly <- (number.days.op.threshold(temp=ci@data$wind_dir, date.factor=ci@date.factors[[match.arg(freq)]], 
                                             threshold=135, op=">") -
                      number.days.op.threshold(temp=ci@data$wind_dir, date.factor=ci@date.factors[[match.arg(freq)]], 
                                               threshold=225, op=">")) * ci@namasks[[match.arg(freq)]]$wind_dir
  return(wind_southerly)
}

#' westerly winds
#' 
#' This function computes the climdex index WIND_WESTERLY
#' 
#' This function takes a climdexInput object as input and computes the 
#' WIND_WESTERLY index: Days with westerly wind (225° < wind_dir <= 315°).
#' 
#' @param ci Object of type climdexInput (representing the daily mean wind direction in [°] with 270° being wind from west)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of days with westerly winds.
#' @template generic_seealso_references 
#' @templateVar cdxvar wind
#' @templateVar cdxdescription Days with westerly wind.
#' @template get_generic_example
#' 
#' @export
climdex.wind_westerly <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$wind_dir))
  wind_westerly <- (number.days.op.threshold(temp=ci@data$wind_dir, date.factor=ci@date.factors[[match.arg(freq)]], 
                                              threshold=225, op=">") -
                       number.days.op.threshold(temp=ci@data$wind_dir, date.factor=ci@date.factors[[match.arg(freq)]], 
                                                threshold=315, op=">")) * ci@namasks[[match.arg(freq)]]$wind_dir
  return(wind_westerly)
}
