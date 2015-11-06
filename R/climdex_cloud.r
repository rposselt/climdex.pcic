## Climate indices involving cloud timeseries
## -- introduced by R. Posselt (MeteoSwiss), July 2015

#' Mean cloud cover
#' 
#' This function computes the climdex index CLOUD_MEAN.
#' 
#' This function takes a climdexInput object as input and computes the 
#' CLOUD_MEAN index: The mean cloud cover measured within a period.
#' 
#' @param ci Object of type climdexInput (representing the daily mean cloud cover in octa or percent)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of mean cloud cover.
#' 
#' @template generic_seealso_references 
#' 
#' @export
climdex.cloud_mean <- function(ci,freq=c("annual","halfyear","seasonal","monthly")) { 
  stopifnot(!is.null(ci@data$cloud))
  return(tapply.fast(ci@data$cloud, ci@date.factors[[match.arg(freq)]], mean, na.rm=TRUE) * ci@namasks[[match.arg(freq)]]$cloud)
}

#' Mostly cloudy days
#' 
#' This function computes the climdex index CLOUD_CLOUDY.
#' 
#' This function takes a climdexInput object as input and computes the 
#' CLOUD_CLOUDY index: The number of mostly cloudy days (Cloud cover >= 6octa / 80%) within a period.
#' 
#' @param ci Object of type climdexInput (representing the daily mean cloud cover in octa or percent)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @param unit unit of the cloud cover. Allowed are: "octa" or "percent". Default: "octa".
#' @return A vector containing the number of mostly cloudy days.
#' 
#' @template generic_seealso_references 
#' 
#' @export
climdex.cloud_cloudy <- function(ci,freq=c("annual","halfyear","seasonal","monthly"),unit="octa") { 
  stopifnot(!is.null(ci@data$cloud))
  if (unit=="octa"){
    threshold=6
  }else if (unit=="percent"){
    threshold=80
  } else{
    stop("unit should be either 'octa' or 'percent'")
  }
  return(number.days.op.threshold(temp=ci@data$cloud, date.factor=ci@date.factors[[match.arg(freq)]], 
                                  threshold=threshold, op=">=") * ci@namasks[[match.arg(freq)]]$cloud)
}

#' Mostly sunny days
#' 
#' This function computes the climdex index CLOUD_SUNNY.
#' 
#' This function takes a climdexInput object as input and computes the 
#' CLOUD_SUNNY index: The number of mostly sunny days (Cloud cover <= 2octa / 20%) within a period.
#' 
#' @param ci Object of type climdexInput (representing the daily mean cloud cover in octa or percent)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @param unit unit of the cloud cover. Allowed are: "octa" or "percent". Default: "octa".
#' @return A vector containing the number of mostly sunny days.
#' 
#' @template generic_seealso_references 
#' 
#' @export
climdex.cloud_sunny <- function(ci,freq=c("annual","halfyear","seasonal","monthly"),unit="octa") { 
  stopifnot(!is.null(ci@data$cloud))
  if (unit=="octa"){
    threshold=2
  }else if (unit=="percent"){
    threshold=20
  } else{
    stop("unit should be either 'octa' or 'percent'")
  }
  return(number.days.op.threshold(temp=ci@data$cloud, date.factor=ci@date.factors[[match.arg(freq)]], 
                                  threshold=threshold, op="<=") * ci@namasks[[match.arg(freq)]]$cloud)
}