#' Get the last month and day of the year
#'
#' Get the last month and day of the year as a character sting, separated by
#' the specified separator.
#'
#' This is a utility function necessitated by 360-day calendars. Works on PCICt objects.
#'
#' @param d An exemplar date.
#' @param sep Separator to use.
#' @return A string (like "12-30", or "12-31")
#'
#' @examples
#' library(PCICt)
#' last.mday <- get.last.monthday.of.year(as.PCICt("2011-01-01", cal="360"))
#'
#' @export
get.last.monthday.of.year <- function(d, sep = "-") {
  if (!is.null(attr(d, "months")))
    paste("12", attr(d, "months")[12], sep = sep)
  else
    paste("12", "31", sep = sep)
}

## Lower overhead version of tapply
tapply.fast <-
  function (X,
            INDEX,
            FUN = NULL,
            ...,
            simplify = TRUE) {
    FUN <- if (!is.null(FUN))
      match.fun(FUN)
    
    if (!is.factor(INDEX))
      stop("INDEX must be a factor.")
    
    if (length(INDEX) != length(X))
      stop("arguments must have same length")
    
    if (is.null(FUN))
      return(INDEX)
    
    namelist <- levels(INDEX)
    ans <- lapply(split(X, INDEX), FUN, ...)
    
    ans <- unlist(ans, recursive = FALSE)
    names(ans) <- levels(INDEX)
    return(ans)
  }

## Check that climdexInput data structure is valid.
valid.climdexInput <- function(x) {
  temp.quantiles <- c(10, 25, 75, 90)
  prec.quantiles <- c(25, 75, 95, 99)
  errors <- c()
  
  separate.base <- c(
    tmax = T,
    tmin = T,
    tavg = T,
    prec = F
  )
  present.data.vars <- names(x@data)
  length.check.slots <- c("dates", "jdays")
  length.check.members <- c("date.factors", "data")
  data.lengths <- c(
    sapply(x@data, length),
    sapply(length.check.slots, function(y)
      length(slot(x, y))),
    unlist(sapply(length.check.members, function(y) {
      sapply(slot(x, y), length)
    }))
  )
  quantiles <- list(
    tmax = temp.quantiles,
    tmin = temp.quantiles,
    tavg = temp.quantiles,
    prec = prec.quantiles
  )
  
  if (!all(data.lengths == max(data.lengths)))
    errors <- c(errors,
                "Data fields, dates, and date factors must all be of the same length")
  
  ## Check that namasks have columns for each of the variables
  if (!all(c("annual", "halfyear", "seasonal", "monthly") %in% names(x@namasks)) ||
      !all(
        present.data.vars %in% names(x@namasks$annual) &
        present.data.vars %in% names(x@namasks$halfyear) &
        present.data.vars %in% names(x@namasks$seasonal) &
        present.data.vars %in% names(x@namasks$monthly)
      ))
    errors <- c(
      errors,
      "NA mask for monthly, seasonal, halfyear and annual must contain data for all variables supplied."
    )
  
  ## Check that appropriate thresholds are present.
  need.base.data <- get.num.days.in.range(x@dates, x@base.range) > 0
  errors <-
    do.call(c, c(list(errors), lapply(intersect(present.data.vars, c("tmax", "tmin", "tavg", "prec")), function(n) {
      if (is.null(quantiles[n]))
        return(NULL)
      ## FIXME: This test isn't necessarily valid and prevents calculating indices when no base period data is available.
      if (!(n %in% ls(envir = x@quantiles)))
        return(paste("Quantiles for", n, "are missing."))
      return(NULL)
    })))
  
  if (length(x@northern.hemisphere) != 1)
    errors <- c(errors, "northern.hemisphere must be of length 1.")
  
  if (length(errors) == 0)
    return(TRUE)
  else
    return(errors)
}

## Class definition declaration
#' @title climdexInput
#'
#' @description
#' The climdexInput class contains all the data necessary to compute the
#' climdex indices.
#'
#' @details
#' The \code{climdexInput} class consists of all the data necessary to compute
#' the climdex indices. Users will not need to modify any of the slots in this
#' class. That being said, users may want or need to repurpose this data for
#' further analysis. The following description of the data is aimed at that
#' audience.
#'
#' The \code{data} slot contains time series' of daily data of equal length for
#' each of the provided variables. Missing days have been replaced with NA.
#' The \code{dates} slot is the corresponding series of dates (of type PCICt)
#' for the daily data.
#'
#' The \code{quantiles} slot contains quantiles used for computing the
#' tn/tx 10/90p indices, w/csdi, r95ptot, and r99ptot. If precipitation data
#' is supplied, the 'prec' member contains the 95th and 99th percentile values
#' for precipitation within the base period. For tmin and tmax, if present each
#' will have a corresponding member in the slot. Within each of these, there
#' will be an 'inbase' and 'outbase' member, corresponding to thresholds to be
#' used within the base period (inbase) and outside the base period (outbase).
#' The 'inbase' member consists of one percentile for each day of the year,
#' computed using an n-day (default is 5-day) running window surrounding that
#' day. These percentiles are computed for at least the 10th and 90th
#' percentile of the data. For the 'outbase' member, given n years
#' of data to use as the base period, there are n * (n - 1) sets of daily
#' quantiles of the same type as those in 'inbase'.
#'
#' To ease computation of monthly and annual data, \code{date.factors}
#' contains date factors which group data into annual and monthly time
#' buckets. They are of the same length as the time series and can be reused
#' for computation of any annual or monthly aggregates.
#'
#' The climdexInput class also includes NA masks for both monthly
#' and annual as parts of the \code{namasks} slot. Each of these masks consist
#' of a vector of numbers of the same length as the monthly or annual output
#' data. The values used are 1 to signify that the data meets the QC criteria,
#' and NA to signify it does not. Years with more than (by default) 15 days
#' missing, and months with more than (by default) 3 days missing, are
#' considered to be of poor quality and are masked here with NA. These
#' thresholds can be set when instantiating the object, and are stored in the
#' \code{max.missing.days} slot.
#'
#' The \code{base.range} slot contains vector of type PCICt containing the
#' first and last day included in the baseline.
#'
#' The \code{northern.hemisphere} slot contains a boolean indicating whether
#' the data came from the northern hemisphere. If FALSE, data is assumed to
#' have come from the southern hemisphere. This is used when computing growing
#' season length; if the data is from the southern hemisphere, growing season
#' length is the growing season starting in the beginning of July of the year
#' indicated, running to the end of June of the following year.
#'
#' The \code{max.missing.days} slot is a vector consisting of 'annual'
#' (the number of days that can be missing in a year) and 'monthly' (the
#' number of days that can be missing in a month. If one month in a year fails
#' the test, the corresponding year will be omitted.
#'
#' @name climdexInput
#' @aliases climdexInput-class
#' @docType class
#' @section Slots: \describe{
#' \item{data}{Time series of supplied data variables.}
#' \item{quantiles}{Threshold quantiles used for threshold-based indices.}
#' \item{namasks}{Data quality masks for annual and monthly data.}
#' \item{dates}{Date sequence (type PCICt) corresponding to temperature and
#' precipitation data.}
#' \item{jdays}{Julian days for the date sequence.}
#' \item{base.range}{Date range (type PCICt) of baseline period.}
#' \item{date.factors}{Factors used for creation of annual and monthly indices.}
#' \item{northern.hemisphere}{Boolean used when computing growing season
#' length.}
#' \item{max.missing.days}{Maximum number of missing days of data for annual
#' and monthly data.}
#' }
#' @seealso \code{\link{climdexInput.csv}}, \code{\link{climdexInput.raw}}.
#' @keywords climate ts
#' @examples
#' library(PCICt)
#'
#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
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
#' @export
#'
setClass(
  "climdexInput",
  representation(
    data = "list",
    quantiles = "environment",
    namasks = "list",
    dates = "PCICt",
    jdays = "numeric",
    base.range = "PCICt",
    date.factors = "list",
    northern.hemisphere = "logical",
    max.missing.days = "numeric"
  ),
  validity = valid.climdexInput
)

## Returns PCICt field or dies
get.date.field <- function(input.data, cal, date.types) {
  valid.date.types <-
    sapply(date.types, function(x) {
      return(!inherits(try(input.data[, x$fields], silent = TRUE)
                       , "try-error"))
    })
  
  if (sum(valid.date.types) == 0) {
    stop("Could not find a workable set of date fields")
  }
  
  
  date.type <- date.types[[which(valid.date.types)[1]]]
  date.strings <- do.call(paste, input.data[, date.type$fields])
  return(as.PCICt(date.strings, format = date.type$format, cal = cal))
}

## Creates a filled series given the data, dates, and new date sequence to be used.
create.filled.series <-
  function(data, data.dates, new.date.sequence) {
    new.data <- rep(NA, length(new.date.sequence))
    data.in.new.data <-
      (data.dates >= new.date.sequence[1]) &
      (data.dates <= new.date.sequence[length(new.date.sequence)])
    indices <-
      floor(as.numeric(data.dates[data.in.new.data] - new.date.sequence[1], units =
                         "days")) + 1
    new.data[indices] <- data[data.in.new.data]
    return(new.data)
  }

## Get julian day of year
get.jdays <- function(dates) {
  return(as.POSIXlt(dates)$yday + 1)
}

## Get year
get.years <- function(dates) {
  return(as.POSIXlt(dates)$year + 1900)
}

## Get month number
get.months <- function(dates) {
  return(as.POSIXlt(dates)$mon + 1)
}

## Juggle the list so that day 366 == day 365
get.jdays.replaced.feb29 <- function(jdays) {
  indices <- which(jdays == 366)
  if (length(indices) > 0)
    jdays[rep(indices, each = 366) +-365:0] <- c(1:59, 59, 60:365)
  jdays
}

## Get set of days for bootstrap use
get.bootstrap.set <- function(dates, bootstrap.range, win.size) {
  dpy <- ifelse(is.null(attr(dates, "dpy")), 365, attr(dates, "dpy"))
  return(dates >= bootstrap.range[1] &
           dates <= bootstrap.range[2] &
           (dpy == 360 | format(
             dates, format = "%m-%d", tz = "GMT"
           ) != "02-29"))
}

## Calculate a running quantile on the data set over the bootstrap range.
## If get.bootstrap.data is TRUE, use the Zhang boostrapping method described in Xuebin Zhang et al's 2005 paper, "Avoiding Inhomogeneity in Percentile-Based Indices of Temperature Extremes" J.Clim vol 18 pp.1647-1648, "Removing the 'jump'".
## Expects PCICt for all dates
zhang.running.qtile <-
  function(x,
           dates.base,
           qtiles,
           bootstrap.range,
           include.mask = NULL,
           n = 5,
           get.bootstrap.data = FALSE,
           min.fraction.present = 0.1) {
    inset <- get.bootstrap.set(dates.base, bootstrap.range, n)
    dpy <-
      ifelse(is.null(attr(dates.base, "dpy")), 365, attr(dates.base, "dpy"))
    nyears <- floor(sum(inset) / dpy)
    
    if (!is.null(include.mask))
      x[include.mask] <- NA
    
    bs.data <- x[inset]
    
    qdat <- NULL
    if (get.bootstrap.data) {
      d <-
        .Call(
          "running_quantile_windowed_bootstrap",
          bs.data,
          n,
          qtiles,
          dpy,
          min.fraction.present,
          PACKAGE = 'climdex.pcic'
        )
      dim(d) <- c(dpy, nyears, nyears - 1, length(qtiles))
      qdat <-
        lapply(1:length(qtiles), function(x) {
          r <- d[, , , x, drop = FALSE]
          dim(r) <- dim(r)[1:3]
          r
        })
    } else {
      res <-
        running.quantile(bs.data, n, qtiles, dpy, min.fraction.present)
      qdat <- lapply(1:length(qtiles), function(x) {
        res[, x]
      })
    }
    names(qdat) <- paste("q", qtiles * 100, sep = "")
    return(qdat)
  }

## Get NA mask given threshold and split factor
get.na.mask <- function(x, f, threshold) {
  return(c(1, NA)[1 + as.numeric(tapply.fast(is.na(x), f, function(y) {
    return(sum(y) > threshold)
  }))])
}

## Get number of days within range
get.num.days.in.range <- function(x, date.range) {
  return(sum(x >= date.range[1] & x <= date.range[2]))
}


## Check that arguments to climdexInput.raw et al are complete enough and valid enough.
check.basic.argument.validity <- function(tmax = NULL,
                                          tmin = NULL,
                                          tavg = NULL,
                                          prec = NULL,
                                          snow = NULL,
                                          snow_new = NULL,
                                          wind = NULL,
                                          wind_gust = NULL,
                                          wind_dir = NULL,
                                          cloud = NULL,
                                          sun = NULL,
                                          sun_rel = NULL,
                                          tmax.dates = NULL,
                                          tmin.dates = NULL,
                                          tavg.dates = NULL,
                                          prec.dates = NULL,
                                          snow.dates = NULL,
                                          snow_new.dates = NULL,
                                          wind.dates = NULL,
                                          wind_gust.dates = NULL,
                                          wind_dir.dates = NULL,
                                          cloud.dates = NULL,
                                          sun.dates = NULL,
                                          sun_rel.dates = NULL,
                                          base.range = c(1961, 1990),
                                          n = 5) {
  
  check.var <- function(var, var.dates, var.name) {
    if (is.null(var) != is.null(var.dates))
      stop(paste("If passing in", var, ", must pass in", var, "dates too.."))
    if (!is.null(var.dates) && length(var) != length(var.dates))
      stop(paste("Length of", var.name, "data and dates do not match."))
    if (!is.null(var.dates) && !inherits(var.dates, "PCICt"))
      stop(paste(var.name, "dates must be of class PCICt."))
    if (!is.null(var) && !is.numeric(var))
      stop(paste(var.name, "must be of type numeric."))
  }
  
  check.var(tmax, tmax.dates, "tmax")
  check.var(tmin, tmin.dates, "tmin")
  check.var(tavg, tavg.dates, "tavg")
  check.var(prec, prec.dates, "prec")
  check.var(snow, snow.dates, "snow")
  check.var(snow_new, snow_new.dates, "snow_new")
  check.var(wind, wind.dates, "wind")
  check.var(wind_gust, wind_gust.dates, "wind_gust")
  check.var(wind_dir, wind_dir.dates, "wind_dir")
  check.var(cloud, cloud.dates, "cloud")
  check.var(sun, sun.dates, "sun")
  check.var(sun_rel, sun_rel.dates, "sun_rel")
  
  if (all(
    c(
      is.null(tmax),
      is.null(tmin),
      is.null(tavg),
      is.null(prec),
      is.null(snow),
      is.null(snow_new),
      is.null(wind),
      is.null(wind_gust),
      is.null(wind_dir),
      is.null(cloud),
      is.null(sun),
      is.null(sun_rel)
    )
  ))
  stop("Must supply at least one variable to calculate indices upon.")
  
  if (!(length(base.range) == 2 && is.numeric(base.range)))
    stop("Invalid base date range; expecting vector of 2 numeric years.")
  
  if (!is.numeric(n) || length(n) != 1)
    stop("n must be numeric and of length 1.")
  
  if (n != 5)
    warning("Use of n != 5 varies from the Climdex definition. Use at your own risk.")
}

## Check validity of quantile input.
check.quantile.validity <-
  function(quantiles, present.vars, days.in.base) {
    if (is.null(quantiles))
      return()
    
    if (class(quantiles) != "list")
      stop("Provided quantiles must be a list.")
    
    if (!all(present.vars %in% names(quantiles)))
      stop("Quantiles must be present for all variables provided.\n")
    
    if (!all(sapply(quantiles[names(quantiles) %in% intersect(present.vars, c("tmax", "tmin", "tavg"))], function(x) {
      "outbase" %in% names(x) &&
        all(c("q10", "q25", "q75", "q90") %in% names(x$outbase))
    })))
      stop("Temperature out-of-base quantiles must contain 10th and 90th percentiles.\n")
    
    if (any(days.in.base > 0) &&
        !all(sapply(quantiles[names(quantiles) %in% intersect(intersect(present.vars, c("tmax", "tmin", "tavg")), names(days.in.base)[days.in.base > 0])], function(x) {
          "inbase" %in% names(x) &&
            all(c("q10", "q25", "q75", "q90") %in% names(x$inbase))
        })))
      stop("Temperature in-base quantiles must contain 10th, 25th, 75th, and 90th percentiles.\n")
    
    if ("prec" %in% names(quantiles) &&
        !all(c("q25", "q75", "q95", "q99") %in% names(quantiles$prec)))
      stop("Precipitation quantiles must contain 25th, 75th, 95th and 99th percentiles.\n")
  }

get.temp.var.quantiles <-
  function(filled.data,
           date.series,
           bs.date.series,
           qtiles,
           bs.date.range,
           n,
           in.base = FALSE,
           min.base.data.fraction.present = 0.1) {
    base.data <-
      create.filled.series(filled.data, date.series, bs.date.series)
    if (in.base)
      return(list(
        outbase = zhang.running.qtile(
          base.data,
          dates.base = bs.date.series,
          qtiles = qtiles,
          bootstrap.range = bs.date.range,
          n = n,
          min.fraction.present = min.base.data.fraction.present
        ),
        inbase = zhang.running.qtile(
          base.data,
          dates.base = bs.date.series,
          qtiles = qtiles,
          bootstrap.range = bs.date.range,
          n = n,
          get.bootstrap.data = TRUE,
          min.fraction.present = min.base.data.fraction.present
        )
      ))
    else
      return(list(
        outbase = zhang.running.qtile(
          base.data,
          dates.base = bs.date.series,
          qtiles = qtiles,
          bootstrap.range = bs.date.range,
          n = n,
          min.fraction.present = min.base.data.fraction.present
        )
      ))
  }

get.prec.var.quantiles <-
  function(filled.prec,
           date.series,
           bs.date.range,
           qtiles = c(0.25, 0.75, 0.95, 0.99)) {
    wet.days <- !(is.na(filled.prec) | filled.prec < 1)
    inset <-
      date.series >= bs.date.range[1] &
      date.series <= bs.date.range[2] &
      !is.na(filled.prec) & wet.days
    pq <- quantile(filled.prec[inset], qtiles, type = 8)
    names(pq) <- paste("q", qtiles * 100, sep = "")
    return(pq)
  }

#' @title Method for getting threshold quantiles for use in computing indices
#'
#' @description
#' This function creates threshold quantiles for use with climdexInput.raw
#' or climdexInput.csv.
#'
#' @details
#' This function takes input climate data at daily resolution, and produces as
#' output a set of threshold quantiles. This data structure can then be passed
#' to climdexInput.raw or climdexInput.csv.
#'
#' For more details on arguments, see \code{\link{climdexInput.raw}}.
#'
#' @seealso \code{\link{climdex.pcic-package}}, \code{\link{climdexInput.raw}}.
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' @keywords ts climate
#'
#' @param tmax Daily maximum temperature data.
#' @param tmin Daily minimum temperature data.
#' @param prec Daily total precipitation data.
#' @param tmax.dates Dates for the daily maximum temperature data.
#' @param tmin.dates Dates for the daily minimum temperature data.
#' @param prec.dates Dates for the daily total precipitation data.
#' @template climdexInput_common_params
#' @param quantiles Threshold quantiles for supplied variables.
#' @return A set of threshold quantiles
#' @note Units are assumed to be mm/day for precipitation and degrees Celsius
#' for temperature. No units conversion is performed internally.
#'
#' @examples
#' library(PCICt)
#'
#' ## Create a climdexInput object from some data already loaded in and
#' ## ready to go.
#'
#'#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#'
#' ## Load the data in.
#' quantiles <- get.outofbase.quantiles(tmax=ec.1018935.tmax$MAX_TEMP,
#'                                      tmin=ec.1018935.tmin$MIN_TEMP,
#'                                      prec=ec.1018935.prec$ONE_DAY_PRECIPITATION,
#'                                      tmax.dates=tmax.dates,
#'                                      tmin.dates=tmin.dates,
#'                                      prec.dates=prec.dates,
#'                                      base.range=c(1971, 2000))
#'
#' @export
#'
get.outofbase.quantiles <-
  function(tmax = NULL,
           tmin = NULL,
           tavg = NULL,
           prec = NULL,
           tmax.dates = NULL,
           tmin.dates = NULL,
           tavg.dates = NULL,
           prec.dates = NULL,
           base.range = c(1961, 1990),
           n = 5,
           temp.qtiles = c(0.10, 0.25, 0.75, 0.90),
           prec.qtiles = c(0.25, 0.75, 0.95, 0.99),
           min.base.data.fraction.present = 0.1) {
    days.threshold <- 359
    check.basic.argument.validity(
      tmax = tmax,
      tmin = tmin,
      tavg = tavg,
      prec = prec,
      tmax.dates = tmax.dates,
      tmin.dates = tmin.dates,
      tavg.dates = tavg.dates,
      prec.dates = prec.dates,
      base.range = base.range,
      n = n
    )
    
    d.list <- list(tmin.dates, tmax.dates, tavg.dates, prec.dates)
    all.dates <- do.call(c, d.list[!sapply(d.list, is.null)])
    last.day.of.year <- get.last.monthday.of.year(all.dates)
    cal <- attr(all.dates, "cal")
    
    bs.date.range <-
      as.PCICt(paste(base.range, c("01-01", last.day.of.year), sep = "-"), cal =
                 cal)
    new.date.range <-
      as.PCICt(paste(as.numeric(format(
        range(all.dates), "%Y", tz = "GMT"
      )), c("01-01", last.day.of.year), sep = "-"), cal = cal)
    date.series <-
      seq(new.date.range[1], new.date.range[2], by = "day")
    bs.date.series <-
      seq(bs.date.range[1], bs.date.range[2], by = "day")
    
    quantiles <- list()
    
    if (!is.null(tmax)) {
      if (get.num.days.in.range(tmax.dates, bs.date.range) <= days.threshold)
        stop(
          "There is less than a year of tmax data within the base period. Consider revising your base range and/or check your input data."
        )
      filled.tmax <-
        create.filled.series(tmax, trunc(tmax.dates, "days"), date.series)
      quantiles$tmax <-
        get.temp.var.quantiles(filled.tmax,
                               date.series,
                               bs.date.series,
                               temp.qtiles,
                               bs.date.range,
                               n)
    }
    
    if (!is.null(tmin)) {
      if (get.num.days.in.range(tmin.dates, bs.date.range) <= days.threshold)
        stop(
          "There is less than a year of tmin data within the base period. Consider revising your base range and/or check your input data."
        )
      filled.tmin <-
        create.filled.series(tmin, trunc(tmin.dates, "days"), date.series)
      quantiles$tmin <-
        get.temp.var.quantiles(filled.tmin,
                               date.series,
                               bs.date.series,
                               temp.qtiles,
                               bs.date.range,
                               n)
    }
    
    if (!is.null(tavg)) {
      if (get.num.days.in.range(tavg.dates, bs.date.range) <= days.threshold)
        stop(
          "There is less than a year of tavg data within the base period. Consider revising your base range and/or check your input data."
        )
      filled.tavg <-
        create.filled.series(tavg, trunc(tavg.dates, "days"), date.series)
      quantiles$tavg <-
        get.temp.var.quantiles(filled.tavg,
                               date.series,
                               bs.date.series,
                               temp.qtiles,
                               bs.date.range,
                               n)
    }
    
    if (!is.null(prec)) {
      if (get.num.days.in.range(prec.dates, bs.date.range) <= days.threshold)
        stop(
          "There is less than a year of prec data within the base period. Consider revising your base range and/or check your input data."
        )
      filled.prec <-
        create.filled.series(prec, trunc(prec.dates, "days"), date.series)
      quantiles$prec <-
        get.prec.var.quantiles(filled.prec, date.series, bs.date.range, prec.qtiles)
    }
    return(quantiles)
  }

#' @title Method for creating climdexInput object from vectors of data
#'
#' @description
#' This function creates a climdexInput object from data already ingested into
#' R.
#'
#' @details
#' This function takes input climate data at daily resolution, and produces as
#' output a ClimdexInput data structure. This data structure can then be passed
#' to any of the routines used to compute the Climdex indices. The indices
#' themselves are specified on the webpage cited in the references section.
#' The \code{base.range} argument is a pair of 4 digit years which bound the
#' data on which the base percentiles are calculated.
#'
#' The \code{tmax}, \code{tmin}, and \code{prec} arguments are numeric vectors
#' containing the data on which the indices are to be computed. The units are
#' assumed to be degrees C for temperature, and mm/day for precipitation.
#'
#' The \code{tmax.dates}, \code{tmin.dates}, and \code{prec.dates} arguments
#' are vectors of type \code{PCICt}.
#'
#' The \code{n} argument specifies the size of the window used when computing
#' the percentiles used in \code{\link{climdex.tx10p}},
#' \code{\link{climdex.tn10p}}, \code{\link{climdex.tx90p}}, and
#' \code{\link{climdex.tn90p}}.
#'
#' The \code{northern.hemisphere} argument specifies whether the data came from
#' the northern hemisphere. If FALSE, data is assumed to have come from the
#' southern hemisphere. This is used when computing growing season length; if
#' the data is from the southern hemisphere, growing season length is the
#' growing season starting in the beginning of July of the year indicated,
#' running to the end of June of the following year.
#'
#' The \code{quantiles} argument allows the user to supply pre-computed quantiles.
#' This is a list consisting of quantiles for each variable.
#'
#' For each temperature variable, there are separate lists of quantiles for
#' inbase and outbase, with these names. In both cases, quantiles within these
#' lists are named q10 for the 10th percentile and q90 for the 90th percentile.
#' Other percentiles would be named qnn for the nnth percentile. For the
#' outbase quantiles, each element in the list is a vector of length 365 (or 360
#' in the case of 360-day calendars), corresponding to one value for each day of
#' the year. For the inbase quantiles, each element in the list is an array of
#' dimensions [365 or 360, nyr, nyr - 1], where nyr is the number of years in
#' the base period. Each value corresponds to a quantile for each day, for each
#' year, with a particular year replaced.
#'
#' For precipitation variables, there is a named vector of quantiles, consisting
#' of at least q95 and q99.
#'
#' The \code{temp.qtiles} and \code{prec.qtiles} arguments allow the user to
#' modify the quantiles calculated. For example, specifying
#' temp.qtiles=c(0.10, 0.50, 0.90) would calculate the 10th, 50th, and 90th
#' percentiles for temperature.
#'
#' The \code{min.base.fraction.present} argument specifies the minimum fraction
#' of data which must be present for a quantile to be calculated for a
#' particular day. If the fraction of data present is less than this threshold,
#' the quantile for that day will be set to NA.
#'
#' The \code{max.missing.days} argument is a vector consisting of 'annual'
#' (the number of days that can be missing in a year) and 'monthly' (the
#' number of days that can be missing in a month. If one month in a year fails
#' the test, the corresponding year will be omitted.
#'
#' @seealso \code{\link{climdex.pcic-package}}, \code{\link{strptime}}.
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' @keywords ts climate
#'
#' @template climdexInput_raw_help1
#' @template climdexInput_raw_params
#' @template climdexInput_common_params
#' @param northern.hemisphere Whether this point is in the northern hemisphere.
#' @param quantiles Threshold quantiles for supplied variables.
#' @param max.missing.days Vector containing thresholds for number of days
#' allowed missing per year (annual) and per month (monthly).
#' @return An object of class \code{\link{climdexInput-class}} for use with
#' other climdex methods.
#' @note Units are assumed to be mm/day for precipitation and degrees Celsius
#' for temperature. No units conversion is performed internally.
#'
#' @examples
#' library(PCICt)
#'
#' ## Create a climdexInput object from some data already loaded in and
#' ## ready to go.
#'
#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
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
#' @export
climdexInput.raw <- function(tmax = NULL,
                             tmax.dates = NULL,
                             tmin = NULL,
                             tmin.dates = NULL,
                             tavg = NULL,
                             tavg.dates = NULL,
                             prec = NULL,
                             prec.dates = NULL,
                             snow = NULL,
                             snow.dates = NULL,
                             snow_new = NULL,
                             snow_new.dates = NULL,
                             wind = NULL,
                             wind.dates = NULL,
                             wind_gust = NULL,
                             wind_gust.dates = NULL,
                             wind_dir = NULL,
                             wind_dir.dates = NULL,
                             cloud = NULL,
                             cloud.dates = NULL,
                             sun = NULL,
                             sun.dates = NULL,
                             sun_rel = NULL,
                             sun_rel.dates = NULL,
                             quantiles = NULL,
                             temp.qtiles = c(0.10, 0.25, 0.75, 0.90),
                             prec.qtiles = c(0.25, 0.75, 0.95, 0.99),
                             base.range = c(1961, 1990),
                             n = 5,
                             northern.hemisphere = TRUE,
                             max.missing.days = c(
                               annual = 15,
                               halfyear = 10,
                               seasonal = 8,
                               monthly = 3
                             ),
                             min.base.data.fraction.present = 0.1) {
  ## Make sure all of these arguments are valid...
  check.basic.argument.validity(
    tmax = tmax,
    tmin = tmin,
    tavg = tavg,
    prec = prec,
    snow = snow,
    snow_new = snow_new,
    wind = wind,
    wind_gust = wind_gust,
    wind_dir = wind_dir,
    cloud = cloud,
    sun = sun,
    sun_rel = sun_rel,
    tmax.dates = tmax.dates,
    tmin.dates = tmin.dates,
    tavg.dates = tavg.dates,
    prec.dates = prec.dates,
    snow.dates = snow.dates,
    snow_new.dates = snow_new.dates,
    wind.dates = wind.dates,
    wind_gust.dates = wind_gust.dates,
    wind_dir.dates = wind_dir.dates,
    cloud.dates = cloud.dates,
    sun.dates = sun.dates,
    sun_rel.dates = sun_rel.dates,
    base.range = base.range,
    n = n
  )
  
  stopifnot(length(max.missing.days) == 4 &&
              all(
                c("annual", "halfyear", "seasonal", "monthly") %in% names(max.missing.days)
              ))
  
  stopifnot(
    is.numeric(min.base.data.fraction.present) &&
      length(min.base.data.fraction.present) == 1
  )
  
  d.list <- list(
    tmin.dates,
    tmax.dates,
    tavg.dates,
    prec.dates,
    snow.dates,
    snow_new.dates,
    wind.dates,
    wind_gust.dates,
    wind_dir.dates,
    cloud.dates,
    sun.dates,
    sun_rel.dates
  )
  
  all.dates <- do.call(c, d.list[!sapply(d.list, is.null)])
  last.day.of.year <- get.last.monthday.of.year(all.dates)
  cal <- attr(all.dates, "cal")
  
  ## Convert base range (in years) to PCICt
  bs.date.range <-
    as.PCICt(paste(base.range, c("01-01", last.day.of.year), sep = "-"), cal =
               cal)
  bs.date.series <-
    seq(bs.date.range[1], bs.date.range[2], by = "day")
  
  ## Get dates for normal data
  new.date.range <-
    as.PCICt(paste(as.numeric(format(
      range(all.dates), "%Y", tz = "GMT"
    )), c("01-01", last.day.of.year), sep = "-"), cal = cal)
  date.series <-
    seq(new.date.range[1], new.date.range[2], by = "day")
  jdays <- get.jdays.replaced.feb29(get.jdays(date.series))
  
  ## Factors for dividing data up
  date.months <-
    as.numeric(format(date.series, format = "%m", tz = "GMT"))
  date.years  <-
    as.numeric(format(date.series, format = "%Y", tz = "GMT"))
  # get factors for seasons
  # Winter month D of prev year and JF of next year belong together, Year belongs to Jan => increase year of prev Dec by 1
  seas.years <- date.years
  seas.seas  <- date.months %/% 3 + 1
  seas.idx   <- which(seas.seas == 5)
  seas.years[seas.idx] <- seas.years[seas.idx] + 1
  seas.seas[seas.idx]  <- 1
  # get factors for half years (winter (ONDJFM) & summer (APJJAS))
  # winter months OND of prev year and JFM of next year belong together, Year belongs to Jan => increase year of prev OND by 1
  half.years <- date.years
  half.half  <- (date.months + 2) %/% 6 + 1
  half.idx   <- which(half.half == 3)
  half.years[half.idx] <- half.years[half.idx] + 1
  half.half[half.idx]  <- 1
  
  # set up date.factors list
  date.factors <-
    list(
      annual = factor(format(
        date.series, format = "%Y", tz = "GMT"
      )),
      halfyear = factor(paste(half.years, half.half, sep =
                                "-")),
      seasonal = factor(paste(seas.years, seas.seas, sep =
                                "-")),
      monthly = factor(format(
        date.series, format = "%Y-%m", tz = "GMT"
      ))
    )
  
  ## Filled data...
  var.list <-
    c(
      "tmax",
      "tmin",
      "tavg",
      "prec",
      "snow",
      "snow_new",
      "wind",
      "wind_gust",
      "wind_dir",
      "cloud",
      "sun",
      "sun_rel"
    )
  
  present.var.list <-
    var.list[sapply(var.list, function(x)
      ! is.null(get(x)))]
  
  filled.list <- sapply(present.var.list, function(x) {
    return(create.filled.series(get(x),
                                trunc(get(
                                  paste(x, "dates", sep = ".")
                                )),
                                date.series))
  }, simplify = FALSE)
  
  if (is.null(tavg) && !is.null(tmin) && !is.null(tmax)) {
    filled.list$tavg <- (filled.list$tmax + filled.list$tmin) / 2
    
    # idx.finite <- !is.na(filled.list$tavg)
    # tavg <- tavg[idx.finite]
    # tavg.dates <- date.series[idx.finite]
    tavg.dates <- date.series
  }
  
  ## Establish some truth values for later use in logic...
  days.threshold <- 359
  present.dates <-
    sapply(present.var.list, function(x)
      get(paste(x, "dates", sep = ".")))
  quantile.dates <-
    list(
      tmax = tmax.dates,
      tmin = tmin.dates,
      tavg = tavg.dates,
      prec = prec.dates
    )
  days.in.base <-
    sapply(quantile.dates, get.num.days.in.range, bs.date.range)
  
  ## Check that provided quantiles, if any, are valid
  check.quantile.validity(quantiles, present.var.list, days.in.base)
  
  data.in.base.period <- any(days.in.base != 0)
  have.quantiles <- all(present.var.list %in% names(quantiles))
  
  ## NA masks
  ## NA masks
  namasks <-
    list(
      annual = lapply(
        filled.list,
        get.na.mask,
        date.factors$annual,
        max.missing.days['annual']
      ),
      halfyear = lapply(
        filled.list,
        get.na.mask,
        date.factors$halfyear,
        max.missing.days['halfyear']
      ),
      seasonal = lapply(
        filled.list,
        get.na.mask,
        date.factors$seasonal,
        max.missing.days['seasonal']
      ),
      monthly = lapply(
        filled.list,
        get.na.mask,
        date.factors$monthly,
        max.missing.days['monthly']
      )
    )
  
  ## Pad data passed as base if we're missing endpoints...
  if (!have.quantiles) {
    quantiles <- new.env(parent = emptyenv())
    
    if (days.in.base['tmax'] > days.threshold)
      delayedAssign(
        "tmax",
        get.temp.var.quantiles(
          filled.list$tmax,
          date.series,
          bs.date.series,
          temp.qtiles,
          bs.date.range,
          n,
          TRUE,
          min.base.data.fraction.present
        ),
        assign.env = quantiles
      )
    if (days.in.base['tmin'] > days.threshold)
      delayedAssign(
        "tmin",
        get.temp.var.quantiles(
          filled.list$tmin,
          date.series,
          bs.date.series,
          temp.qtiles,
          bs.date.range,
          n,
          TRUE,
          min.base.data.fraction.present
        ),
        assign.env = quantiles
      )
    if (days.in.base['tavg'] > days.threshold)
      delayedAssign(
        "tavg",
        get.temp.var.quantiles(
          filled.list$tavg,
          date.series,
          bs.date.series,
          temp.qtiles,
          bs.date.range,
          n,
          TRUE,
          min.base.data.fraction.present
        ),
        assign.env = quantiles
      )
    if (days.in.base['prec'] > days.threshold)
      delayedAssign(
        "prec",
        get.prec.var.quantiles(filled.list$prec, date.series, bs.date.range, prec.qtiles),
        assign.env = quantiles
      )
  } else {
    quantiles <- as.environment(quantiles)
  }
  
  return(
    new(
      "climdexInput",
      data = filled.list,
      quantiles = quantiles,
      namasks = namasks,
      dates = date.series,
      jdays = jdays,
      base.range = bs.date.range,
      date.factors = date.factors,
      northern.hemisphere = northern.hemisphere,
      max.missing.days = max.missing.days
    )
  )
}

#' @title Method for creating climdexInput object from CSV files
#'
#' @description
#' This function creates a climdexInput object from data in CSV files.
#'
#' @details
#' This function takes input climate data in CSV files at daily resolution,
#' and produces as output a ClimdexInput data structure. This data structure
#' can then be passed to any of the routines used to compute the Climdex
#' indices. The indices themselves are specified on the webpage cited in the
#' references section.
#'
#' Any of tmin.file (daily minimum temperature), tmax.file (daily maximum
#' temperature), tavg.file (daily mean temperature), and prec.file (daily
#' precipitation) can be passed in. tavg will be derived from the mean of
#' tmax and tmin if it is not supplied. If any of tmin.file, tmax.file, and
#' prec.file are not supplied, the set of indices which can be calculated will
#' be limited to indices which do not involve the missing variables.
#'
#' The \code{tmax.file}, \code{tmin.file}, and \code{prec.file} arguments
#' should be names of CSV files containing dates and the data on which the
#' indices are to be computed. The units are assumed to be degrees C for
#' temperature, and mm/day for precipitation.
#'
#' The \code{data.columns} argument is a vector consisting of named items tmax,
#' tmin, and prec. These named items are used as the column names in their
#' respective files when loading in CSV.
#'
#' The \code{cal} argument is a textual description of the calendar type, as
#' described in the documentation for \code{\link{as.PCICt}}.
#'
#' The \code{date.types} argument is a list of lists containing two named
#' items: \code{fields}, and \code{format}. The \code{fields} item is a vector
#' of names consisting of the columns to be concatenated together with spaces.
#' The \code{format} item is a date format as taken by \code{strptime}.
#'
#' For more details on arguments, see \code{\link{climdexInput.raw}}.
#'
#' @seealso \code{\link{climdex.pcic-package}}, \code{\link{climdexInput.raw}}.
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' @keywords ts climate
#'
#' @param tmax.file Name of file containing daily maximum temperature data.
#' @param tmin.file Name of file containing daily minimum temperature data.
#' @param prec.file Name of file containing daily total precipitation data.
#' @param tavg.file Name of file containing daily mean temperature data.
#' @param snow.file Name of file containing daily mean snow height data.
#' @param snow_new.file Name of file containing daily mean new snow height data.
#' @param wind.file Name of file containing daily mean wind speed data.
#' @param wind_gust.file Name of file containing daily wind gust data.
#' @param wind_dir.file Name of file containing daily mean wind direction data.
#' @param cloud.file Name of file containing daily mean cloud cover data.
#' @param sun.file Name of file containing daily mean sunshine duration data.
#' @param sun_rel.file Name of file containing daily mean relative sunshine duration data.
#' @param data.columns Column names for tmin, tmax, and prec data.
#' @param date.types Column names for tmin, tmax, and prec data (see notes).
#' @param na.strings Strings used for NA values; passed to
#' \code{\link{read.csv}}.
#' @param cal The calendar type used in the input files.
#' @template climdexInput_common_params
#' @param northern.hemisphere Whether this point is in the northern hemisphere.
#' @param quantiles Threshold quantiles for supplied variables.
#' @param max.missing.days Vector containing thresholds for number of days
#' allowed missing per year (annual) and per month (monthly).
#' @return An object of class \code{\link{climdexInput-class}} for use with
#' other climdex methods.
#' @note Units are assumed to be mm/day for precipitation and degrees Celsius
#' for temperature. No units conversion is performed internally.
#' @examples
#' ## This would create a climdexInput object from a set of filenames (already
#' ## stored as variables), with a different date format.
#' \dontrun{ci.csv <- climdexInput.csv(tmax.filename, tmin.filename,
#' prec.filename, date.types=list(list(fields=c("date"), format="%Y-%m-%d")))}
#'
#' @export
climdexInput.csv <-
  function(tmax.file = NULL,
           tmin.file = NULL,
           tavg.file = NULL,
           prec.file = NULL,
           snow.file = NULL,
           snow_new.file = NULL,
           wind.file = NULL,
           wind_gust.file = NULL,
           wind_dir.file = NULL,
           cloud.file = NULL,
           sun.file = NULL,
           sun_rel.file = NULL,
           data.columns = list(
             tmin = "tmin",
             tmax = "tmax",
             tavg = "tavg",
             prec = "prec",
             snow = "snow",
             snow_new = "snow_new",
             wind = "wind",
             wind_gust = "wind_gust",
             wind_dir = "wind_dir",
             cloud = "cloud",
             sun = "sun",
             sun_rel = "sun_rel"
           ),
           base.range = c(1961, 1990),
           na.strings = NULL,
           cal = "gregorian",
           date.types = NULL,
           n = 5,
           northern.hemisphere = TRUE,
           quantiles = NULL,
           temp.qtiles = c(0.10, 0.25, 0.75, 0.90),
           prec.qtiles = c(0.25, 0.75, 0.95, 0.99),
           max.missing.days = c(
             annual = 15,
             halfyear = 10,
             seasonal = 8,
             monthly = 3
           ),
           min.base.data.fraction.present = 0.1) {
    
    get.and.check.data <- function(fn, datacol) {
      if (!is.null(fn)) {
        dat <- read.csv(fn, na.strings = na.strings)
        if (!(datacol %in% names(dat)))
          stop("Data column not found in tmin data.")
        return(list(dat = dat[!is.na(dat[, datacol]), datacol],  dates = get.date.field(dat, cal, date.types)))
      }
      return(list(dat = NULL, dates = NULL))
    }
    
    if (missing(date.types))
      date.types <-
        list(list(fields = c("year", "jday"), format = "%Y %j"),
             list(
               fields = c("year", "month", "day"),
               format = "%Y %m %d"
             ))
    else
      if (any(!sapply(date.types, function(x) {
        return(sum(c("fields", "format") %in% names(x)) == 2 &&
               is.character(x$fields) && is.character(x$format))
      })))
        stop("Invalid date.types specified. See ?climdexInput.csv .")
    
    tmin <- get.and.check.data(tmin.file, data.columns$tmin)
    tmax <- get.and.check.data(tmax.file, data.columns$tmax)
    tavg <- get.and.check.data(tavg.file, data.columns$tavg)
    prec <- get.and.check.data(prec.file, data.columns$prec)
    snow <- get.and.check.data(snow.file, data.columns$snow)
    snow_new <-
      get.and.check.data(snow_new.file, data.columns$snow_new)
    wind <- get.and.check.data(wind.file, data.columns$wind)
    wind_gust <-
      get.and.check.data(wind_gust.file, data.columns$wind_gust)
    wind_dir <-
      get.and.check.data(wind_dir.file, data.columns$wind_dir)
    cloud <- get.and.check.data(cloud.file, data.columns$cloud)
    sun <- get.and.check.data(sun.file, data.columns$sun)
    sun_rel <-
      get.and.check.data(sun_rel.file, data.columns$sun_rel)
    
    return(
      climdexInput.raw(
        tmax = tmax$dat,
        tmax.dates = tmax$dates,
        tmin = tmin$dat,
        tmin.dates = tmin$dates,
        tavg = tavg$dat,
        tavg.dates = tavg$dates,
        prec = prec$dat,
        prec.dates = prec$dates,
        snow = snow$dat,
        snow.dates = snow$dates,
        snow_new = snow_new$dat,
        snow_new.dates = snow_new$dates,
        wind = wind$dat,
        wind.dates = wind$dates,
        wind_gust = wind_gust$dat,
        wind_gust.dates = wind_gust$dates,
        wind_dir = wind_dir$dat,
        wind_dir.dates = wind_dir$dates,
        cloud = cloud$dat,
        cloud.dates = cloud$dates,
        sun = sun$dat,
        sun.dates = sun$dates,
        sun_rel = sun_rel$dat,
        sun_rel.dates = sun_rel$dates,
        base.range = base.range,
        n = n,
        northern.hemisphere = northern.hemisphere,
        quantiles = quantiles,
        temp.qtiles = temp.qtiles,
        prec.qtiles = prec.qtiles,
        max.missing.days = max.missing.days,
        min.base.data.fraction.present = min.base.data.fraction.present
      )
    )
  }

#' Frost Days
#'
#' This function computes the climdex index FD.
#'
#' This function takes a climdexInput object as input and computes the FD (frost
#' days) climdex index: that is, the annual count of days where daily minimum
#' temperature drops below 0 degrees Celsius.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the number of frost days for each year.
#' @template generic_seealso_references
#'
#' @templateVar cdxvar fd
#' @templateVar cdxdescription an annual timeseries of the number of frost days.
#' @template get_generic_example
#'
#' @export
climdex.fd <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$tmin))
    return(
      number.days.op.threshold(ci@data$tmin, ci@date.factors[[match.arg(freq)]], 0, "<") * ci@namasks[[match.arg(freq)]]$tmin
    )
  }

#' Summer Days
#'
#' This function computes the climdex index SU.
#'
#' This function takes a climdexInput object as input and computes the SU (summer
#' days) climdex index: that is, the annual count of days where daily maximum
#' temperature exceeds 25 degrees Celsius.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the number of summer days for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar su
#' @templateVar cdxdescription an annual timeseries of the number of summer days.
#' @template get_generic_example
#'
#' @export
climdex.su <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$tmax))
    return(
      number.days.op.threshold(ci@data$tmax, ci@date.factors[[match.arg(freq)]], 25, ">") * ci@namasks[[match.arg(freq)]]$tmax
    )
  }

#' Icing Days
#'
#' This function computes the climdex index ID.
#'
#' This function takes a climdexInput object as input and computes the ID (icing
#' days) climdex index: that is, the annual count of days where daily maximum
#' temperature is below 0 degrees Celsius.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the number of icing days for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar id
#' @templateVar cdxdescription an annual timeseries of the number of icing days.
#' @template get_generic_example
#'
#' @export
climdex.id <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$tmax))
    return(
      number.days.op.threshold(ci@data$tmax, ci@date.factors[[match.arg(freq)]], 0, "<") * ci@namasks[[match.arg(freq)]]$tmax
    )
  }

#' Tropical Nights
#'
#' This function computes the climdex index TR.
#'
#' This function takes a climdexInput object as input and computes the TR
#' (tropical nights) climdex index: that is, the annual count of days where
#' daily minimum temperature stays above 20 degrees Celsius.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the number of frost days for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar tr
#' @templateVar cdxdescription an annual timeseries of the number of tropical nights.
#' @template get_generic_example
#'
#' @export
climdex.tr <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$tmin))
    return(
      number.days.op.threshold(ci@data$tmin, ci@date.factors[[match.arg(freq)]], 20, ">") * ci@namasks[[match.arg(freq)]]$tmin
    )
  }

#' @title Growing Season Length
#'
#' @description
#' This function computes the growing season length (GSL) given the input.
#'
#' @details
#' This function takes a climdexInput object as input and computes the growing
#' season length based on this data.
#'
#' Growing season length as defined by the climdex indices is the number of
#' days between the start of the first spell of warm days in the first half of
#' the year, and the start of the first spell of cold days in the second half
#' of the year. Spells of warm days are defined as six or more days with mean
#' temperature above 5 degrees Celsius; spells of cold days are defined as six
#' or more days with a mean temperature below 5 degrees Celsius.
#'
#' The three alternate modes provided ('GSL_first', 'GSL_max', and 'GSL_sum')
#' are for testing purposes only. They differ considerably from the first
#' ('GSL') mode. All of them use a list of growing seasons -- here defined as
#' six or more consecutive days with a mean temperature greater than or equal
#' to 5 degrees Celsius, followed by either the end of the year or six or more
#' consecutive days with a mean temperature less than 5 degrees Celsius.
#' 'GSL_first' returns the first growing season found; 'GSL_max' returns the
#' longest growing season found; and 'GSL_sum' returns the total length of all
#' growing seasons found.
#'
#' @param ci Object of type climdexInput.
#' @param gsl.mode Growing season length method to use.
#' @return A vector containing the number of days in the growing season for
#' each year.
#' @note Note that fclimdex results may differ from results using the first
#' ('GSL') mode due to bugs in fclimdex. Please ensure you are using the latest
#' version of fclimdex, as there have been numerous bug fixes and the results
#' should, at this point, match.
#'
#' Please do not use the 'GSL_first', 'GSL_max', or 'GSL_sum' modes for
#' anything other than testing purposes at this time, nor should you rely on
#' this parameter being present in future versions of climdex.pcic.
#' @seealso \code{\link{growing.season.length}},
#' \code{\link{climdexInput.csv}}.
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' @keywords ts climate
#' @templateVar cdxvar gsl
#' @templateVar cdxdescription an annual timeseries of the growing season length in days.
#' @template get_generic_example
#'
#' @export
climdex.gsl <-
  function(ci,
           gsl.mode = c("GSL", "GSL_first", "GSL_max", "GSL_sum")) {
    stopifnot(!is.null(ci@data$tavg))
    ## Gotta shift dates so that July 1 is considered Jan 1 of same year in southern hemisphere
    if (ci@northern.hemisphere) {
      return(
        growing.season.length(
          ci@data$tavg,
          ci@date.factors$annual,
          ci@dates,
          ci@northern.hemisphere,
          gsl.mode = match.arg(gsl.mode)
        ) * ci@namasks$annual$tavg
      )
    } else {
      dates.POSIXlt <- as.POSIXlt(ci@dates)
      years <- dates.POSIXlt$year + 1900
      months <- dates.POSIXlt$mon + 1
      
      valid.years <- range(years)
      years.gsl <- years - floor((12 - months) / 6)
      
      inset <- years.gsl >= valid.years[1]
      gsl.factor <- factor(years.gsl[inset])
      gsl.factor.monthly <-
        factor(paste(years.gsl[inset], months[inset], sep = "-"))
      gsl.yearmonth.factor <-
        unlist(strsplit(levels(gsl.factor.monthly), "-"))[(0:(nlevels(gsl.factor.monthly) - 1)) * 2 + 1]
      gsl.temp.data <- ci@data$tavg[inset]
      namask.gsl.monthly <-
        get.na.mask(gsl.temp.data, gsl.factor.monthly, ci@max.missing.days['annual'])
      namask.gsl <-
        get.na.mask(gsl.temp.data, gsl.factor, ci@max.missing.days['annual']) * as.numeric(tapply(namask.gsl.monthly, gsl.yearmonth.factor, prod))
      dim(namask.gsl) <- dimnames(namask.gsl) <- NULL
      namask.gsl[length(namask.gsl)] <- NA
      return((
        growing.season.length(
          gsl.temp.data,
          gsl.factor,
          ci@dates[inset],
          ci@northern.hemisphere,
          gsl.mode = match.arg(gsl.mode)
        ) * namask.gsl
      ))
    }
  }

#' Monthly Maximum of Daily Maximum Temperature
#'
#' This function computes the climdex index TXx.
#'
#' This function takes a climdexInput object as input and computes
#' the monthly or annual maximum of daily maximum temperature.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each month.
#' @template generic_seealso_references
#' @templateVar cdxvar txx
#' @templateVar cdxdescription a monthly timeseries of maximum daily maximum temperature.
#' @template get_generic_example
#'
#' @export
climdex.txx <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$tmax))
    return(suppressWarnings(
      tapply.fast(ci@data$tmax, ci@date.factors[[match.arg(freq)]], max, na.rm =
                    TRUE)
    ) * ci@namasks[[match.arg(freq)]]$tmax)
  }

#' Monthly Maximum of Daily Minimum Temperature
#'
#' This function computes the climdex index TNx.
#'
#' This function takes a climdexInput object as input and computes
#' the monthly or annual maximum of daily minimum temperature.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each month.
#' @template generic_seealso_references
#' @templateVar cdxvar tnx
#' @templateVar cdxdescription a monthly timeseries of maximum daily minimum temperature.
#' @template get_generic_example
#'
#' @export
climdex.tnx <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$tmin))
    return(suppressWarnings(
      tapply.fast(ci@data$tmin, ci@date.factors[[match.arg(freq)]], max, na.rm =
                    TRUE)
    ) * ci@namasks[[match.arg(freq)]]$tmin)
  }

#' Monthly Minimum of Daily Maximum Temperature
#'
#' This function computes the climdex index TXn.
#'
#' This function takes a climdexInput object as input and computes
#' the monthly or annual minimum of daily maximum temperature.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each month.
#' @template generic_seealso_references
#' @templateVar cdxvar txn
#' @templateVar cdxdescription a monthly timeseries of minimum daily maximum temperature.
#' @template get_generic_example
#'
#' @export
climdex.txn <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$tmax))
    return(suppressWarnings(
      tapply.fast(ci@data$tmax, ci@date.factors[[match.arg(freq)]], min, na.rm =
                    TRUE)
    ) * ci@namasks[[match.arg(freq)]]$tmax)
  }

#' Monthly Minimum of Daily Minimum Temperature
#'
#' This function computes the climdex index TNn.
#'
#' This function takes a climdexInput object as input and computes
#' the monthly or annual minimum of daily minimum temperature.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each month.
#' @template generic_seealso_references
#' @templateVar cdxvar tnn
#' @templateVar cdxdescription a monthly timeseries of minimum daily minimum temperature.
#' @template get_generic_example
#'
#' @export
climdex.tnn <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$tmin))
    return(suppressWarnings(
      tapply.fast(ci@data$tmin, ci@date.factors[[match.arg(freq)]], min, na.rm =
                    TRUE)
    ) * ci@namasks[[match.arg(freq)]]$tmin)
  }

## Our implementation currently follows the example set by fclimdex for dealing with missing values, which is wrong; it biases results upwards when missing values are present.

#' Percent of Values Below 10th Percentile Daily Minimum Temperature
#'
#' This function computes the climdex index TN10p.
#'
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values below the 10th percentile of baseline
#' daily minimum temperature.
#'
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' @templateVar cdxvar tn10p
#' @templateVar cdxdescription a monthly timeseries of the TN10p index.
#' @template get_generic_example
#'
#' @export
climdex.tn10p <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$tmin) &&
                !is.null(ci@quantiles$tmin))
    return(
      percent.days.op.threshold(
        ci@data$tmin,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tmin$outbase$q10,
        ci@quantiles$tmin$inbase$q10,
        ci@base.range,
        "<",
        ci@max.missing.days[match.arg(freq)]
      ) * ci@namasks[[match.arg(freq)]]$tmin
    )
  }

#' Percent of Values Below 10th Percentile Daily Maximum Temperature
#'
#' This function computes the climdex index TX10p.
#'
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values below the 10th percentile of baseline
#' daily maximum temperature.
#'
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' @templateVar cdxvar tx10p
#' @templateVar cdxdescription a monthly timeseries of the TX10p index.
#' @template get_generic_example
#'
#' @export
climdex.tx10p <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$tmax) &&
                !is.null(ci@quantiles$tmax))
    return(
      percent.days.op.threshold(
        ci@data$tmax,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tmax$outbase$q10,
        ci@quantiles$tmax$inbase$q10,
        ci@base.range,
        "<",
        ci@max.missing.days[match.arg(freq)]
      ) * ci@namasks[[match.arg(freq)]]$tmax
    )
  }

#' Percent of Values Above 90th Percentile Daily Minimum Temperature
#'
#' This function computes the climdex index TN90p.
#'
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values above the 90th percentile of baseline
#' daily minimum temperature.
#'
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' @templateVar cdxvar tn90p
#' @templateVar cdxdescription a monthly timeseries of the TN90p index.
#' @template get_generic_example
#'
#' @export
climdex.tn90p <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$tmin) &&
                !is.null(ci@quantiles$tmin))
    return(
      percent.days.op.threshold(
        ci@data$tmin,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tmin$outbase$q90,
        ci@quantiles$tmin$inbase$q90,
        ci@base.range,
        ">",
        ci@max.missing.days[match.arg(freq)]
      ) * ci@namasks[[match.arg(freq)]]$tmin
    )
  }

#' Percent of Values Above 90th Percentile Daily Maximum Temperature
#'
#' This function computes the climdex index TX90p.
#'
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values above the 90th percentile of baseline
#' daily maximum temperature.
#'
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' @templateVar cdxvar tx90p
#' @templateVar cdxdescription a monthly timeseries of the TX90p index.
#' @template get_generic_example
#'
#' @export
climdex.tx90p <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$tmax) &&
                !is.null(ci@quantiles$tmax))
    return(
      percent.days.op.threshold(
        ci@data$tmax,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tmax$outbase$q90,
        ci@quantiles$tmax$inbase$q90,
        ci@base.range,
        ">",
        ci@max.missing.days[match.arg(freq)]
      ) * ci@namasks[[match.arg(freq)]]$tmax
    )
  }

#' @title Warm Spell Duration Index
#'
#' @description
#' This function computes the climdex index WSDI.
#'
#' @details
#' This function takes a climdexInput object as input and computes the climdex
#' index WSDI (Warm Spell Duration Index).
#'
#' The warm spell duration index is defined as the number of days each year
#' which are part of a "warm spell". A "warm spell" is defined as a sequence of
#' 6 or more days in which the daily maximum temperature exceeds the 90th
#' percentile of daily maximum temperature for a 5-day running window
#' surrounding this day during the baseline period.
#'
#' The \code{spells.can.span.years} option specifies whether spells can cross
#' year boundaries -- i.e., span years. The default for this is the same as
#' fclimdex.
#'
#' @template wcsdi_common
#' @templateVar cdxvar wsdi
#' @templateVar cdxdescription an annual timeseries of the warm spell duration index.
#' @template get_generic_example
#'
#' @export
climdex.wsdi <-
  function(ci, spells.can.span.years = FALSE) {
    stopifnot(!is.null(ci@data$tmax) &&
                !is.null(ci@quantiles$tmax))
    return(
      threshold.exceedance.duration.index(
        ci@data$tmax,
        ci@date.factors$annual,
        ci@jdays,
        ci@quantiles$tmax$outbase$q90,
        ">",
        spells.can.span.years = spells.can.span.years,
        max.missing.days = ci@max.missing.days['annual']
      ) * ci@namasks$annual$tmax
    )
  }

#' @title Cold Spell Duration Index
#'
#' @description
#' This function computes the climdex index CSDI.
#'
#' @details
#' This function takes a climdexInput object as input and computes the climdex
#' index CSDI (Cold Spell Duration Index).
#'
#' The cold spell duration index is defined as the number of days
#' each year which are part of a "cold spell". A "cold spell" is defined as a
#' sequence of 6 or more days in which the daily minimum temperature is below
#' the 10th percentile of daily minimum temperature for a 5-day running window
#' surrounding this day during the baseline period.
#'
#' The \code{spells.can.span.years} option specifies whether spells can cross
#' year boundaries -- i.e., span years. The default for this is the same as
#' fclimdex.
#'
#' @template wcsdi_common
#' @templateVar cdxvar csdi
#' @templateVar cdxdescription an annual timeseries of the cold spell duration index.
#' @template get_generic_example
#'
#' @export
climdex.csdi <-
  function(ci, spells.can.span.years = FALSE) {
    stopifnot(!is.null(ci@data$tmin) &&
                !is.null(ci@quantiles$tmin))
    return(
      threshold.exceedance.duration.index(
        ci@data$tmin,
        ci@date.factors$annual,
        ci@jdays,
        ci@quantiles$tmin$outbase$q10,
        "<",
        spells.can.span.years = spells.can.span.years,
        max.missing.days = ci@max.missing.days['annual']
      ) * ci@namasks$annual$tmin
    )
  }

#' Mean Diurnal Temperature Range
#'
#' This function computes the diurnal temperature range on a monthly basis.
#'
#' \code{climdex.dtr} computes the mean daily diurnal temperature range. The
#' frequency of observation can be either monthly or annual.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the mean monthly or mean annual diurnal
#' temperature range.
#' @note This function creates results which may differ in the 3rd decimal
#' place from the results from fclimdex.
#' @template generic_seealso_references
#' @templateVar cdxvar dtr
#' @templateVar cdxdescription a monthly timeseries of mean diurnal temperature range.
#' @template get_generic_example
#'
#' @export
climdex.dtr <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$tmin) &&
                !is.null(ci@data$tmax) &&
                !is.null(ci@data$tavg))
    return(
      mean.daily.temp.range(ci@data$tmax, ci@data$tmin, ci@date.factors[[match.arg(freq)]]) * ci@namasks[[match.arg(freq)]]$tavg
    )
  }

#' Monthly Maximum 1-day Precipitation
#'
#' This function computes the climdex index Rx1day.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index Rx1day: monthly or annual maximum 1-day precipitation.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @template rx5day_common
#' @template generic_seealso_references
#' @templateVar cdxvar rx1day
#' @templateVar cdxdescription a timeseries of monthly maximum 1-day precipitation.
#' @template get_generic_example
#'
#' @export
climdex.rx1day <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$prec))
    return(nday.consec.prec.max(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1) * ci@namasks[[match.arg(freq)]]$prec)
  }

#' Monthly Maximum 5-day Consecutive Precipitation
#'
#' This function computes the climdex index Rx5day.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index Rx5day: monthly or annual maximum 5-day consecutive precipitation.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @param center.mean.on.last.day Whether to center the 5-day running mean on
#' the last day of the window, instead of the center day.
#' @template rx5day_common
#' @template generic_seealso_references
#' @templateVar cdxvar rx5day
#' @templateVar cdxdescription a timeseries of monthly maximum 5-day consecutive precipitation.
#' @template get_generic_example
#'
#' @export
climdex.rx5day <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal"),
           center.mean.on.last.day = FALSE) {
    stopifnot(!is.null(ci@data$prec))
    return(
      nday.consec.prec.max(ci@data$prec, ci@date.factors[[match.arg(freq)]], 5, center.mean.on.last.day) * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Simple Precpitation Intensity Index
#'
#' This function computes the climdex index SDII.
#'
#' \code{climdex.sdii} computes the climdex index SDII, or Simple Precipitation
#' Intensity Index. This is defined as the sum of precipitation in wet days
#' (days with preciptitation over 1mm) during the year divided by the number of
#' wet days in the year.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each year.
#' @note fclimdex rounds to 1 decimal place, whereas climdex.sdii does not.
#' This results in some small differences.
#' @template generic_seealso_references
#' @templateVar cdxvar sdii
#' @templateVar cdxdescription a timeseries of annual SDII values.
#' @template get_generic_example
#'
#' @export
climdex.sdii <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$prec))
    return(
      simple.precipitation.intensity.index(ci@data$prec, ci@date.factors[[match.arg(freq)]]) * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Precipitation Exceeding 10mm Per Day
#'
#' This function computes the climdex index R10mm.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index R10mm: the annual count of days where daily precipitation is more than 10mm per day.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar r10mm
#' @templateVar cdxdescription an annual timeseries of the R10mm index.
#' @template get_generic_example
#'
#' @export
climdex.r10mm <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$prec))
    return(
      number.days.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 10, ">=") * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Precipitation Exceeding 20mm Per Day
#'
#' This function computes the climdex index R20mm.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index R20mm: the annual count of days where daily precipitation is more than 20mm per day.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar r20mm
#' @templateVar cdxdescription an annual timeseries of the R20mm index.
#' @template get_generic_example
#'
#' @export
climdex.r20mm <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$prec))
    return(
      number.days.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 20, ">=") * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Precipitation Exceeding A Specified Amount Per Day
#'
#' This function computes the climdex index Rnnmm.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index Rnnmm: the annual count of days where daily precipitation is more than \code{nn} mm per day.
#'
#' @param ci Object of type climdexInput.
#' @param threshold The threshold to be used for Rnnmm.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar rnnmm
#' @templateVar cdxdescription an annual timeseries of the R1mm index.
#' @template get_generic_example
#'
#' @export
climdex.rnnmm <-
  function(ci,
           threshold = 1,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$prec))
    
    if (!is.numeric(threshold) ||
        length(threshold) != 1)
      stop("Please specify a single numeric threshold value.")
    
    
    return(
      number.days.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], threshold, ">=") * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Maximum Consecutive Dry Days
#'
#' This function computes the climdex index CDD.
#'
#' This function computes the climdex index CDD: the annual maximum length of dry spells, in days.
#' Dry spells are considered to be sequences of days where daily preciptation
#' is less than 1mm per day.
#'
#' @template cdd_common_freq
#' @templateVar cdxvar cdd
#' @templateVar cdxdescription an annual timeseries of the CDD index.
#' @template get_generic_example
#'
#' @export
climdex.cdd <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           spells.can.span.years = TRUE) {
    stopifnot(!is.null(ci@data$prec))
    return(
      spell.length.max(
        ci@data$prec,
        ci@date.factors[[match.arg(freq)]],
        1,
        "<",
        spells.can.span.years
      ) * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Maximum Consecutive Wet Days
#'
#' This function computes the climdex index CWD.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index CWD: the annual maximum length of wet spells, in days.
#' Wet spells are considered to be sequences of days where daily precipitation
#' is at least 1mm per day.
#'
#' @template cdd_common_freq
#' @templateVar cdxvar cdd
#' @templateVar cdxdescription an annual timeseries of the CWD index.
#' @template get_generic_example
#'
#' @export
climdex.cwd <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           spells.can.span.years = TRUE) {
    stopifnot(!is.null(ci@data$prec))
    return(
      spell.length.max(
        ci@data$prec,
        ci@date.factors[[match.arg(freq)]],
        1,
        ">=",
        spells.can.span.years
      ) * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Total Daily Precipitation Exceeding 75\%ile Threshold
#'
#' This function computes the climdex index R75pTOT.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index R75pTOT: the annual sum of precipitation in days where daily precipitation exceeds the
#' 75th percentile of daily precipitation in the base period.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing an annual timeseries of precipitation exceeding
#' the threshold.
#' @template generic_seealso_references
#' @templateVar cdxvar r75ptot
#' @templateVar cdxdescription an annual timeseries of the R75pTOT index.
#' @template get_generic_example
#'
#' @export
climdex.r75ptot <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$prec) &&
                !is.null(ci@quantiles$prec))
    return(
      total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], ci@quantiles$prec['q75'], ">") * ci@namasks[[match.arg(freq)]]$prec
    )
  }


#' Total Daily Precipitation Exceeding 95\%ile Threshold
#'
#' This function computes the climdex index R95pTOT.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index R95pTOT: the annual sum of precipitation in days where daily precipitation exceeds the
#' 95th percentile of daily precipitation in the base period.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing an annual timeseries of precipitation exceeding
#' the threshold.
#' @template generic_seealso_references
#' @templateVar cdxvar r95ptot
#' @templateVar cdxdescription an annual timeseries of the R95pTOT index.
#' @template get_generic_example
#'
#' @export
climdex.r95ptot <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$prec) &&
                !is.null(ci@quantiles$prec))
    return(
      total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], ci@quantiles$prec['q95'], ">") * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Total Daily Precipitation Exceeding 99\%ile Threshold
#'
#' This function computes the climdex index R99pTOT.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index R99pTOT: the annual sum of precipitation in days where daily precipitation exceeds the
#' 99th percentile of daily precipitation in the base period.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing an annual timeseries of precipitation exceeding
#' the threshold.
#' @template generic_seealso_references
#' @templateVar cdxvar r99ptot
#' @templateVar cdxdescription an annual timeseries of the R99pTOT index.
#' @template get_generic_example
#'
#' @export
climdex.r99ptot <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$prec) &&
                !is.null(ci@quantiles$prec))
    return(
      total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], ci@quantiles$prec['q99'], ">") * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Total Daily Precipitation
#'
#' This function computes the climdex index PRCPTOT.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index PRCPTOT: the annual sum of precipitation in wet days
#' (days where precipitation is at least 1mm).
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#' @templateVar cdxvar prcptot
#' @templateVar cdxdescription an annual timeseries of the sum of precipitation in wet days.
#' @template get_generic_example
#'
#' @export
climdex.prcptot <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$prec))
    return(
      total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1, ">=") * ci@namasks[[match.arg(freq)]]$prec
    )
  }

#' Standardized Precipitation Index
#' @description Adapted by ClimPACT2 and SPEI (arguments taken from here) libraries.
#' Given a timeseries of daily precipitation amounts [mm], the function calculates the Standardized Precipitation Index (SPI) as it is created and defined by the SPEI library.
#' This function computes the climdex index spi at multiple scales.
#'
#' @param ci Object of type climdexInput (representing the daily precipitation [mm])
#' @param freq Time frequency to aggregate to. Allowed only monthly
#' @param scale an integer, representing the time scale at which the SPI will be computed. Default is 3
#' @param distribution name of the distribution function to be used for computing the SPI (default 'Gamma')
#' @param fit name of the method used for computing the distribution function parameters (default 'ub-pwm')
#' @param kernel optional, a list defining the type of kernel used for computing the SPI at scales higher than one. Defaults to unshifted rectangular kernel.
#' @param ref.start optional, starting point of the reference period used for computing the index. Defaults to NULL, indicating that the first value in data will be used as starting point.
#' @param ref.end optional, ending  point  of  the  reference  period  used  for  computing  the index. Defaults to NULL, indicating that the last value in data will be used as ending point.
#' @param ... For more details please refer to the SPEI
#' @return A vector containing a monthly SPI at the selected scale
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/documents/atbd.pdf}
#' @references \url{https://cran.r-project.org/web/packages/SPEI/SPEI.pdf}
#' @importFrom SPEI spi
#'
#' @export
climdex.spi <-
  function(ci,
           freq = c("monthly"),
           scale = 3,
           distribution = "Gamma",
           fit = "ub-pwm",
           kernel = list(type = "rectangular", shift = 1),
           ref.start = NULL,
           ref.end = NULL) {
    spiprec <- ci@data$prec
    spifactor <- ci@date.factors$monthly
    prec_sum <-
      as.numeric(tapply.fast(spiprec, spifactor, sum, na.rm = TRUE))
    
    ts.start <- c(as.numeric(format(ci@dates[1], format = "%Y")), 1)
    ts.end <-
      c(as.numeric(format(ci@dates[length(ci@dates)], format = "%Y")), 12)
    
    data.spi <- ts(prec_sum,
                   freq = 12,
                   start = ts.start,
                   end = ts.end)
    
    spi_col <-
      spi(
        data.spi,
        scale = scale,
        ref.start = ref.start,
        ref.end = ref.end,
        distribution = distribution,
        fit = fit,
        kernel = kernel,
        na.rm = TRUE
      )
    
    tmpvar <- (spi_col$fitted)
    tmpvar <- ifelse(tmpvar == "-Inf", NA, tmpvar)
    tmpvar <- ifelse(tmpvar == "Inf", NA, tmpvar)
    
    tmpvar <- ifelse(tmpvar == "NaNf", NA, tmpvar)
    tmpvar <- ifelse(tmpvar == "NaN", NA, tmpvar)
    
    x <- as.numeric(tmpvar)
    names(x) <- unique(spifactor)
    return(x)
  }


#' Consecutive Summer Days
#' @description
#' This function takes a climdexInput object as input and computes the climdex
#' index csu: the annual (or at different periods) count of consecutive summer days (TX >25C)
#'
#' @param ci Object of type climdexInput. Here the daily maximum temperature.
#' @param freq Time frequency to aggregate to. Allowed only monthly, annual, halfyear, seasonal.
#' @param spells.can.span.years Default FALSE
#' @return A vector containing a timeseries of the number of consecutive summer days in a given period (freq).
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/documents/atbd.pdf}
#'
#' @export
climdex.csu <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal"),
           spells.can.span.years = FALSE) {
    stopifnot(!is.null(ci@data$tmax))
    
    return(
      spell.length.max(
        ci@data$tmax,
        ci@date.factors[[match.arg(freq)]],
        25,
        ">",
        spells.can.span.years
      ) * ci@namasks[[match.arg(freq)]]$tmax
    )
  }


#' Consecutive Frost Days
#' @description
#' This function takes a climdexInput object as input and computes the climdex
#' index cfd: the annual (or at different periods) count of consecutive frost days (TN < 0C)
#'
#' @param ci Object of type climdexInput. Here the daily maximum temperature.
#' @param freq Time frequency to aggregate to. Allowed only monthly, annual, halfyear, seasonal.
#' @param spells.can.span.years Default FALSE
#' @return A vector containing a timeseries of the number of consecutive summer days in a given period (freq).
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
climdex.cfd <-
  function(ci,
           freq = c("monthly", "annual", "halfyear", "seasonal"),
           spells.can.span.years = FALSE) {
    stopifnot(!is.null(ci@data$tmin))
    
    return(
      spell.length.max(
        ci@data$tmin,
        ci@date.factors[[match.arg(freq)]],
        0,
        "<",
        spells.can.span.years
      ) * ci@namasks[[match.arg(freq)]]$tmin
    )
  }



#' Heating Degree Days
#' @description
#' This function takes a climdexInput object as input and computes the climdex
#' index hd17: the annual (or at different periods) sum of heating degree days (17-tavg)
#' @param ci Object of type climdexInput. Here the daily maximum temperature.
#' @param freq Time frequency to aggregate to. Allowed only monthly, annual, halfyear, seasonal.
#' @param spells.can.span.years Default FALSE
#' @return A vector containing a timeseries of the number of consecutive summer days in a given period (freq).
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
climdex.hd17 <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly")) {
    stopifnot(!is.null(ci@data$tavg))
    
    return(tapply((17 -  ci@data$tavg), ci@date.factors[[match.arg(freq)]], sum) * ci@namasks[[match.arg(freq)]]$tavg)
  }

#' Lengths of strings of TRUE (1 & 0) values
## -- introduced by C. Photiadou (KNMI), September 2015
#' Computes which days are above or below the baseline threshold.
#'
#' This function computes which days are above or below baseline thresholds.
#' It is used to implement the compound indices.
#' It is based on the "percent.days.op.threshold"
#'
#' @param temp Sequence of temperature values.
#' @param dates Sequence of associated dates.
#' @param jdays Sequence of associated days of year.
#' @param date.factor Factor to aggregate data using.
#' @param threshold.outside.base Sequence of thresholds to be used for data outside the base period.
#' @param base.thresholds Data structure containing sets of thresholds to be used inside the base period; see \link{climdexInput-class}.
#' @param base.range Date range (type PCICt) of the baseline period.
#' @param op Comparison operator to use.
#' @param max.missing.days Maximum number of NA values per time period.
#' @return A vector consisting of the mean fraction of days above or below the supplied set of thresholds.
#' @note If date.factor is omitted, daily series will be returned.
#' @seealso \link{climdexInput-class}.
#' @keywords ts climate
days.op.threshold <-
  function(temp,
           dates,
           jdays,
           date.factor,
           threshold.outside.base,
           base.thresholds,
           base.range,
           op = '<') {
    f <- match.fun(op)
    dat <- f(temp, threshold.outside.base[jdays])
    
    inset <- dates >= base.range[1] & dates <= base.range[2]
    ## Don't use in-base thresholds with data shorter than two years; no years to replace with.
    if (sum(inset) > 0 && length(dates) >= 360 * 2) {
      jdays.base <- jdays[inset]
      years.base <- climdex.pcic:::get.years(dates[inset])
      
      ## Get number of base years, subset temp data to base period only.
      temp.base <- temp[inset]
      years.base.range <- range(years.base)
      byrs <- (years.base.range[2] - years.base.range[1] + 1)
      
      ## Linearize thresholds, then compare them to the temperatures
      bdim <- dim(base.thresholds)
      dim(base.thresholds) <- c(bdim[1] * bdim[2], bdim[3])
      yday.byr.indices <-
        jdays.base + (years.base - climdex.pcic:::get.years(base.range)[1]) * bdim[1]
      f.result <-
        f(rep(temp.base, byrs - 1), base.thresholds[yday.byr.indices,])
      dim(f.result) <- c(length(yday.byr.indices), bdim[3])
      
      ## Chop up data along the 2nd dim into a list; sum elements of the list
      dat[inset] <- rowSums(f.result, na.rm = TRUE) / (byrs - 1)
    }
    return(dat)
  }

## Climate Compound Indices
## -- introduced by C. Photiadou (KNMI), September 2015
#' Compound Indices
#'
#' Cold-dry days
#'
#' This function computes the climdex index CD.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index CD: the number of days where TG<25 & RR<25 (for wet days: days where precipitation is at least 1mm).
#'
#' @param ci Object of type climdexInput (representing the daily precipitation [mm] and the averaged daily temperature [C])
#' @param freq Time frequency to aggregate to.
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#'
#' @export
climdex.cd <-
  function(ci,
           freq = c("annual", "monthly", "halfyear", "seasonal"),
           precip.thresh = "q25",
           precip.op = "<",
           temp.thresh = "q25",
           temp.op = "<") {
    stopifnot(
      !is.null(ci@data$prec) &&
        !is.null(ci@quantiles$prec) &&
        !is.null(ci@data$tavg) && !is.null(ci@quantiles$tavg)
    )
    daily.prec <- ci@data$prec
    daily.temp <- ci@data$tavg
    q.precip <- ci@quantiles$prec[[precip.thresh]]
    f.prec <- match.fun(precip.op)
    f.temp <-
      days.op.threshold(
        daily.temp,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tavg$outbase[[temp.thresh]],
        ci@quantiles$tavg$inbase[[temp.thresh]],
        ci@base.range,
        temp.op
      )
    #Convert from 1/0 to TRUE/FALSE
    logic.f.temp <- f.temp == 1
    df.for.data <-
      data.frame(daily.prec, logic.f.temp, date.factor = ci@date.factors[[match.arg(freq)]])
    result <-
      lapply(split(df.for.data, df.for.data$date.factor), function(chunk) {
        sum(f.prec(chunk$daily.prec, q.precip) &
              chunk$logic.f.temp,
            na.rm = FALSE)
      })
    return(unlist(result))
  }

#' Cold-wet days
#'
#' This function computes the climdex index CW.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index CW: the number of days where TG<25 & RR>75 (for wet days: days where precipitation is at least 1mm).
#'
#' @param ci Object of type climdexInput (representing the daily precipitation [mm] and the averaged daily temperature [C])
#' @param freq Time frequency to aggregate to.
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#'
#' @export
climdex.cw <-
  function(ci,
           freq = c("annual", "monthly", "halfyear", "seasonal"),
           precip.thresh = "q75",
           precip.op = ">",
           temp.thresh = "q25",
           temp.op = "<") {
    stopifnot(
      !is.null(ci@data$prec) &&
        !is.null(ci@quantiles$prec) &&
        !is.null(ci@data$tavg) && !is.null(ci@quantiles$tavg)
    )
    daily.prec <- ci@data$prec
    daily.temp <- ci@data$tavg
    q.precip <- ci@quantiles$prec[[precip.thresh]]
    f.prec <- match.fun(precip.op)
    f.temp <-
      days.op.threshold(
        daily.temp,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tavg$outbase[[temp.thresh]],
        ci@quantiles$tavg$inbase[[temp.thresh]],
        ci@base.range,
        temp.op
      )
    #Convert from 1/0 to TRUE/FALSE
    logic.f.temp <- f.temp == 1
    df.for.data <-
      data.frame(daily.prec, logic.f.temp, date.factor = ci@date.factors[[match.arg(freq)]])
    result <-
      lapply(split(df.for.data, df.for.data$date.factor), function(chunk) {
        sum(f.prec(chunk$daily.prec, q.precip) &
              chunk$logic.f.temp,
            na.rm = FALSE)
      })
    return(unlist(result))
  }

#' Warm-dry days
#'
#' This function computes the climdex index WD.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index WD: the number of days where TG>75 & RR<25 (for wet days: days where precipitation is at least 1mm).
#'
#' @param ci Object of type climdexInput (representing the daily precipitation [mm] and the averaged daily temperature [C])
#' @param freq Time frequency to aggregate to.
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#'
#' @export
climdex.wd <-
  function(ci,
           freq = c("annual", "monthly", "halfyear", "seasonal"),
           precip.thresh = "q25",
           precip.op = "<",
           temp.thresh = "q75",
           temp.op = ">") {
    stopifnot(
      !is.null(ci@data$prec) &&
        !is.null(ci@quantiles$prec) &&
        !is.null(ci@data$tavg) && !is.null(ci@quantiles$tavg)
    )
    daily.prec <- ci@data$prec
    daily.temp <- ci@data$tavg
    q.precip <- ci@quantiles$prec[[precip.thresh]]
    f.prec <- match.fun(precip.op)
    f.temp <-
      days.op.threshold(
        daily.temp,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tavg$outbase[[temp.thresh]],
        ci@quantiles$tavg$inbase[[temp.thresh]],
        ci@base.range,
        temp.op
      )
    #Convert from 1/0 to TRUE/FALSE
    logic.f.temp <- f.temp == 1
    df.for.data <-
      data.frame(daily.prec, logic.f.temp, date.factor = ci@date.factors[[match.arg(freq)]])
    result <-
      lapply(split(df.for.data, df.for.data$date.factor), function(chunk) {
        sum(f.prec(chunk$daily.prec, q.precip) &
              chunk$logic.f.temp,
            na.rm = FALSE)
      })
    return(unlist(result))
  }

#' Warm-wet days
#'
#' This function computes the climdex index WW
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index WW: the number of days where TG>75 & RR>75 (for wet days: days where precipitation is at least 1mm).
#'
#' @param ci Object of type climdexInput (representing the daily precipitation [mm] and the averaged daily temperature [C])
#' @param freq Time frequency to aggregate to.
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#'
#' @export
climdex.ww <-
  function(ci,
           freq = c("annual", "monthly", "halfyear", "seasonal"),
           precip.thresh = "q75",
           precip.op = ">",
           temp.thresh = "q75",
           temp.op = ">") {
    stopifnot(
      !is.null(ci@data$prec) &&
        !is.null(ci@quantiles$prec) &&
        !is.null(ci@data$tavg) && !is.null(ci@quantiles$tavg)
    )
    daily.prec <- ci@data$prec
    daily.temp <- ci@data$tavg
    q.precip <- ci@quantiles$prec[[precip.thresh]]
    f.prec <- match.fun(precip.op)
    f.temp <-
      days.op.threshold(
        daily.temp,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tavg$outbase[[temp.thresh]],
        ci@quantiles$tavg$inbase[[temp.thresh]],
        ci@base.range,
        temp.op
      )
    #Convert from 1/0 to TRUE/FALSE
    logic.f.temp <- f.temp == 1
    df.for.data <-
      data.frame(daily.prec, logic.f.temp, date.factor = ci@date.factors[[match.arg(freq)]])
    result <-
      lapply(split(df.for.data, df.for.data$date.factor), function(chunk) {
        sum(f.prec(chunk$daily.prec, q.precip) &
              chunk$logic.f.temp,
            na.rm = FALSE)
      })
    return(unlist(result))
  }

## Climate indices involving wind timeseries
## -- introduced by R. Posselt (MeteoSwiss), July 2015

#' Mean wind
#' @description
#' This function computes the climdex index FG: the mean wind speed measured within a period.
#'
#' @param ci Object of type climdexInput (representing the daily mean wind speed in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of mean wind speed.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/documents/atbd.pdf}
#'
#' @export
climdex.fg <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$wind))
    return(tapply.fast(ci@data$wind, ci@date.factors[[match.arg(freq)]], mean, na.rm =
                         TRUE) * ci@namasks[[match.arg(freq)]]$wind)
  }

#' Calm days
#' @description
#' This function computes the climdex index FGcalm days: The number of days with mean wind lower than or equal 2 m/s.
#'
#' @param ci Object of type climdexInput (representing the daily mean wind speed in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the number of calm days.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.fgcalm <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$wind))
    return(
      number.days.op.threshold(
        temp = ci@data$wind,
        date.factor = ci@date.factors[[match.arg(freq)]],
        threshold = 2,
        op = "<="
      ) * ci@namasks[[match.arg(freq)]]$wind
    )
  }

#' Windy days
#' @description
#' This function computes the climdex index FG6bft: The number of days with mean wind greater than or equal 10.8 m/s (~6 Bft).
#'
#' @param ci Object of type climdexInput (representing the daily mean wind speed in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the number of calm days.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.fg6bft <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$wind))
    return(
      number.days.op.threshold(
        temp = ci@data$wind,
        date.factor = ci@date.factors[[match.arg(freq)]],
        threshold = 10.8,
        op = ">="
      ) * ci@namasks[[match.arg(freq)]]$wind
    )
  }

#' Storm days
#'
#' This function computes the climdex index FXstorm: The number of days with wind gusts greater than or equal 20.8 m/s (75 km/h).
#'
#' @param ci Object of type climdexInput (representing the daily maximum wind gust in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the number of storm days.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.fxstorm <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$wind_gust))
    return(
      number.days.op.threshold(
        temp = ci@data$wind_gust,
        date.factor = ci@date.factors[[match.arg(freq)]],
        threshold = 20.8,
        op = ">="
      ) * ci@namasks[[match.arg(freq)]]$wind_gust
    )
  }

#' Maximum wind gust
#'
#' This function computes the climdex index FXx: The maximum of the maximum daily wind gust.
#'
#' @param ci Object of type climdexInput (representing the daily maximum wind gust in [m/s])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the maximum of the maximum daily wind gust.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.fxx <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$wind_gust))
    return(
      tapply.fast(ci@data$wind_gust, ci@date.factors[[match.arg(freq)]], max, na.rm =
                    TRUE) *
        ci@namasks[[match.arg(freq)]]$wind_gust
    )
  }

#' Northerly winds
#' @description
#' This function computes the climdex index DDnorth: Days with northerly wind (-45deg (315deg) < wind_dir <= 45deg).
#'
#' @param ci Object of type climdexInput (representing the daily mean wind direction in [deg] with 0deg being wind from north)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of days with northerly winds.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.ddnorth <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$wind_dir))
    wind_northerly <-
      (
        number.days.op.threshold(
          temp = ci@data$wind_dir,
          date.factor = ci@date.factors[[match.arg(freq)]],
          threshold = 315,
          op = ">"
        ) +
          number.days.op.threshold(
            temp = ci@data$wind_dir,
            date.factor = ci@date.factors[[match.arg(freq)]],
            threshold = 45,
            op = "<="
          )
      ) * ci@namasks[[match.arg(freq)]]$wind_dir
    return(wind_northerly)
  }

#' Easterly winds
#' @description
#' This function computes the climdex index DDeast: Days with easterly wind (45deg < wind_dir <= 135deg).
#'
#' @param ci Object of type climdexInput (representing the daily mean wind direction in [deg] with 90deg being wind from east)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of days with easterly winds.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.ddeast <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$wind_dir))
    wind_easterly <-
      (
        number.days.op.threshold(
          temp = ci@data$wind_dir,
          date.factor = ci@date.factors[[match.arg(freq)]],
          threshold = 45,
          op = ">"
        ) -
          number.days.op.threshold(
            temp = ci@data$wind_dir,
            date.factor = ci@date.factors[[match.arg(freq)]],
            threshold = 135,
            op = ">"
          )
      ) * ci@namasks[[match.arg(freq)]]$wind_dir
    return(wind_easterly)
  }

#' Southerly winds
#' @description
#' This function computes the climdex index DDsouth: Days with southerly wind (135deg < wind_dir <= 225deg).
#'
#' @param ci Object of type climdexInput (representing the daily mean wind direction in [deg] with 180deg being wind from south)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of days with southerly winds.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.ddsouth <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$wind_dir))
    wind_southerly <-
      (
        number.days.op.threshold(
          temp = ci@data$wind_dir,
          date.factor = ci@date.factors[[match.arg(freq)]],
          threshold = 135,
          op = ">"
        ) -
          number.days.op.threshold(
            temp = ci@data$wind_dir,
            date.factor = ci@date.factors[[match.arg(freq)]],
            threshold = 225,
            op = ">"
          )
      ) * ci@namasks[[match.arg(freq)]]$wind_dir
    return(wind_southerly)
  }

#' Westerly winds
#' @description
#' This function computes the climdex index DDwest: Days with westerly wind (225deg < wind_dir <= 315deg).
#'
#' @param ci Object of type climdexInput (representing the daily mean wind direction in [deg] with 270deg being wind from west)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of days with westerly winds.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.ddwest <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$wind_dir))
    wind_westerly <-
      (
        number.days.op.threshold(
          temp = ci@data$wind_dir,
          date.factor = ci@date.factors[[match.arg(freq)]],
          threshold = 225,
          op = ">"
        ) -
          number.days.op.threshold(
            temp = ci@data$wind_dir,
            date.factor = ci@date.factors[[match.arg(freq)]],
            threshold = 315,
            op = ">"
          )
      ) * ci@namasks[[match.arg(freq)]]$wind_dir
    return(wind_westerly)
  }


## Climate indices involving temperature timeseries
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
#' @author Rebekka Posselt (MeteoSwiss)
#'
#' @name climdex.tnday
NULL

#'
#' @rdname climdex.tnday
#' @export
#'
climdex.txndaymax <-
  function(ci,
           ndays = 5,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           center.mean.on.last.day = FALSE) {
    stopifnot(!is.null(ci@data$tmax))
    return(
      nday.consec.temp.mean(
        ci@data$tmax,
        ci@date.factors[[match.arg(freq)]],
        ndays = ndays,
        freq.fun = "max",
        center.mean.on.last.day
      ) *
        ci@namasks[[match.arg(freq)]]$tmax
    )
  }

#'
#' @rdname climdex.tnday
#' @export
#'
climdex.txndaymin <-
  function(ci,
           ndays = 5,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           center.mean.on.last.day = FALSE) {
    stopifnot(!is.null(ci@data$tmax))
    return(
      nday.consec.temp.mean(
        ci@data$tmax,
        ci@date.factors[[match.arg(freq)]],
        ndays = ndays,
        freq.fun = "min",
        center.mean.on.last.day
      ) *
        ci@namasks[[match.arg(freq)]]$tmax
    )
  }

#'
#' @rdname climdex.tnday
#' @export
#'
climdex.tnndaymax <-
  function(ci,
           ndays = 5,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           center.mean.on.last.day = FALSE) {
    stopifnot(!is.null(ci@data$tmin))
    return(
      nday.consec.temp.mean(
        ci@data$tmin,
        ci@date.factors[[match.arg(freq)]],
        ndays = ndays,
        freq.fun = "max",
        center.mean.on.last.day
      ) *
        ci@namasks[[match.arg(freq)]]$tmin
    )
  }

#'
#' @rdname climdex.tnday
#' @export
#'
climdex.tnndaymin <-
  function(ci,
           ndays = 5,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           center.mean.on.last.day = FALSE) {
    stopifnot(!is.null(ci@data$tmin))
    return(
      nday.consec.temp.mean(
        ci@data$tmin,
        ci@date.factors[[match.arg(freq)]],
        ndays = ndays,
        freq.fun = "min",
        center.mean.on.last.day
      ) *
        ci@namasks[[match.arg(freq)]]$tmin
    )
  }

#'
#' @rdname climdex.tnday
#' @export
#'
climdex.tmndaymax <-
  function(ci,
           ndays = 5,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           center.mean.on.last.day = FALSE) {
    stopifnot(!is.null(ci@data$tavg))
    return(
      nday.consec.temp.mean(
        ci@data$tavg,
        ci@date.factors[[match.arg(freq)]],
        ndays = ndays,
        freq.fun = "max",
        center.mean.on.last.day
      ) *
        ci@namasks[[match.arg(freq)]]$tavg
    )
  }

#'
#' @rdname climdex.tnday
#' @export
#'
climdex.tmndaymin <-
  function(ci,
           ndays = 5,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           center.mean.on.last.day = FALSE) {
    stopifnot(!is.null(ci@data$tavg))
    return(
      nday.consec.temp.mean(
        ci@data$tavg,
        ci@date.factors[[match.arg(freq)]],
        ndays = ndays,
        freq.fun = "min",
        center.mean.on.last.day
      ) *
        ci@namasks[[match.arg(freq)]]$tavg
    )
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
climdex.txnp <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           quant = 0.9,
           op = ">") {
    quant.str <- paste0("q", as.integer(quant * 100))
    stopifnot(
      !is.null(ci@data$tmax) &&
        !is.null(ci@quantiles$tmax) &&
        !is.null(ci@quantiles$tmax$outbase[[quant.str]])
    )
    return(
      percent.days.op.threshold(
        ci@data$tmax,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tmax$outbase[[quant.str]],
        ci@quantiles$tmax$inbase[[quant.str]],
        ci@base.range,
        op,
        ci@max.missing.days[match.arg(freq)]
      ) * ci@namasks[[match.arg(freq)]]$tmax
    )
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
climdex.tnnp <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal","monthly"),
           quant = 0.9,
           op = ">") {
    quant.str <- paste0("q", as.integer(quant * 100))
    stopifnot(
      !is.null(ci@data$tmin) &&
        !is.null(ci@quantiles$tmin) &&
        !is.null(ci@quantiles$tmin$outbase[[quant.str]])
    )
    return(
      percent.days.op.threshold(
        ci@data$tmin,
        ci@dates,
        ci@jdays,
        ci@date.factors[[match.arg(freq)]],
        ci@quantiles$tmin$outbase[[quant.str]],
        ci@quantiles$tmin$inbase[[quant.str]],
        ci@base.range,
        op,
        ci@max.missing.days[match.arg(freq)]
      ) * ci@namasks[[match.arg(freq)]]$tmin
    )
  }

## Climate indices involving sun timeseries
#' Sunshine duration
#'
#' This function computes the climdex index SS: The sum of the sunshine duration hours within a period.
#'
#' @param ci Object of type climdexInput (representing the daily sunshine duration in [h])
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of sunshine duration.
#'
#' @template generic_seealso_references
#' @author Rebekka Posselt (MeteoSwiss)
#'
#' @export
climdex.ss <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$sun))
    return(tapply.fast(ci@data$sun, ci@date.factors[[match.arg(freq)]], sum, na.rm =
                         TRUE) * ci@namasks[[match.arg(freq)]]$sun)
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
climdex.sun_relmean <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$sun_rel))
    return(
      tapply.fast(ci@data$sun_rel, ci@date.factors[[match.arg(freq)]], mean, na.rm =
                    TRUE) * ci@namasks[[match.arg(freq)]]$sun_rel
    )
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
climdex.sun_cloudy <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$sun_rel))
    return(
      number.days.op.threshold(
        temp = ci@data$sun_rel,
        date.factor = ci@date.factors[[match.arg(freq)]],
        threshold = 20,
        op = "<"
      ) * ci@namasks[[match.arg(freq)]]$sun_rel
    )
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
climdex.sun_sunny <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$sun))
    return(
      number.days.op.threshold(
        temp = ci@data$sun_rel,
        date.factor = ci@date.factors[[match.arg(freq)]],
        threshold = 80,
        op = "<"
      ) * ci@namasks[[match.arg(freq)]]$sun
    )
  }

## Climate indices involving snow timeseries
#' Number of snow days
#'
#' This function computes the climdex index SDD: The number of days with a snow depth > Y cm.
#'
#' @param ci Object of type climdexInput (representing the daily snow depth timeseries in [cm]).
#' @param threshold Snow depth threshhold [in cm]. (Default: 1)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing an annual timeseries of the number of snow days.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.sdd <-
  function(ci,
           threshold = 1,
           freq = c("annual", "monthly", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$snow))
    return(
      number.days.op.threshold(
        temp = ci@data$snow,
        date.factor = ci@date.factors[[match.arg(freq)]],
        threshold = threshold,
        op = ">="
      ) * ci@namasks[[match.arg(freq)]]$snow
    )
  }

#' Maximum snow depth
#'
#' This function computes the climdex index SDx: The maximum snow depth measured within a year.
#'
#' @param ci Object of type climdexInput (representing the daily snow depth timeseries in [cm]).
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing an annual timeseries of the maximum snow depth.
#' @author Rebekka Posselt (MeteoSwiss)
#'
#' @export
#'
climdex.sdx <-
  function(ci,
           freq = c("annual", "monthly", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$snow))
    return(tapply.fast(ci@data$snow, ci@date.factors[[match.arg(freq)]], max, na.rm =
                         TRUE) *
             ci@namasks[[match.arg(freq)]]$snow)
  }

#' Mean snow depth
#'
#' This function computes the climdex index SD: The mean snow depth measured within a period.
#'
#' @param ci Object of type climdexInput (representing the daily snow depth timeseries in [cm]).
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing the timeseries of the mean snow depth.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
#'
climdex.sd <-
  function(ci,
           freq = c("annual", "monthly", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$snow))
    return(tapply.fast(ci@data$snow, ci@date.factors[[match.arg(freq)]], mean, na.rm =
                         TRUE) *
             ci@namasks[[match.arg(freq)]]$snow)
  }

#' Number of new snow days
#'
#' This function computes the climdex index NSD: The number of days with a (new) snowfall > Y cm.
#'
#' @param ci Object of type climdexInput (representing the daily snowfall timeseries in [cm]).
#' @param threshold Snow depth threshhold [in cm]. (Default: 1)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing the timeseries of the number of new snow days.
#' @author Rebekka Posselt (MeteoSwiss)
#'
#' @export
#'
climdex.nsd <-
  function(ci,
           threshold = 1,
           freq = c("annual", "monthly", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$snow_new))
    return(
      number.days.op.threshold(
        temp = ci@data$snow_new,
        date.factor = ci@date.factors[[match.arg(freq)]],
        threshold = threshold,
        op = ">="
      ) * ci@namasks[[match.arg(freq)]]$snow_new
    )
  }

#' Maximum new snow
#'
#' This function computes the climdex index NSX: The maximum snowfall measured within a year.
#'
#' @param ci Object of type climdexInput (representing the daily snowfall timeseries in [cm]).
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing the timeseries of the maximum snowfall.
#' @author Rebekka Posselt (MeteoSwiss)
#'
#' @export
#'
climdex.nsx <-
  function(ci,
           freq = c("annual", "monthly", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$snow_new))
    return(
      tapply.fast(ci@data$snow_new, ci@date.factors[[match.arg(freq)]], max, na.rm =
                    TRUE) *
        ci@namasks[[match.arg(freq)]]$snow_new
    )
  }

#' New snow sum
#'
#' This function computes the climdex index NSS: The sum of all snowfall measured within a year.
#'
#' @param ci Object of type climdexInput (representing the daily snowfall timeseries in [cm]).
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".
#' @return A vector containing the timeseries of the snowfall sum.
#' @author Rebekka Posselt (MeteoSwiss)
#'
#' @export
#'
climdex.nss <-
  function(ci,
           freq = c("annual", "monthly", "halfyear", "seasonal")) {
    stopifnot(!is.null(ci@data$snow_new))
    return(
      tapply.fast(ci@data$snow_new, ci@date.factors[[match.arg(freq)]], sum, na.rm =
                    TRUE) *
        ci@namasks[[match.arg(freq)]]$snow_new
    )
  }

## Climate indices involving cloud timeseries
#' Mean cloud cover
#'
#' This function computes the climdex index CC
#'
#' This function takes a climdexInput object as input and computes the
#' CLOUD_MEAN index: The mean cloud cover measured within a period.
#'
#' @param ci Object of type climdexInput (representing the daily mean cloud cover in octa or percent)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @return A vector containing the time series of mean cloud cover.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
climdex.cc <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly")) {
    stopifnot(!is.null(ci@data$cloud))
    return(
      tapply.fast(ci@data$cloud, ci@date.factors[[match.arg(freq)]], mean, na.rm =
                    TRUE) * ci@namasks[[match.arg(freq)]]$cloud
    )
  }

#' Mostly cloudy days
#'
#' This function computes the climdex index CC6: The number of mostly cloudy days (Cloud cover >= 6octa / 80%) within a period.
#'
#' @param ci Object of type climdexInput (representing the daily mean cloud cover in octa or percent)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @param unit unit of the cloud cover. Allowed are: "octa" or "percent". Default: "octa".
#' @return A vector containing the number of mostly cloudy days.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
climdex.cc6 <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly"),
           unit = "octa") {
    stopifnot(!is.null(ci@data$cloud))
    if (unit == "octa") {
      threshold = 6
    } else if (unit == "percent") {
      threshold = 80
    } else{
      stop("unit should be either 'octa' or 'percent'")
    }
    return(
      number.days.op.threshold(
        temp = ci@data$cloud,
        date.factor = ci@date.factors[[match.arg(freq)]],
        threshold = threshold,
        op = ">="
      ) * ci@namasks[[match.arg(freq)]]$cloud
    )
  }

#' Mostly sunny days
#'
#' This function computes the climdex index CC2: The number of mostly sunny days (Cloud cover <= 2octa / 20%) within a period.
#'
#' @param ci Object of type climdexInput (representing the daily mean cloud cover in octa or percent)
#' @param freq Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".
#' @param unit unit of the cloud cover. Allowed are: "octa" or "percent". Default: "octa".
#' @return A vector containing the number of mostly sunny days.
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
climdex.cc2 <-
  function(ci,
           freq = c("annual", "halfyear", "seasonal", "monthly"),
           unit = "octa") {
    stopifnot(!is.null(ci@data$cloud))
    if (unit == "octa") {
      threshold = 2
    } else if (unit == "percent") {
      threshold = 20
    } else{
      stop("unit should be either 'octa' or 'percent'")
    }
    return(
      number.days.op.threshold(
        temp = ci@data$cloud,
        date.factor = ci@date.factors[[match.arg(freq)]],
        threshold = threshold,
        op = "<="
      ) * ci@namasks[[match.arg(freq)]]$cloud
    )
  }

# ######################################################################################################################
# #' HUGLIN INDEX (only for climdex.pcic.ncdf)
#' Introduces by C.Photiadou (KNMI)
# #' This function is not ready yet. It is uses coefficient based on the latitude
# #' For this index I had to curry the cdx.funcs to be able to include the subset. Later I realised
# #' I need also to include the latitude. This would be used in compute.indices.for.stripe together with get.lat
# #' to retrieve subset & latitude
# #' I didn't proceed with finisheing the eca.HI function. I thought to ask you first if its possible
# #' to adapt compute.indices.for.stripe so it can include the currying and the latitude. Or if you had a better idea on this
# #' please let me know.
#
# #' Function to Curry a cxd.funcs for subset (now at cur_sub)
# #' used only for Huglin Index
curry_in_subset_for_huglin <- function(cdx.funcs, cur_sub) {
  cdx.names = names(cdx.funcs)
  cdx.funcs <- lapply(cdx.names, function(function_name) {
    f = cdx.funcs[[function_name]]
    if (grepl('^hi', function_name)) {
      return(functional::Curry(f, cur_sub = cur_sub))
    } else {
      return(f)
    }
  })
  names(cdx.funcs) = cdx.names
  return(cdx.funcs)
}

### Get latitude function
get.lat <- function(open_file_list, variable.name.map) {
  #var.name <- variable.name.map[[names(v.f.idx)[1]]]
  y.dim <-
    ncdf4.helpers::nc.get.dim.for.axis(open_file_list[[1]], variable.name.map, "Y")
  return(y.dim$vals)
}

#' Huglin Index
#'
#' This function computes the climdex index HI:
#' @param ci Object of type climdexInput (representing the daily mean and daily max temperature)
#' @param freq Time frequency to aggregate to. Allowed are only "annual"
#' @param unit. Allowed are only deg C.
#' @param cur_sub TO DO!
#' @return A vector containing the HI
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#'
#' @export
climdex.HI <- function(ci, freq = c("annual"), cur_sub) {
  tempavg <- ci@data$tavg
  tempmax <- ci@data$tmax
  
  month.series <- get.months(ci@dates)
  year.series <- get.years(ci@dates)
  valid.months <- month.series >= 4 & month.series <= 9
  hi_coef <-  if (cur_sub <= 40) {
    hi_coeff <- 1
  } else if (cur_sub > 40 & cur_sub < 42) {
    hi_coef <- 1.02
  } else if (cur_sub > 42 & cur_sub < 44) {
    hi_coef <- 1.03
  } else if (cur_sub > 44 & cur_sub < 48) {
    hi_coef <- 1.04
  } else if (cur_sub > 46 & cur_sub < 48) {
    hi_coef <- 1.05
  } else if (cur_sub > 48 & cur_sub < 50) {
    hi_coef <- 1.06
  } else if (cur_sub >= 50) {
    hi_coef <- 1
  }
  valid.sel <- year.series[valid.months]
  tempdata <- ((((tempavg - 10) + (tempmax - 10)) / 2) * hi_coef)
  dat_final <- tempdata[valid.months]
  
  return(tapply(dat_final, valid.sel, sum))
}
####################################################################33
## Functions for eca\&d station files
## -- introduced by C. Photiadou (KNMI), November 2015
# Secondary function for eca.input
#
# Typical ECA\&D files contain a header and this functions finds the line number where the data starts.
#
find.start.of.data.index = function(fname) {
  matched.indices = which(grepl('^SOUID', gsub(" ", "", readLines(fname, n = 50))))
  if (length(matched.indices) > 1)
    stop('ECA fileformat error: cannot determine start of data, multiple header lines')
  if (length(matched.indices) == 0)
    stop('ECA fileformat error: cannot find start of data: cannot find header line')
  return(matched.indices - 1)
}

## Functions for eca\&d station files
#' ECA &D station data files
#'
#' This function reads and prepares the station data files from the European Climate Assessment and Dataset (ECA&D) for use in climdex
#'
#' @param filename File name and path of station data file.
#' @param var.name A varialbe name from the ECA\&D variavle list: prec (RR), tavg (TG), tmax (TX), tmin(TN), sun (SS),
#'        wind_gust (FX), wind (FG).
#' @param data.name Always DATE
#' @return A data frame containing two columns: DATE with dates in PCICt format and "var.name" the varialbe name.
#' @template get_generic_example
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/}
#' @export
eca.input <- function(filename, var.name, date.name) {
  ifile.eca <-
    read.table(
      filename,
      skip = find.start.of.data.index(filename),
      sep = ",",
      header = T
    )[, c(date.name, var.name)]
  
  ifile.eca[[date.name]] <-
    as.PCICt(strptime(as.character(ifile.eca[[date.name]]), "%Y%m%d"), cal =
               "gregorian")
  
  ifile.eca[[var.name]][ifile.eca[[var.name]] == -9999] <- NA
  
  if (!(var.name %in% c("TG", "TX" , "TN", "RR", "SS", "FX", "FG"))) {
    return(ifile.eca)
  } else
    ifile.eca[[var.name]] <- ifile.eca[[var.name]] * 0.1
  
  return(ifile.eca)
}

#' Get available indices by name
#'
#' This function returns a vector of (function) names of available indices.
#'
#' This function takes a climdexInput object as input and returns the names of
#' all the indices which may be computed or, if \code{get.function.names} is
#' TRUE (the default), the names of the functions corresponding to the indices.
#'
#' @param ci Object of type climdexInput.
#' @param function.names Whether to return function names.
#' @return A vector containing an annual timeseries of precipitation in wet days.
#'
#' @examples
#' library(PCICt)
#'
#' ## Create a climdexInput object from some data already loaded in and
#' ## ready to go.
#'
#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
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
#' ## Get list of functions which might be run.
#' func.names <- climdex.get.available.indices(ci)
#'
#' @export
#'
climdex.get.available.indices <-
  function(ci, function.names = TRUE) {
    available.indices <-
      list(
        tmax = c(
          'su',
          'id',
          'txx',
          'txn',
          'tx10p',
          'tx90p',
          'wsdi',
          'csu',
          'txndaymin',
          'txndaymax'
        ),
        tmin = c(
          'fd',
          'tr',
          'tnx',
          'tnn',
          'tn10p',
          'tn90p',
          'csdi',
          'cfd',
          'tnndaymin',
          'tnndaymax'
        ),
        tavg = c(
          'gsl',
          'dtr',
          'hd17',
          'tmndaymin',
          'tmndaymax',
          'cd',
          'cw',
          'wd',
          'ww'
        ),
        prec = c(
          'rx1day',
          'rx5day',
          'sdii',
          'r10mm',
          'r20mm',
          'rnnmm',
          'cdd',
          'cwd',
          'r75ptot',
          'r95ptot',
          'r99ptot',
          'prcptot',
          'spi'
        ),
        snow = c('sdd', 'sdx', 'sd'),
        snow_new = c('nsd', 'nsx', 'nss'),
        wind = c("fg", "fgcalm", "fg6bft"),
        wind_gust = c('fxstorm', 'fxx'),
        wind_dir = c('ddnorth', 'ddeast', 'ddsouth', 'ddwest'),
        cloud = c('cc', 'cc6', 'cc2'),
        sun = c("ss"),
        sun_rel = c("sun_cloudy", "sun_sunny", "sun_relmean")
      )
    if (function.names) {
      return(paste("climdex", unlist(available.indices[names(ci@data)]), sep =
                     "."))
    } else {
      return(unlist(available.indices[names(ci@data)], use.names = FALSE))
    }
  }

##
## HELPERS FINISHED. IMPLEMENTATION BELOW.
##

#' Get series length at ends
#'
#' This function takes a series of boolean values and returns a list of
#' integers of the same length corresponding to the lengths at the ends of
#' sequences of TRUE values.
#'
#' It can often be useful to know how long a series of boolean values is. This
#' function provides a method of knowing where and how long such sequences are.
#'
#' @param x Sequence of booleans.
#' @param na.value Value to replace NAs with.
#' @return A vector consisting of the lengths of sequences of TRUE values at
#' the location of the last TRUE value in the sequence, and zeroes elsewhere.
#' @keywords ts climate
#' @examples
#'
#' ## Get lengths of sequences of TRUE values in a sequence
#' series.lengths <- get.series.lengths.at.ends(c(TRUE, TRUE, TRUE, FALSE,
#' TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
#'
#'
#' @export
get.series.lengths.at.ends <- function(x, na.value = FALSE) {
  stopifnot(is.logical(x) && is.logical(na.value))
  n <- length(x)
  if (n == 1)
    return(as.numeric(x))
  
  res <- rep(0, n)
  x[is.na(x)] <- na.value
  
  ## Compare series to lag-1 and lag+1 series; false added to trigger state transition from TRUE at ends of series
  start <- which(x & !(c(FALSE, x[1:(n - 1)])))
  end <- which(x & !(c(x[2:n], FALSE)))
  res[end] <- end - start + 1
  return(res)
}

#' Number of days (less than, greater than, etc) a threshold
#'
#' Produces sums of values that exceed (or are below) the specified threshold.
#'
#' This function takes a data series, a threshold, an operator, and a factor to
#' aggregate by. It uses the operator to compare the threshold to the data
#' series, creating a series of booleans, then sums the booleans according to
#' the factor.
#'
#' @param temp Sequence temperature values.
#' @param date.factor Factor to aggregate by.
#' @param threshold Threshold to use.
#' @param op Operator to use for comparison.
#' @return A vector consisting of the number of values that meet the criteria
#' in the given time period (as specified by \code{date.factor}).
#' @keywords ts climate
#' @examples
#' library(PCICt)
#'
#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
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
#' ## Calculate frost days.
#' fd <- number.days.op.threshold(ci@@data$tmin,
#'                                ci@@date.factors$annual, 0, "<")
#'
#' @export
number.days.op.threshold <-
  function(temp, date.factor, threshold, op = "<") {
    stopifnot(is.numeric(temp) &&
                is.numeric(threshold) && is.factor(date.factor))
    return(tapply.fast(match.fun(op)(temp, threshold), date.factor, sum, na.rm =
                         TRUE))
  }

#' @title Flexible GSL function
#'
#' @description
#' This function computes the growing season length (GSL) given the input,
#' which is allowed to vary considerably from the ETCCDI definitions.
#'
#' @details
#' This function is the function used to implement \code{\link{climdex.gsl}}.
#' It's designed to be flexible to allow for experimentation and testing of new
#' thresholds and methods.
#'
#' If you need to use this code for experimentation in the southern hemisphere,
#' you'll need to rip off the climdex.gsl code to rotate the year around so
#' that July 1st is treated as January 1st.
#'
#' See \code{\link{climdex.gsl}} for more information on what \code{gsl.mode}
#' does.
#'
#' @param daily.mean.temp Timeseries of daily mean temperature (in degrees C),
#' padded out to end on a year boundary (ie: starts on January 1st of some
#' year, ends on December 31st).
#' @param date.factor Factor of the same length as daily.mean.temp that divides
#' the timeseries up into years of data.
#' @param dates The corresponding series of dates.
#' @param northern.hemisphere Whether the data is from the northern hemisphere.
#' @param min.length The minimum number of days above or below the threshold
#' temperature that defines the start or end of a growing season.
#' @param t.thresh The temperature threshold for being considered part of a
#' growing season (in degrees C).
#' @param gsl.mode The growing season length mode (ETCCDI mode is "GSL").
#' @return A vector containing the number of days in the growing season for
#' each year.
#' @seealso \code{\link{climdex.gsl}}, \code{\link{climdexInput.csv}}.
#' @keywords ts climate
#' @examples
#' library(PCICt)
#'
#' ## Create a climdexInput object from some data already loaded in and
#' ## ready to go.
#'
#'#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
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
#' ## Create an annual timeseries of the growing season length in days.
#' gsl <- growing.season.length(ci@@data$tavg, ci@@date.factors$annual, ci@@dates,
#'                              ci@@northern.hemisphere, gsl.mode="GSL") *
#'        ci@@namasks$annual$tavg
#'
#' ## Print these out for testing purposes.
#' gsl
#'
#' @export
growing.season.length <-
  function(daily.mean.temp,
           date.factor,
           dates,
           northern.hemisphere,
           min.length = 6,
           t.thresh = 5,
           gsl.mode = c("GSL", "GSL_first", "GSL_max", "GSL_sum")) {
    gsl.mode <- match.arg(gsl.mode)
    month.series <- get.months(dates)
    transition.month <- if (northern.hemisphere)
      7
    else
      1
    if (gsl.mode == "GSL") {
      return(tapply.fast(1:length(daily.mean.temp), date.factor, function(idx) {
        temp.data <- daily.mean.temp[idx]
        ts.mid <-
          head(which(month.series[idx] == transition.month), n = 1)
        if (!length(ts.mid))
          return(NA)
        
        ts.len <- length(temp.data)
        gs.begin <-
          which(select.blocks.gt.length(temp.data[1:(ts.mid - 1)] > t.thresh, min.length - 1))
        
        ## Growing season actually ends the day -before- the sequence of sketchy days
        gs.end <-
          which(select.blocks.gt.length(temp.data[ts.mid:ts.len] < t.thresh, min.length - 1)) - 1
        
        ## If no growing season start, 0 length; if no end, ends at end of year; otherwise, end - start + 1
        return(ifelse(
          length(gs.begin) == 0,
          0,
          ifelse(
            length(gs.end) == 0,
            ts.len - gs.begin[1] + 1,
            gs.end[1] - gs.begin[1] + ts.mid
          )
        ))
      }))
    } else {
      in.gsl <-
        !select.blocks.gt.length(
          !select.blocks.gt.length(daily.mean.temp >= t.thresh, min.length - 1),
          min.length - 1
        )
      warning(
        "GSL_first, GSL_max, and GSL_sum are experimental alternative growing season length definitions. Use at your own risk."
      )
      
      innerfunc <-
        switch(
          gsl.mode,
          GSL_first = function(bl) {
            ifelse(any(bl > 0), (bl[bl > 0])[1], 0)
          },
          GSL_max = max,
          GSL_sum = sum
        )
      return(tapply.fast(in.gsl, date.factor, function(ts) {
        block.lengths <-
          get.series.lengths.at.ends(ts)
        return(innerfunc(block.lengths))
        
      }))
    }
  }

#' Lengths of strings of TRUE values
#'
#' Computes fraction of days above or below the baseline threshold for each
#' day, and averages them using the date factor passed in.
#'
#' This function computes fractions of days above or below baseline thresholds
#' for each day, then aggregates them using \code{date.factor}. It is used to
#' implement TN/TX 10/90p.
#'
#' @param temp Sequence of temperature values.
#' @param dates Sequence of associated dates.
#' @param jdays Sequence of associated days of year.
#' @param date.factor Factor to aggregate data using.
#' @param threshold.outside.base Sequence of thresholds to be used for data
#' outside the base period.
#' @param base.thresholds Data structure containing sets of thresholds to be
#' used inside the base period; see \link{climdexInput-class}.
#' @param base.range Date range (type PCICt) of the baseline period.
#' @param op Comparison operator to use.
#' @param max.missing.days Maximum number of NA values per time period.
#' @return A vector consisting of the mean fraction of days above or below the
#' supplied set of thresholds.
#' @note If date.factor is omitted, daily series will be returned.
#' @seealso \link{climdexInput-class}.
#' @keywords ts climate
#' @examples
#' library(PCICt)
#'
#' #' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
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
#' ## Compute monthly tx90p.
#' tx90p <- percent.days.op.threshold(ci@@data$tmax, ci@@dates, ci@@jdays,
#'                                    ci@@date.factors$monthly,
#'                                    ci@@quantiles$tmax$outbase$q90,
#'                                    ci@@quantiles$tmax$inbase$q90,
#'                                    ci@@base.range, ">",
#'                                    ci@@max.missing.days['monthly']) *
#'          ci@@namasks$monthly$tmax
#'
#' @export
percent.days.op.threshold <-
  function(temp,
           dates,
           jdays,
           date.factor,
           threshold.outside.base,
           base.thresholds,
           base.range,
           op = '<',
           max.missing.days) {
    f <- match.fun(op)
    dat <- f(temp, threshold.outside.base[jdays])
    
    inset <- dates >= base.range[1] & dates <= base.range[2]
    ## Don't use in-base thresholds with data shorter than two years; no years to replace with.
    if (sum(inset) > 0 && length(dates) >= 360 * 2) {
      jdays.base <- jdays[inset]
      years.base <- get.years(dates[inset])
      
      ## Get number of base years, subset temp data to base period only.
      temp.base <- temp[inset]
      years.base.range <- range(years.base)
      byrs <- (years.base.range[2] - years.base.range[1] + 1)
      
      ## Linearize thresholds, then compare them to the temperatures
      bdim <- dim(base.thresholds)
      dim(base.thresholds) <- c(bdim[1] * bdim[2], bdim[3])
      yday.byr.indices <-
        jdays.base + (years.base - get.years(base.range)[1]) * bdim[1]
      f.result <-
        f(rep(temp.base, byrs - 1), base.thresholds[yday.byr.indices,])
      dim(f.result) <- c(length(yday.byr.indices), bdim[3])
      
      ## Chop up data along the 2nd dim into a list; sum elements of the list
      dat[inset] <- rowSums(f.result, na.rm = TRUE) / (byrs - 1)
    }
    dat[is.nan(dat)] <- NA
    if (missing(date.factor))
      return(dat)
    na.mask <- get.na.mask(dat, date.factor, max.missing.days)
    ## FIXME: Need to monthly-ize the NA mask calculation, which will be ugly.
    ret <-
      tapply.fast(dat, date.factor, mean, na.rm = TRUE) * 100 * na.mask
    ret[is.nan(ret)] <- NA
    return(ret)
  }

#' @title Sum of spell lengths exceeding daily threshold
#'
#' @description
#' This function returns the number of spells of more than \code{min.length}
#' days which exceed or are below the given threshold.
#'
#' @details
#' This routine compares data to the thresholds using the given operator,
#' generating a series of TRUE or FALSE values; these values are then filtered
#' to remove any sequences of less than \code{min.length} days of TRUE values.
#' It then computes the lengths of the remaining sequences of TRUE values
#' (spells) and sums their lengths.
#'
#' The \code{spells.can.span.years} option controls whether spells must always
#' terminate at the end of a period, or whether they may continue until the
#' criteria ceases to be met or the end of the data is reached. The default for
#' fclimdex is FALSE.
#'
#' @param daily.temp Data to compute index on.
#' @param date.factor Date factor to split by.
#' @param jdays Timeseries of days of year.
#' @param thresholds The thresholds to compare to.
#' @param op The operator to use to compare data to threshold.
#' @param min.length The minimum spell length to be considered.
#' @param spells.can.span.years Whether spells can span years.
#' @param max.missing.days Maximum number of NA values per time period.
#' @return A timeseries of maximum spell lengths for each period.
#' @seealso \code{\link{climdex.wsdi}}.
#' @keywords ts climate
#' @examples
#'
#' prec.dat <- c(0.1, 3.0, 4.3, 1.9, 1.3, 6.0, 0, 0, 4.0, 1)
#' phony.date.factor <- factor(rep(1:2, each=5))
#'
#' ## With spells spanning years...
#' alttedi <- threshold.exceedance.duration.index(prec.dat,
#' phony.date.factor, rep(1:5, 2), rep(1, 5), ">=", 2, TRUE, 1)
#'
#' ## Without spells spanning years...
#' tedi <- threshold.exceedance.duration.index(prec.dat, phony.date.factor,
#' rep(1:5, 2), rep(1, 5), ">=", 2, FALSE, 1)
#'
#' @export
threshold.exceedance.duration.index <-
  function(daily.temp,
           date.factor,
           jdays,
           thresholds,
           op = ">",
           min.length = 6,
           spells.can.span.years = TRUE,
           max.missing.days) {
    stopifnot(is.numeric(c(daily.temp, thresholds, min.length)),
              is.factor(date.factor),
              is.function(match.fun(op)),
              min.length > 0)
    f <- match.fun(op)
    na.mask <-
      get.na.mask(is.na(daily.temp + thresholds[jdays]),
                  date.factor,
                  max.missing.days)
    
    if (spells.can.span.years) {
      periods <-
        select.blocks.gt.length(f(daily.temp, thresholds[jdays]), min.length - 1)
      return(tapply.fast(periods, date.factor, sum) * na.mask)
    } else {
      ## fclimdex behaviour...
      return(tapply.fast(1:length(daily.temp), date.factor, function(idx) {
        sum(select.blocks.gt.length(f(daily.temp[idx], thresholds[jdays[idx]]), min.length - 1))
      }) * na.mask)
    }
  }

## DTR
## Computes mean diurnal temperature range in each period (as specified by date.factor).
## Max and min temps are assumed to be same length
mean.daily.temp.range <-
  function(daily.max.temp,
           daily.min.temp,
           date.factor) {
    dat <-
      tapply.fast(daily.max.temp - daily.min.temp, date.factor, mean, na.rm =
                    TRUE)
    dat[is.nan(dat)] <- NA
    dat
  }

#' @title Number of days (less than, greater than, etc) a threshold
#'
#' @description
#' Produces sums of values that exceed (or are below) the specified threshold.
#'
#' @details
#' This function takes a data series, the number of days in the running window,
#' a date factor to aggregate by, and an optional modifier parameter
#' (center.mean.on.last.day). It computes the n-day running sum of
#' precipitation and returns the maximum n-day total precipitation per unit
#' time, as defined by \code{date.factor}.
#'
#' @param daily.prec Daily timeseries of precipitation.
#' @param date.factor Factor to aggregate by.
#' @param ndays Number of days in the running window.
#' @param center.mean.on.last.day Whether to center the n-day running mean on
#' the last day of the series, instead of the middle day.
#' @return A vector consisting of the maximum n-day sum of precipitation per
#' time interval.
#' @keywords ts climate
#' @examples
#' library(PCICt)
#'
#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
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
#' ## Compute rx5day on a monthly basis.
#' rx5day <- nday.consec.prec.max(ci@@data$prec, ci@@date.factors$monthly, 5)
#'
#' @export
#'
nday.consec.prec.max <-
  function(daily.prec,
           date.factor,
           ndays,
           center.mean.on.last.day = FALSE) {
    if (ndays == 1) {
      return(suppressWarnings(tapply.fast(daily.prec, date.factor, max, na.rm =
                                            TRUE)))
    }
    ## Ends of the data will be de-emphasized (padded with zero precip data); NAs replaced with 0
    daily.prec[is.na(daily.prec)] <- 0
    prec.runsum <- runmean(daily.prec, k = ndays, endrule = "NA")
    prec.runsum[is.na(prec.runsum)] <- 0
    if (center.mean.on.last.day) {
      k2 = ndays %/% 2
      prec.runsum <-
        c(rep(0, k2), prec.runsum[1:(length(prec.runsum) - k2)])
    }
    return(tapply.fast(prec.runsum, date.factor, max) * ndays)
  }
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
#' @param freq.fun Function to apply on the choosen period (e.g., max, min, ...)
#' @param center.mean.on.last.day Whether to center the n-day running mean on
#' the last day of the series, instead of the middle day.
#' @return A vector consisting of the mean n-day temp per
#' time interval.
#' @keywords ts climate
#'
#' @examples
#' library(PCICt)
#'
#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year","jday")]),
#'                        format="%Y %j", cal="gregorian")
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
#' ## Compute txnday on a monthly basis.
#' tx5daymax <- nday.consec.temp.mean(ci@@data$tmax, ci@@date.factors$monthly, freq.fun="max", ndays=5)
#'
#' @export
#'
nday.consec.temp.mean <-
  function(daily.temp,
           date.factor,
           ndays,
           freq.fun = "max",
           center.mean.on.last.day = FALSE) {
    if (ndays == 1) {
      return(suppressWarnings(tapply.fast(daily.temp, date.factor, mean, na.rm =
                                            TRUE)))
    }
    ## Ends of the data will be de-emphasized (padded with zero precip data); NAs replaced with 0
    daily.temp[is.na(daily.temp)] <- 0
    temp.runmean <- runmean(daily.temp, k = ndays, endrule = "NA")
    temp.runmean[is.na(temp.runmean)] <- 0
    if (center.mean.on.last.day) {
      k2 = ndays %/% 2
      temp.runmean <-
        c(rep(0, k2), temp.runmean[1:(length(temp.runmean) - k2)])
    }
    return(tapply.fast(temp.runmean, date.factor, match.fun(freq.fun)))
  }

#' Simple Precipitation Intensity Index
#'
#' This function implements the ETCCDI Simple Precipitation Intensity Index.
#'
#' The simple precipitation intensity index is computed by taking the sum of
#' precipitation in wet days (days with >1mm of precipitation), and dividing
#' that by the number of wet days in the period. This gives the mean
#' precipitation in wet days.
#'
#' @param daily.prec Data to compute index on.
#' @param date.factor Date factor to split by.
#' @return The mean precipitation in wet days for each period (as defined by
#' date.factor).
#' @keywords ts climate
#' @examples
#'
#' prec.dat <- c(0.1, 3.0, 4.3, 0.9, 1.3, 6.0, 0, 0, 4.0, 1)
#' phony.date.factor <- factor(rep(1:2, each=5))
#' sdii <- simple.precipitation.intensity.index(prec.dat, phony.date.factor)
#'
#' @export
simple.precipitation.intensity.index <-
  function(daily.prec, date.factor) {
    return(tapply.fast(daily.prec, date.factor, function(prec) {
      idx <-
        prec >= 1 &
        !is.na(prec)
      if (sum(idx) == 0) {
        return(0)
        
      } else {
        return(sum(prec[idx], na.rm = TRUE) / sum(idx))
      }
    }))
  }

#' @title Maximum spell length
#'
#' @description
#' This function returns the longest string of days which exceed or are below
#' the given threshold.
#'
#' @details
#' This routine compares data to the threshold using the given operator,
#' generating a series of TRUE or FALSE values. It then computes the lengths of
#' sequences of TRUE values (spells) and chooses the longest spell in each
#' period (as defined by date.factor).
#'
#' The \code{spells.can.span.years} option controls whether spells must always
#' terminate at the end of a period, or whether they may continue until the
#' criteria ceases to be met or the end of the data is reached. The default for
#' fclimdex is TRUE.
#'
#' @param daily.prec Data to compute index on.
#' @param date.factor Date factor to split by.
#' @param threshold The threshold to compare to.
#' @param op The operator to use to compare data to threshold.
#' @param spells.can.span.years Whether spells can span years.
#' @return A timeseries of maximum spell lengths for each period.
#' @seealso \code{\link{climdex.cdd}}.
#' @keywords ts climate
#' @examples
#'
#' prec.dat <- c(0.1, 3.0, 4.3, 1.9, 1.3, 6.0, 0, 0, 4.0, 1)
#' phony.date.factor <- factor(rep(1:2, each=5))
#'
#' ## With spells spanning years...
#' cwd <- spell.length.max(prec.dat, phony.date.factor, 1, ">=", TRUE)
#'
#' ## Without spells spanning years...
#' altcwd <- spell.length.max(prec.dat, phony.date.factor, 1, ">=", FALSE)
#'
#' @export
spell.length.max <-
  function(daily.prec,
           date.factor,
           threshold,
           op,
           spells.can.span.years) {
    bools <- match.fun(op)(daily.prec, threshold)
    
    if (spells.can.span.years) {
      all.true <- tapply.fast(bools, date.factor, all)
      max.spell <-
        tapply.fast(get.series.lengths.at.ends(bools), date.factor, max)
      
      ## Mask out values which are in the middle of a spell with NA
      na.mask <-
        c(1, NA)[as.integer((max.spell == 0) & all.true) + 1]
      return(max.spell * na.mask)
    } else {
      return(tapply.fast(bools, date.factor, function(x) {
        max(get.series.lengths.at.ends(x))
      }))
    }
  }

#' Sum of precipitation above a threshold
#'
#' This function returns the sum of values above a threshold for each period
#' (as defined by date.factor).
#'
#' This routine sums up all values which exceed or are below (depending on op)
#' the given threshold.
#'
#' @param daily.prec Data to compute index on.
#' @param date.factor Date factor to split by.
#' @param threshold The threshold to compare to.
#' @param op The operator to use to compare data to threshold.
#' @return A timeseries of sums of numbers above the threshold for each period.
#' @seealso \code{\link{climdex.r99ptot}}.
#' @keywords ts climate
#' @examples
#'
#' prec.dat <- c(0.1, 3.0, 4.3, 1.9, 1.3, 6.0, 0, 0, 4.0, 1)
#' phony.date.factor <- factor(rep(1:2, each=5))
#'
#' ## Compute equiv of PRCPTOT
#' prec.sum <- total.precip.op.threshold(prec.dat, phony.date.factor, 1, ">=")
#'
#' @export
total.precip.op.threshold <-
  function(daily.prec, date.factor, threshold, op) {
    f <- match.fun(op)
    return(tapply.fast(daily.prec, date.factor, function(pr) {
      return(sum(pr[f(pr, threshold)], na.rm = TRUE))
    }))
  }


## Returns an n-day running quantile for each day of data (dimensions c(dpy, q))
running.quantile <- function(data, n, q, dpy, min.fraction) {
  ret <-
    .Call("running_quantile_windowed",
          data,
          n,
          q,
          dpy,
          min.fraction,
          PACKAGE = 'climdex.pcic')
  dim(ret) <- c(length(q), dpy)
  return(t(ret))
}

#' Select blocks of TRUE values of sufficient length.
#'
#' Produces a sequence of booleans of the same length as input, with sequences
#' of TRUE values shorter than n replaced with FALSE.
#'
#' This function takes a series of booleans and returns a sequence of booleans
#' of equal length, with all sequences of TRUE of length \code{n} or shorter
#' replaced with sequences of FALSE. NA values are replaced with
#' \code{na.value}.
#'
#' @param d Sequence of booleans.
#' @param n Longest sequence of TRUE to replace with FALSE.
#' @param na.value Values to replace NAs with.
#' @return A vector of booleans, with the length \code{n} or less sequences of
#' TRUE replaced with FALSE.
#' @keywords ts climate
#' @examples
#'
#' ## Return only the first sequence of TRUE... second sequence will be FALSE.
#' foo <- select.blocks.gt.length(c(rep(TRUE, 4), FALSE, rep(TRUE, 3)), 3)
#'
#' @export
select.blocks.gt.length <- function(d, n, na.value = FALSE) {
  stopifnot(is.logical(d), is.numeric(n))
  
  if (n < 1)
    return(d)
  
  if (n >= length(d))
    return(rep(FALSE, length(d)))
  
  d[is.na(d)] <- na.value
  
  d2 <-
    Reduce(function(x, y) {
      return(c(rep(FALSE, y), d[1:(length(d) - y)]) & x)
    }, 1:n, d)
  return(Reduce(function(x, y) {
    return(c(d2[(y + 1):length(d2)], rep(FALSE, y)) | x)
  }, 1:n, d2))
}

#' Climdex quantile function
#'
#' This function implements R's type=8 in a more efficient manner.
#'
#' This is a reimplementation of R's type=8 created to improve the efficiency
#' of this package.
#'
#' @param x Data to compute quantiles on.
#' @param q Quantiles to be computed.
#' @return A vector of the quantiles in question.
#' @seealso \code{\link{quantile}}
#' @keywords ts climate
#' @examples
#'
#' ## Compute 10th, 50th, and 90th percentile of example data.
#' climdex.quantile(1:10, c(0.1, 0.5, 0.9))
#'
#' @export
climdex.quantile <- function(x, q = c(0, 0.25, 0.5, 0.75, 1)) {
  return(.Call("c_quantile2", as.double(x), q, PACKAGE = 'climdex.pcic'))
}
