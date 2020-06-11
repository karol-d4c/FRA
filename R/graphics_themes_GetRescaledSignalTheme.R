#' GetRescaledSignalTheme
#' @description This functions return list of objects usfeul for theme compositions that depends on signal.
#' If \code{signals.rescale.df} and \code{colors} are not defined, then functions \code{rescaleSignalsValues.DataFrame}
#' and \code{GetLevelColors} are called respectively
#'
#'
#' @param model  FRAModel object return by SCRC function
#' @param signals.rescale.df optional, data.frame returned from \code{rescaleSignalsValues.DataFrame}
#' @param colors optional, vector returned from \code{GetLevelColors}
#'
#' @return \code{list(colors = colors, signals.rescale.df = signals.rescale.df)}
#' @export
GetRescaledSignalTheme   <-
  function(
    model = NULL,
    signals.rescale.df = NULL,
    colors = NULL,
    ...
  ){
    if(is.null(model)){
      stop("model must be an object of class FRAModel")
    } else if(class(model) != "FRAModel"){
      stop("model must be an object of class FRAModel")
    }

    col.rescaled <- "signal_rescaled"
    col.to.rescale <- model$signal
    if(is.null(signals.rescale.df)){
      signals.rescale.df <-
        rescaleSignalsValues.DataFrame(
          model = model,
          col.to.rescale = model$signal,
          col.rescaled   = col.rescaled,
          ...
        )
    }

    if(is.null(colors)){
      colors <-
        GetLevelColors(
          levels = as.numeric(signals.rescale.df[[col.rescaled]]),
          levels.names = signals.rescale.df[[model$signal]],
          ...
        )
    }
    return(
      list(
        col.rescaled = col.rescaled,
        col.to.rescale = col.to.rescale,
        colors = colors,
        signals.rescale.df = signals.rescale.df
      )
    )
  }
