#' plotITRCWaves.Comparison
#'
#' @description This functions return ggplot2 figure that visualise Information
#'  Theoretic Response Curves and specificity of cellular response to particular signals.
#' @param model ITRCModel object return by ITRC function
#' @param data, column "model$signal" and
#' @param variable.to.compare, column of data
#' @param data_raw_min, column of data
#' @param ylab.right character, label of right y axes and legend title, default \code{"Signal levels"}
#' @inheritDotParams plotITRCWaves
# #' @inheritDotParams GetPlotTheme
#' @details TODO important
#' @export
plotITRCWaves.Comparison <-
  function(
    model,
    data,
    variable.to.compare,
    variable.to.rescale = variable.to.compare,
    signals.rescale.df = NULL,
    ylab.right = "Cellular Response",
    data_raw_min = 0,
    theme.data.points = list(
      color = "black"
    ),
    theme.data.line = list(
      color = "black"
    ),
    ...
  ){
    if(is.null(model)){
      stop("model must be an object of class ITRCModel")
    } else if(class(model) != "ITRCModel"){
      stop("model must be an object of class ITRCModel")
    }

    if(!exists(x = "data")){
      stop("data not defined")
    } else {
      if(!("data.frame" %in% class(data))){
        stop("data should be data.frame or data.table")
      }
    }

    if(!exists(x = "variable.to.compare")){
      stop("variable.to.compare must be defined")
    }

    col.rescaled <- "signal_rescaled"
    col.to.rescale <- model$signal
    if(is.null(signals.rescale.df)){
      signals.rescale.df <-
        rescaleSignalsValues.DataFrame(
          model = model,
          col.to.rescale = col.to.rescale,
          col.rescaled   = col.rescaled, #rescale.fun = rescale.fun)
          ...)
    }

    if(is.null(variable.to.rescale)){
      variable.to.rescale <- variable.rescaled
    }
    rescaled.list <-
      rescaleDataToITRC.DataFrame(
        model = model,
        data  = data,
        variable.to.compare = variable.to.compare,
        variable.rescaled = variable.to.rescale,
        variable.to.rescale = variable.to.rescale,
        data_raw_min = data_raw_min,
        ...)

    rescaled.list$data.rescaled %>%
      dplyr::left_join(
        signals.rescale.df,
        by = col.to.rescale
      ) ->
      rescaled.list$data.rescaled

    #ylimits_ <- c(floor(ylimits_[1]), ceiling(ylimits_[2]))

    g.plot <-
      ITRC::plotITRCWaves(
        model = model,
        signals.rescale.df = signals.rescale.df,
        getScaleY =
          getScaleY.ITRCComparison,
        scaleY.a = rescaled.list$a,
        scaleY.b = rescaled.list$b,
        ylab.right = ylab.right,
        ...
      )
    if(!is.null(theme.data.points)){
      g.plot +
        do.call(
          what = ggplot2::geom_point,
          args = append(
            theme.data.points,
            list(
              mapping = ggplot2::aes_string(
                x = getVariable(col.rescaled),
                y = "response_rescaled"
              ),
              data = rescaled.list$data.rescaled
            )
          )
        )  -> g.plot
    }
    if(!is.null(theme.data.line)){
      g.plot +
        do.call(
          what = ggplot2::geom_line,
          args = append(
            theme.data.line,
            list(
              mapping = ggplot2::aes_string(
                x = getVariable(col.rescaled),
                y = "response_rescaled",
                group = "variable"
              ),
              data = rescaled.list$data.rescaled
            )
          )
        )-> g.plot
    }

    return(g.plot)
  }
