#' plotSCRCWaves.Comparison
#'
#' @description This functions return ggplot2 figure that visualise Information
#'  Theoretic Response Curves and specificity of cellular response to particular signals.
#' @param model SCRCModel object return by SCRC function
#' @param data, column "model$signal" and
#' @param variable.to.compare, column of data
#' @param data_raw_min, column of data
#' @param ylab.right character, label of right y axes and legend title, default \code{"Signal levels"}
#' @param theme.signal optional, object returned by \code{GetRescaledSignalTheme}
#' @param signal.max ...
#' @inheritDotParams plotSCRCWaves
# #' @inheritDotParams GetPlotTheme
#' @details TODO important
#' @export
plotSCRCWaves.Comparison <-
  function(
    model,
    data,
    variable.to.compare,
    variable.to.rescale = variable.to.compare,
    theme.signal = NULL,
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
      stop("model must be an object of class SCRCModel")
    } else if(class(model) != "SCRCModel"){
      stop("model must be an object of class SCRCModel")
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

    if(is.null(theme.signal)){
      theme.signal <-
        SCRC::GetRescaledSignalTheme(
          model = model,
          ...
        )
    }
    signals.rescale.df <- theme.signal$signals.rescale.df
    colors <- theme.signal$colors
    col.rescaled <- theme.signal$col.rescaled
    col.to.rescale <- theme.signal$col.to.rescale

    if(is.null(variable.to.rescale)){
      variable.to.rescale <- variable.rescaled
    }
    rescaled.list <-
      SCRC::rescaleDataToSCRC.DataFrame(
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
    print(signals.rescale.df)
    g.plot <-
      SCRC::plotSCRCWaves(
        model = model,
        signals.rescale.df = signals.rescale.df,
        getScaleY =
          getScaleY.SCRCComparison,
        scaleY.a = rescaled.list$a,
        scaleY.b = rescaled.list$b,
        ylab.right = ylab.right,
        theme.signal = theme.signal,
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
