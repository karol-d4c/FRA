#' plotITRCWaves
#'
#' @description This functions return ggplot2 figure that visualise Information
#'  Theoretic Response Curves and specificity of cellular response to particular signals.
#'
#' @param model ITRCModel object return by ITRC function
#' @param title_ character, specify title of plot, default \code{"Information Theoretic Response Curve"}
#' @param xlab_ character, label of x axes, default \code{"States number"}
#' @param ylab_ character, label of y axes and legend title, default \code{"Signal levels"}
#' @param ylimits_ TRUE FALSE or vector of minimum and maximum of y axes
#' @param fill.guide_ logical, specify if legend should be displayed
#' @inheritDotParams rescaleSignalsValues
# #' @inheritDotParams GetPlotTheme
#' @details TODO important
#' @export
plotITRCWaves <-
  function(
    model,
    title_ = "Information Theoretic Response Curve",
    xlab_ = "States number",
    ylab_ = "Signal levels",
    fill.guide_ = TRUE,
    ylimits_ = TRUE,
    ...
  ){
    if(is.null(model)){
      stop("model must be an object of class ITRCModel")
    } else if(class(model) != "ITRCModel"){
      stop("model must be an object of class ITRCModel")
    }

    model <-
      CalculateConfusion(
        model,
        ...)

    col.rescaled <- "signal_rescaled"
    col.to.rescale <- model$signal
    signals.rescale.df <-
      rescaleSignalsValues.DataFrame(
        model = model,
        col.to.rescale = model$signal,
        col.rescaled   = col.rescaled,
        ...)

    print(signals.rescale.df)
    print(as.numeric(signals.rescale.df[[col.rescaled]]))
    colors <-
      GetLevelColors(
        levels = as.numeric(signals.rescale.df[[col.rescaled]]),
        levels.names = signals.rescale.df[[model$signal]]
      )

    g.plot <-
      ggplot2::ggplot() +
      GetPlotTheme(...) +
      ggplot2::ggtitle(title_)



    x.itrc  <- "signal_rescaled"
    y.itrc <- "itrc"
    group.itrc <- "type"
    ggplot.data.itrc <-
      model$confusion.waves %>%
      dplyr::filter_(paste(model$signal, "==", model$class)) %>%
      dplyr::left_join(
        signals.rescale.df,
        by = model$signal
      ) %>%
      dplyr::mutate(
        type = "itrc")

    g.plot +
      ggplot2::geom_point(
        data = ggplot.data.itrc,
        mapping = ggplot2::aes_string(
          x = x.itrc,
          y = y.itrc)
      ) +
      ggplot2::geom_line(
        data = ggplot.data.itrc,
        mapping = ggplot2::aes_string(
          x = x.itrc,
          y = y.itrc,
          group = group.itrc)
      ) ->
      g.plot

    x.waves  <- "signal_rescaled"
    y.waves <- "position"
    fill.waves <- paste("factor(", model$class, ")")
    group.waves <- paste("interaction(", model$class, ", type)")
    ggplot.data.waves <-
      model$confusion.waves.polygon %>%
      dplyr::left_join(
        signals.rescale.df,
        by = model$signal
      ) %>%
      dplyr::mutate(
        type = "itrc")

    g.plot +
      ggplot2::geom_polygon(
        data = ggplot.data.waves,
        mapping = ggplot2::aes_string(
          x = x.waves,
          y = y.waves,
          fill = fill.waves,
          group = group.waves),
        alpha = 0.5
      ) ->
      g.plot

    g.plot +
      ggplot2::scale_fill_manual(
        name = xlab_,
        values = colors,
        labels = signals.rescale.df[[model$signal]]
      ) ->
      g.plot

    if(!is.factor(signals.rescale.df[[col.rescaled]])){
      g.plot +
        ggplot2::scale_x_continuous(
        name = xlab_,
        breaks = signals.rescale.df[[col.rescaled]],
        labels = signals.rescale.df[[col.to.rescale]]
      )  ->
      g.plot
    } else {
      g.plot +
        ggplot2::scale_x_discrete(
          name = xlab_)  ->
        g.plot
    }


      if(is.logical(ylimits_)){
        if(ylimits_){
          ylimits_ <- c(0, 1.2*max(ggplot.data.waves$position))
        } else {
          ylimits_ <- NULL
        }
      }
      if(!is.null(ylimits_)){
        if(length(ylimits_) != 2){
          warnings("Limits of y axes should be defined as vector that consists
                   minimal and maximal numeric value")
          ylimits_ <- NULL
        }
      }

      if(!is.null(ylimits_)){
        g.plot +
          ggplot2::scale_y_continuous(
            name = ylab_,
            limits = ylimits_
          )
      } else {
        g.plot +
          ggplot2::scale_y_continuous(
            name = ylab_
          )
      } ->
      g.plot



  }