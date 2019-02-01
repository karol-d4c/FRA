#' plotITRCWaves
#'
#' ...
#' @param model ITRC model
#' @inheritDotParams rescaleSignalsValues
# #' @inheritDotParams GetPlotTheme
#'
#' @examples ...
#' @export
plotITRCWaves <-
  function(
    model,
    plot.title_ = "Information Theoretic Response Curve",
    ...
  ){
    # force(g.basic)
    model <-
      GetConfusionData(
        model)

    # model$rescale.stims <-
    #   rescaleClassesValues.DataFrame(
    #     data =
    #       model$data.raw %>%
    #       dplyr::distinct(Stim) %>%
    #       dplyr::filter(Stim %in% stim.list.rcc) %>%
    #       dplyr::arrange(Stim) %>%
    #       dplyr::ungroup(),
    #     col.to.rescale = "Stim",
    #     col.rescaled = "logStim",
    #     class.remove = 0,
    #     #class.remove.rescale = 0.25,
    #     rescale.fun = rcc.waves.logfun)
    #
    # virids.option_ <- "D"
    # colors.pallete <- viridis(n = 1000,
    #                           option = virids.option_, end = 1)
    #
    # model$rcc.pallete <-
    #   GetWaveColors(
    #     class.df =
    #       model$rescale.stims,
    #     class.name = "Stim",
    #     class.values = "logStim",
    #     colors.pallete = colors.pallete
    #   )

    x_  <- "signal_rescaled"
    y.itrc <- "itrc"
    group.itrc <- "type"
    signals.rescale.df <-
      rescaleSignalsValues.DataFrame(
        model = model,
        col.to.rescale = model$signal,
        col.rescaled   = x_,
        ...)

    ggplot.data.itrc <-
      model$confusion.waves %>%
      dplyr::filter(signal == class) %>%
      dplyr::left_join(
        signals.rescale.df,
        by = model$signal
      ) %>%
      dplyr::mutate(
        type = "itrc")


    ggplot2::ggplot() +
      ggplot2::geom_point(
        data = ggplot.data.itrc,
        mapping = ggplot2::aes_string(
          x = x_,
          y = y.itrc)
      ) +
      ggplot2::geom_line(
        data = ggplot.data.itrc,
        mapping = ggplot2::aes_string(
          x = x_,
          y = y.itrc,
          group = group.itrc)
      ) +
      ggplot2::geom_polygon(
        data = model$confusion.waves.polygon %>%
          #dplyr::filter(class %in% c(0, 1e-02,  3e-02, 1e-01)) %>%
          dplyr::mutate(type = "itrc"),,
        mapping = ggplot2::aes_string(
          x = "log(signal)",
          y = "position",
          fill = "factor(class)",
          group = "interaction(class, type)"),
        alpha = 0.5
      ) +
      GetPlotTheme(...)


    #
    #     g <-
    #       g.basic +
    #       geom_polygon(
    #         data = df.confusion.wave.polygon,
    #         mapping = aes_string(
    #           x = rcc.waves.x_,
    #           y = "res",
    #           fill = "factor(Stim.class)"),
    #         alpha = rcc.waves.alpha_
    #       ) +
    #       geom_line(
    #         data = df.confusion.wave %>%
    #           dplyr::filter(Stim == Stim.class),
    #         mapping = aes_string(x = rcc.waves.x_,
    #                              y = "value",
    #                              group = "type"),
    #         size = graphics.line.size
    #       ) +
    #       ggtitle(title_) +
    #       do.call(graphics.args$theme.fun, args = graphics.args$theme.args)
    #     if(!is.null(rcc.polygon_pallete)){
    #       g <-
    #         g +
    #         scale_fill_manual(values = rcc.polygon_pallete)
    #     }
    #     if(!is.null(rcc.waves.ylim_)){
    #       if(is.null(scale_y_continuous.args)){
    #         scale_y_continuous.args <- list()
    #       }
    #       scale_y_continuous.args$limits <- rcc.waves.ylim_
    #       g <-
    #         g +
    #         do.call(what = scale_y_continuous,
    #                 args = scale_y_continuous.args)
    #     }
    #     if(save.plot & !is.null(itrc_confusion_waves.plot.path)){
    #       ggsave.args <- graphics.args$ggsave.args
    #       ggsave.args$height <- 8
    #       ggsave.args$width <- (3/4)*14
    #       do.call(what = ggsave,
    #               args = append(ggsave.args,
    #                             list(
    #                               filename = paste(itrc_confusion_waves.plot.path, "itrc_confusion_waves.pdf", sep = "/"),
    #                               plot = g)
    #               ))
    #     }
    #     if(return.plot){
    #       return(g)
    #     }

  }
