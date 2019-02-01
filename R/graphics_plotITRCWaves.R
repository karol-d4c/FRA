#'
#'
#' @export
plotITRCWaves <-
  function(
    model,
    ...
  ){
    force(g.basic)

    model <-
      GetConfusionData(model)

    ggplot2::ggplot() +
      ggplot2::geom_point(
        data = model$confusion.waves %>%
          dplyr::filter(signal == class),
        mapping = ggplot2::aes_string(
          x = "log(signal)",
          y = "itrc")
      ) +
      ggplot2::geom_line(
        data = model$confusion.waves %>%
          dplyr::filter(signal == class) %>%
          dplyr::mutate(type = "itrc"),
        mapping = ggplot2::aes_string(
          x = "log(signal)",
          y = "itrc",
          group = "type")
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
      )


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
