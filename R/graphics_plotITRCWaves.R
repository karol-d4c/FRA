#### ####
GetSignalLevelDf <-
  function(
    model,
    signal.list,
    ...){
    data.frame(
      class_factor = signal.list,
      class_level  = 1:length(signal.list)) %>%
      dplyr::rename_(
        .dots =
          setNames(
            nm = model$class,
            object = "class_factor")) %>%
      return()
  }
#### ####
CalculateConfusionWavesData <-
  function(model,
           signal.list,
           ...){

  foreach::foreach(signal_ = signal.list) %do% {
      model$confusion.matrix %>%
        dplyr::filter(max.signal == max(max.signal)) %>%
        dplyr::filter_(
          paste(model$signal, "==", signal_)) %>%
        dplyr::mutate_(if_else_signal = model$signal,
                       if_else_class = model$class) %>%
        dplyr::mutate(
          comparison =
            dplyr::if_else(
              condition = if_else_class < if_else_signal, #class < signal
              true  = "down",
              false =
                dplyr::if_else(
                  condition = if_else_class == if_else_signal, # class == signal,
                  true  = "equal",
                  false = "up"))) %>%
        dplyr::select(-if_else_class,
                      -if_else_signal) ->
        df.confusion.tmp


      foreach::foreach(class_ = signal.list) %do% {
        (if(class_ == signal_){
          df.confusion.tmp %>%
            dplyr::filter_(
              paste(model$class, ">=", class_, "&", model$class, "<", signal_))
          data.table::data.table( prob = 0 )

        } else {
          if(class_ < signal_){
            df.confusion.tmp %>%
              dplyr::filter_(
                paste(model$class, ">=", class_, "&", model$class, "<", signal_)) %>%
              #dplyr::group_by(max.signal) %>%
              dplyr::summarise(
                prob = -sum(prob))
          } else {
            df.confusion.tmp %>%
              dplyr::filter_(
                paste(model$class, "<=", class_, "&", model$class, ">", signal_)) %>%
              #dplyr::group_by(max.signal) %>%
              dplyr::summarise(
                prob = sum(prob))
          }
        }) %>%
          dplyr::mutate_(
            .dots =
              setNames(
                nm = c(model$signal,
                       model$class
                       #, max.signal
                ),
                object = c(signal_, class_))) %>%
          return()
      } %>%
        do.call(
          what = rbind,
          args = .
        ) %>%
        return()
    } %>%
      do.call(
        what = rbind,
        args = .) %>%
      dplyr::left_join(
        y = model$itrc,
        by = model$signal) %>%
      dplyr::left_join(
        (data.frame(
          signal_factor = signal.list,
          signal_level  = 1:length(signal.list)) %>%
           dplyr::rename_(
             .dots =
               setNames(
                 nm = model$signal,
                 object = "signal_factor"))),
        by = model$signal) %>%
      dplyr::left_join(GetSignalLevelDf(model = model,
                                        signal.list = signal.list),
                       by = model$class) %>%
      return()
  }
#### ####
CalculateConfusionWavesPolygonData <-
  function(
    model,
    confusion.waves,
    signal.list,
    ...
  ){
    confusion.waves %>%
      dplyr::mutate(position  = itrc + prob) %>%
      dplyr::arrange_(
        model$signal,
        model$class) %>%
      rbind(
        do.call(
          what = rbind,
          args = foreach::foreach(class_level_ =  1:length(signal.list)) %do% {
            (confusion.waves %>%
               dplyr::filter(signal == class &
                               (1 >= abs(class_level_ -
                                           signal_level))) %>%
               dplyr::mutate(position = itrc)
            ) %>%
              rbind(
                (confusion.waves %>%
                   dplyr::filter((class_level == class_level_ - 1) &
                                   (signal_level < class_level_ - 1)) %>%
                   dplyr::mutate(position = itrc + prob))) %>%
              rbind(
                (confusion.waves %>%
                   dplyr::filter((class_level == class_level_ + 1) &
                                   (signal_level > class_level_ + 1)) %>%
                   dplyr::mutate(position = itrc + prob))) %>%
              dplyr::mutate_(
                .dots = setNames(nm = model$class,
                                 object = ((GetSignalLevelDf(model = model,
                                                             signal.list = signal.list) %>%
                                              dplyr::filter(class_level ==
                                                              class_level_))[[model$class]]))) %>%
              dplyr::arrange_(paste("-",model$signal))
          }
        )) %>%
      dplyr::select_(model$signal,
                     model$class,
                     "position") %>%
      dplyr::ungroup() %>%
      return()
  }
#### ####
GetConfusionData <-
  function(model,
           ...){
    signal.list <-
      (model$confusion.matrix %>%
         dplyr::distinct_(model$signal) %>%
         dplyr::arrange_(model$signal))[[model$signal]]
    model$confusion.waves <-
      CalculateConfusionWavesData(model = model, signal.list = signal.list)
    model$confusion.waves.polygon <-
      CalculateConfusionWavesPolygonData(model = model,
                                                          confusion.waves = model$confusion.waves,
                                                          signal.list = signal.list)
    return(model)
  }
#### ####
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
