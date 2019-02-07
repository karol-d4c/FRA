CalculateConfusionWavesPolygons <-
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
               dplyr::filter_(paste(model$signal, "==", model$class, "&", "(1 >= abs(class_level_ - signal_level))")) %>%
               dplyr::mutate(position = itrc)) %>%
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
                                 object = ((GetLevelsDf.Class(model = model,
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
