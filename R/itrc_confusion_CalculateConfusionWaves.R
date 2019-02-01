#'
#'
CalculateConfusionWaves <-
  function(
    model,
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
        y = GetLevelsDf.Signal(
          model = model,
          signal.list = signal.list),
        by = model$signal) %>%
      dplyr::left_join(
        y = GetLevelsDf.Class(
          model = model,
          signal.list = signal.list),
        by = model$class) %>%
      return()
  }
