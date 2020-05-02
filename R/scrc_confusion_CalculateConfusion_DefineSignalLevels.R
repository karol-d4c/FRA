GetLevelsDf.Class <-
  function(
    model,
    signal.list,
    ...){
    colnames = c()
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

GetLevelsDf.Signal <-
  function(
    model,
    signal.list,
    ...){
    colnames = c()
    data.frame(
      signal_factor = signal.list,
      signal_level  = 1:length(signal.list)) %>%
      dplyr::rename_(
        .dots =
          setNames(
            nm = model$signal,
            object = "signal_factor")) %>%
      return()
  }
