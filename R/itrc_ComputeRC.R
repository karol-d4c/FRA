ComputeRC <-
  function(model,
           parallel_cores,
           rc_type = "mean",
           ...
  ){
    if(is.null(model)){
      return()
    }
    signal.list <- (model$data %>%
                      dplyr::arrange_(model$signal) %>%
                      dplyr::distinct_(model$signal))[[model$signal]]
    ### verify rc_type

    doParallel::registerDoParallel(parallel_cores)
    foreach::foreach(
      bootstrap.sample =
        model$bootstrap.samples) %dopar% {
          returnBootstrapData(
            model = model,
            bootstrap_ =
              bootstrap.sample) %>%
            dplyr::group_by_(model$signal) %>%
            dplyr::summarise_(
              .dots =
                setNames(
                  object = paste(rc_type, "(`", model$response, "`)", sep = ""),
                  nm = model$response)) %>%
            dplyr::mutate(bootstrap = bootstrap.sample) %>%
            return(.)
        } %>%
      do.call(
        what = rbind,
        args = .
      ) ->
      model$rc
    doParallel::registerDoParallel(parallel_cores)

    model$rc %>%
      dplyr::group_by_(model$signal)%>%
      dplyr::summarise_(
        .dots =
          setNames(
            object = paste(rc_type, "(`", model$response, "`)", sep = ""),
            nm = model$response)) ->
      model$rc.sum
    return(model)
  }
