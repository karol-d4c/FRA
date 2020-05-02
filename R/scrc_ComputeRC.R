ComputeRC <-
  function(model,
           parallel_cores,
           rc_type = "mean",
           ...
  ){
    if(is.null(model)){
      return()
    }
    rc.name <- paste("rc", rc_type, sep = "_")
    rc.sum.name <- paste("rc.sum", rc_type, sep = "_")
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
      model[[rc.name]]
    doParallel::registerDoParallel(parallel_cores)

    model[[rc.name]] %>%
      dplyr::group_by_(model$signal)%>%
      dplyr::summarise_(
        .dots =
          setNames(
            object = paste(rc_type, "(`", model$response, "`)", sep = ""),
            nm = model$response)) ->
      model[[rc.sum.name]]
    return(model)
  }
