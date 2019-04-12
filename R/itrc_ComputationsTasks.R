GetComputationsTasks <-
  function(
    signal.list,
    bootstrap.samples,
    bootstrap.test.sample = FALSE,
    ...
  ){
    GetComputationsSignalTasks <-
      function(
        signal.list,
        ...
      ){

        foreach::foreach(signal_ = signal.list) %do% {
          computation_task <- signal.list[which(signal.list <= signal_)]
          if(length(computation_task) <= 1 ){
            return(NULL)
          }
          return(
            list(
              max.signal = signal_,
              signal = computation_task))
        } ->
          computation_task.list
        computation.ids.nulls <- sapply(computation_task.list, is.null)
        if(sum(computation.ids.nulls) > 0){
          computation_task.list <-
            computation_task.list[-which(computation.ids.nulls)]
        }
        return(computation_task.list)
      }
    computation.signal.task.list <-
      GetComputationsSignalTasks(signal.list = signal.list)
    if(!bootstrap.test.sample){
      foreach::foreach(bootstrap_ = bootstrap.samples) %do% {
        foreach::foreach(computation.signal.task = computation.signal.task.list) %do% {
          computation.signal.task$bootstrap <- bootstrap_
          computation.signal.task$bootstrap.test <-
            bootstrap.samples[-which(bootstrap.samples == bootstrap_)]
          return(computation.signal.task)
        } %>%
          return()
      } %>%
        unlist(., recursive = FALSE)  %>%
        return()
    } else {
      foreach::foreach(bootstrap_ = bootstrap.samples) %do% {
        foreach::foreach(computation.signal.task = computation.signal.task.list) %do% {
          computation.signal.task$bootstrap <- bootstrap_
          foreach::foreach(bootstrap.test_ =
                             bootstrap.samples[-which(bootstrap.samples == bootstrap_)]) %do% {
                               computation.signal.task$bootstrap.test <- bootstrap.test_
                               return(computation.signal.task)
                             } %>%
            return()
        } %>%
          unlist(., recursive = FALSE)
      } %>%
        unlist(., recursive = FALSE) %>%
        return()
    }
}
