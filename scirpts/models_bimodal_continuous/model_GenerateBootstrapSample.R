### ###
###
### ###

GenerateBootstrapSample <-
  function(
    fun.sample,
    fun.sample.args = list(),
    bootstrap.num = 4,
    output.path,
    no_cores = 8,
    n = 10000,
    return.bootstrap.sample = FALSE,
    force.recalculation = TRUE,
    calculate.remaining = TRUE,
    ...
  ) {
    dir.create(output.path, recursive = TRUE)
    output.sample.path <- paste(output.path, "samples", sep = "")
    registerDoParallel(no_cores)
    data.raw.list <-
      foreach::foreach(bootstrap.i = 1:bootstrap.num) %dopar% {
        if(!force.recalculation &
           file.exists(paste(output.sample.path, bootstrap.i, "data.rds", sep = "/"))){
          data.raw <- (readRDS(paste(output.sample.path, bootstrap.i, "data.rds", sep = "/")))$data.rds
          data.raw$sample <- bootstrap.i
          return(data.raw)

        } else #if(calculate.remaining){
        {
          data.raw <-
            do.call(
              what = fun.sample,
              args = append(fun.sample.args,
                            list(
                              n = n,
                              output.path = paste(output.sample.path, bootstrap.i, sep = "/"))))
          data.raw$sample <- bootstrap.i
          return(data.raw)
        }
      }
    stopImplicitCluster()

    data.raw <-
      do.call(what = rbind,
              args = data.raw.list) %>%
      data.table()

    stim.list <- (data.raw %>% dplyr::distinct(Stim))[["Stim"]]
    signal.list <- paste("X", stim.list, sep = "")

    density.estimation.limits <-
      DensityEstimationLimits(data.long = data.raw)
    model <- model_ReadGeneratedModel(
      output.path = output.path,
      model.name = model.name)
    if(is.null(model)){
      model <-
        list(
          data.raw = data.raw,
          n = n,
          stim.list = stim.list,
          signal.list = signal.list,
          sample.list = 1:bootstrap.num,
          density.estimation.limits = density.estimation.limits)
    } else {
      model$data.raw <- data.raw
      model$sample.list <- 1:bootstrap.num
    }
    saveRDS(object = model,
            file   = paste(output.path, "data.rds", sep = "/"))
    if(return.bootstrap.sample){
      return(data.raw)
    }
  }

