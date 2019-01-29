#'
GenerateNoBootstrapSample <-
  function(model){
    UseMethod("GenerateNoBootstrapSample")
  }
#'
#'
GenerateNoBootstrapSample.ITRCModel <-
  function(
    model,
    ...
  ){
    data.frame(
        bootstrap = 1,
        bootstrap.sample = paste(1, 1:nrow(model$data), sep = "_"),
        sample    = model$data[[model$sample]]
      ) ->
      model$bootstrap.samples.df
      model$bootstrap.samples <-
       (model$bootstrap.samples.df %>%
          dplyr::distinct(bootstrap))[["bootstrap"]]
      return(model)
  }

#'
#'
#'
GenerateBootstrapSample <-
  function(model,
           ...){
    UseMethod("GenerateBootstrapSample")
  }

#'
#'
#'
GenerateBootstrapSample.ITRCModel <-
  function(
    model,
    ...
  ) {
    # dir.create(output.path, recursive = TRUE)
    # output.sample.path <- paste(output.path, "samples", sep = "")
    # registerDoParallel(no_cores)
    # data.raw.list <-
    #   foreach::foreach(bootstrap.i = 1:bootstrap.num) %dopar% {
    #     if(!force.recalculation &
    #        file.exists(paste(output.sample.path, bootstrap.i, "data.rds", sep = "/"))){
    #       data.raw <- (readRDS(paste(output.sample.path, bootstrap.i, "data.rds", sep = "/")))$data.rds
    #       data.raw$sample <- bootstrap.i
    #       return(data.raw)
    #
    #     } else #if(calculate.remaining){
    #     {
    #       data.raw <-
    #         do.call(
    #           what = fun.sample,
    #           args = append(fun.sample.args,
    #                         list(
    #                           n = n,
    #                           output.path = paste(output.sample.path, bootstrap.i, sep = "/"))))
    #       data.raw$sample <- bootstrap.i
    #       return(data.raw)
    #     }
    #   }
    # stopImplicitCluster()

    return(model)
}
