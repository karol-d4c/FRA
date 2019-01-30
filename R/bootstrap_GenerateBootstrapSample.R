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
    data.table::data.table(
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
#' @param model
#' @param bootstrap.number
#' @param bootstrap.sample_size
#'
#' @importFrom foreach %dopar%
#'
GenerateBootstrapSample.ITRCModel <-
  function(
    model,
    bootstrap.number = 1,
    bootstrap.sample_size = NULL,
    parallel_cores = 1,
    ...
  ) {

    stopifnot(is.numeric(bootstrap.number))
    stopifnot(is.numeric(parallel_cores))

    if(is.null(bootstrap.sample_size)){
      bootstrap.sample_size <-
        GenerateDefaultBootstrapSampleSize(
          model,
          bootstrap.sample_size
        )
    }

    doParallel::registerDoParallel(parallel_cores)
    foreach::foreach(bootstrap.i = 1:bootstrap.number) %dopar% {

      foreach::foreach(
        signal_ =
          (model$data %>%
             dplyr::distinct_(model$signal))[[model$signal]]) %dopar% {
               data.frame(
                 bootstrap = bootstrap.i,
                 bootstrap.sample = paste(bootstrap.i,
                                          signal_,
                                          1:bootstrap.number,
                                          sep = "_"),
                 sample    =
                   sample(
                     x =
                       (model$data %>%
                          dplyr::filter_(paste(model$signal, "==" , signal_)))[[model$sample]],
                     size = bootstrap.sample_size,
                     replace = TRUE
                   ))
             } %>%
        do.call(what = rbind,
                args = .) %>%
        return()
    } %>%
      do.call(what = rbind,
              args = .) %>%
      data.table::data.table() ->
      model$bootstrap.samples.df
    doParallel::stopImplicitCluster()

    model$bootstrap.samples <-
      (model$bootstrap.samples.df %>%
         dplyr::distinct(bootstrap))[["bootstrap"]]

    return(model)
}

