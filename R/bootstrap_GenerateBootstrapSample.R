#' GenerateNoBootstrapSample
#'
#' @param model ITRCModel
#'
#' @description This function generate bootstrap samples
#'
GenerateNoBootstrapSample <-
  function(model){
    UseMethod("GenerateNoBootstrapSample")
  }

#' GenerateNoBootstrapSample.ITRCModel
#'
#' @param model ITRCModel
#'
#' @description This function generate bootstrap samples
#'
GenerateNoBootstrapSample.ITRCModel <-
  function(
    model,
    ...
  ){
    data.table::data.table(
      bootstrap = c(rep(x = 1, times = nrow(model$data)),
                    rep(x = 2, times = nrow(model$data))),
      bootstrap.sample =
        c(paste(1,1:nrow(model$data), sep = "_"),
          paste(2, 1:nrow(model$data), sep = "_")),
      sample    = c(model$data[[model$sample]],model$data[[model$sample]])
    ) ->
      model$bootstrap.samples.df
    model$bootstrap.samples <-
      (model$bootstrap.samples.df %>%
         dplyr::distinct(bootstrap))[["bootstrap"]]
    return(model)
  }

#' GenerateBootstrapSample
#'
#' @description This function generate bootstrap samples
#'
#' @param model ITRCModel
#'
GenerateBootstrapSample <-
  function(model,
           ...){
    UseMethod("GenerateBootstrapSample")
  }

#' GenerateBootstrapSample.ITRCModel
#'
#' @param model ITRCModel
#' @param bootstrap.number number of bootstrap sampling
#' @param bootstrap.sample_size size of one bootstrap sample
#'
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @importFrom dplyr %>%
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
    if(bootstrap.number < 2){
      bootstrap.number = 2
    }
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

