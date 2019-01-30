#' Compute Information-Theoretic Response Curve
#'
#' @param data A data.frame or data.table object with columns \code{signal}, \code{response} and \code{sample} in a wide format
#' @param signal
#' @param response
#' @param sample
#' @param bootstrap \code{default = FALSE}
#' @param bootstrap.number \code{default = 1}
#' @param bootstrap.sample_size \code{default = NULL}
#'
#' @return ITRC S3 object with
#' @export
ITRC <-
  function(
    data,
    signal = "signal",
    response = "response",
    sample = "sample",
    bootstrap = FALSE,
    bootstrap.number = 2,
    bootstrap.sample_size = NULL,
    parallel_cores = 1,
    ...
  ){

    model <-
      new_ITRCModel(
        data = data,
        signal = signal,
        response = response,
        sample = sample)

    if(bootstrap){
      model <-
        GenerateBootstrapSample(
          model = model,
          bootstrap.number = bootstrap.number,
          bootstrap.sample_size = bootstrap.sample_size,
          parallel_cores = parallel_cores
        )
    } else {
      model <-
        GenerateNoBootstrapSample(
          model = model
        )
    }


    #### TODO
    model <-
      ComputeBootstrapITRC(
        model = model,
        parallel_cores = parallel_cores)

    return(model)

}



