#' Compute standard Response Curve
#'
#' @description  This function calculate Information Theroetic Response Curves.
#' @param model ITRC S3 object that could be obtain from ITRC function
#' @param data A data.frame or data.table object in a wide format that describe
#'  response (might be multidimmensional) of the samples to the signal
#'  (now only one dimmensional). Data.frame \code{data} consists columns of names
#'  defined by \code{sample}, \code{signal} (optional), and \code{response}.
#'   Each row represents a response of one sample to the input signal.
#'  Column \code{signal} define the input signal (now only in one-dimmension).
#'  Columns \code{response} define the multidimmensional (optional) response to
#'   the input signal.
#'  Column \code{sample} specify identifaction of sample. If \code{sample} is not
#'   defined then sample is identified by row number.
#' @param signal character, specify name of the column that represents the input signal
#' @param response vector of chartacters, that specify names of the columns that represents
#' the output response
#' @param sample character (optional), specify name of the column that consists identifiaction of
#' sample
#' @param bootstrap \code{default = FALSE}
#' @param bootstrap.number \code{default = 1}
#' @param bootstrap.sample_size \code{default = NULL}
#'
#' @return FRA S3 object with
#'
#' @export
RC <-
  function(
    model = NULL,
    data,
    signal = "signal",
    response = "response",
    sample = "sample",
    bootstrap.number = 0,
    bootstrap.sample_size = NULL,
    parallel_cores = 1,
    ...
  ){

    if(is.null(model)){
      model <-
        new_FRAModel(
          data = data,
          signal = signal,
          response = response,
          sample = sample)

      if(!is.numeric(bootstrap.number)){
        stop("Bootstrap number must be numeric")
      }
      bootstrap.number <- round(bootstrap.number)

      if(bootstrap.number > 0){
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
    }
    #### TODO
    model <-
      ComputeRC(
        model = model,
        parallel_cores =
          parallel_cores,
        ...)
    return(model)
  }
