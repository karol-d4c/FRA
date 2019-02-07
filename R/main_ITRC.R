#' Compute Information-Theoretic Response Curve
#'
#' @description  This function calculate Information Theroetic Response Curves.
#'
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
#' @return ITRC S3 object with
#'
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
      ComputeITRC(
        model = model,
        parallel_cores =
          parallel_cores,
        ...)

    return(model)

  }
