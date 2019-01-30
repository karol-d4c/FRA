#' Constructor of ITRCModel S3 object
#'
#' @param data A data.frame or data.table object with columns \code{signal}, \code{response} and \code{sample} in a wide format
#' @param signal
#' @param response
#' @param sample
#'
#' @return ITRCModel S3 object with
new_ITRCModel <-
  function(
    data,
    signal,
    response,
    sample
  ){
    if(!exists(x = "data")){
      stop("data not defined")
    } else {
      if(!("data.frame" %in% class(data))){
        stop("data should be data.frame or data.table")
      }
    }
    if(!exists(x = "signal")){
      stop("object signal not defined")
    } else {
      if(!(as.character(signal) %in% colnames(data))){
        stop(paste("data has not column named", signal))
      }
    }
    if(!exists(x = "response")){
      stop("data not defined")
    } else {
      if(!(all(as.character(response) %in% colnames(data)))){
        stop(paste("data has not column named",
                   paste(response, collapse = " ")))
      }
    }
    if(!exists(x = "sample")){
      data[["sample"]] <- 1:nrow(data)
      sample <- sample
    }
    if(!(as.character(sample) %in% colnames(data))){
      stop(paste("data has not column named", sample))
    }
    model <-
      list(data =
             data %>%
             dplyr::arrange_(sample, signal) %>%
             data.table::data.table(),
           signal = signal,
           class  = "class",
           response = response,
           sample = sample
      )
    class(model) <- "ITRCModel"
    return(model)
  }
