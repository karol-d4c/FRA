CalculateConfusionMatrix <-
  function(
    model,
    cols.list,
    ...
  ){
    max.signal.list <-
      (model$confusion.table %>%
         dplyr::distinct_(cols.list$max.signal) %>%
         dplyr::arrange_(cols.list$max.signal)
      )[[cols.list$max.signal]]
    model$confusion.matrix.list <-
      foreach(
        max.signal_ =
          max.signal.list
      ) %do% {
        model$confusion.table %>%
          dplyr::arrange_(paste("as.numeric(", model$class, ")"))  %>%
          dplyr::arrange_(paste("as.numeric(", model$signal, ")")) %>%
          dplyr::filter_(paste(cols.list$max.signal,
                               "==",
                               max.signal_
          )) %>%
          reshape2::dcast(
            formula = paste( model$signal, "~", model$class),
            value.var = "prob") ->
          confusion.matrix
        confusion.matrix[which(is.na(confusion.matrix), arr.ind = TRUE)] <- 0
        signals <- confusion.matrix[,1]
        confusion.matrix <- confusion.matrix[,-1]
        if(is.null(dim(confusion.matrix))){
          names(confusion.matrix) <- signals
        } else {
          confusion.matrix <-
            confusion.matrix[
              order(as.numeric(signals)),
              order(as.numeric(colnames(confusion.matrix)))]
          rownames(confusion.matrix) <-
            signals[order(as.numeric(signals))]
        }
        return(confusion.matrix)
      }
    names(model$confusion.matrix.list) <- as.character(max.signal.list)
    model$confusion.matrix <-
      model$confusion.matrix.list[[as.character(max(max.signal.list))]]
    return(model)
  }
