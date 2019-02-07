ComputeITRC <-
  function(model,
           parallel_cores,
           cc_maxit = 100,
           lr_maxit = 1000,
           MaxNWts = 5000,
           ...
  ) {
    signal.list <- (model$data %>%
                     dplyr::arrange_(model$signal) %>%
                     dplyr::distinct_(model$signal))[[model$signal]]

    compuatations.task.list <-
      GetComputationsTasks(
        signal.list = signal.list,
        bootstrap.samples = model$bootstrap.samples)

    if(length(compuatations.task.list) < 1){
      stop("ITRC can be computed if number of signals is higher than 1")
    }

    doParallel::registerDoParallel(parallel_cores)
    foreach::foreach(
      computation.task =
        compuatations.task.list) %dopar% {
          df.res <-
            GetLogRegParameters(
              data =
                returnBootstrapData(
                  model = model,
                  bootstrap_ = computation.task$bootstrap),
              model = model,
              signal.list = computation.task$signal)

          lr_model <- nnet::multinom(formula = df.res$formula_string,
                                     data = df.res$data,
                                     na.action = na.omit,
                                     maxit = lr_maxit,
                                     MaxNWts = MaxNWts)#,  model = FALSE )
          attr(x =  lr_model$terms, ".Environment") <- NULL
          df.res.newdata <-
            GetLogRegParameters(
              data =
                returnBootstrapData(
                  model = model,
                  bootstrap_ =
                    model$bootstrap.samples[
                      -which(computation.task$bootstrap ==
                               computation.task$bootstrap)]),
              model = model,
              signal.list = computation.task$signal)

          lr.fit <-
            predict(object  = lr_model,
                    newdata = df.res.newdata$data)

          df.res.newdata$data$class <- as.numeric(as.character(lr.fit))

          df.res.newdata$data %>%
            dplyr::select_(paste("c(",
                                 df.res.newdata$signal,
                                 ",",
                                 model$class,
                                 ")")) %>%
            dplyr::group_by_(
              df.res.newdata$signal,
              model$class) %>%
            dplyr::summarise(counts = n()) %>%
            dplyr::rename_(
              .dots = setNames(nm = model$signal,
                               object = df.res$signal)) ->
            df.confusion

          df.confusion$max.signal <-
            computation.task$max.signal
          df.confusion$bootstrap <-
            computation.task$bootstrap

          return(df.confusion)
        } %>%
      do.call(
        what = rbind,
        args = .
      ) ->
      model$confusion.table
    doParallel::registerDoParallel(parallel_cores)


    model$confusion.table %>%
      dplyr::left_join(
        (model$confusion.table %>%
           dplyr::group_by_(
             model$signal,
             "max.signal",
              "bootstrap") %>%
           dplyr::summarise(
             counts.sum = sum(counts))),
        by = c(model$signal,
               "max.signal",
               "bootstrap" )) %>%
      dplyr::group_by_(
        model$signal,
        model$class,
        "max.signal",
        "counts.sum"
      ) %>%
      dplyr::summarise(
        prob = mean(counts/counts.sum),
        prob.sd = sd(counts/counts.sum)
      ) %>%
      dplyr::select_(.dots= setdiff(names(.), "counts.sum")) %>%
      dplyr::ungroup()  ->
      model$confusion.matrix

    model$confusion.matrix[1,] %>%
      dplyr::mutate_all(.funs = function(x){0}) %>%
      dplyr::mutate(prob = 1) %>%
      rbind(model$confusion.matrix) ->
      model$confusion.matrix

    signal_class.df <-
      expand.grid(signal.list,
                  signal.list,
                  signal.list)
    colnames(signal_class.df)  <- c(model$signal,
                                    model$class,
                                    "max.signal")
    signal_class.df %>%
      dplyr::filter_(paste(model$signal, "<=", "max.signal")) %>%
      dplyr::filter_(paste(model$class, "<=", "max.signal")) ->
      signal_class.df
    signal_class.df$inner_join_id_ <- 1:nrow(signal_class.df)

    signal_class.df %>%
      dplyr::inner_join(model$confusion.matrix,
                        by = c(model$signal,
                               model$class,
                               "max.signal")) ->
      signal_class.inner_join.df

    signal_class.df[
      signal_class.df$inner_join_id_[
        -which(signal_class.df$inner_join_id_ %in%
                 signal_class.inner_join.df$inner_join_id_)],] ->
      signal_class.df

    model$confusion.matrix %>%
      rbind(
        (signal_class.df %>%
           dplyr::select_(
             paste("c(",
                   model$signal, ",",
                   model$class, ",",
                   "max.signal)")
           ) %>%
           dplyr::left_join(
             y = model$confusion.matrix %>%
               dplyr::group_by_("max.signal") %>%
               dplyr::summarise(prob = 0, prob.sd = 0),
             by = "max.signal"
           ))) ->
      model$confusion.matrix

    model$confusion.table %>%
      dplyr::left_join(
        (model$confusion.table %>%
           dplyr::group_by_(
             model$signal,
             "max.signal",
             "bootstrap") %>%
           dplyr::summarise(
             counts.sum = sum(counts))),
        by = c(model$signal,
               "max.signal",
               "bootstrap")) %>%
      dplyr::filter_(
        paste(model$signal,
              "==",
              model$class)
      ) %>%
      dplyr::mutate(
        prob = counts/counts.sum
      ) %>%
      dplyr::select_(
        .dots = setdiff(names(.),
                        c(model$class,
                          "counts",
                          "counts.sum")))  %>%
      dplyr::ungroup() ->
      model$itrc

    foreach::foreach(itrc.i = 1:nrow(model$itrc)) %do% {
      itrc_ <- model$itrc[itrc.i,]
      prob_ <-
        (model$itrc %>%
           dplyr::filter(
             max.signal == itrc_[["max.signal"]],
             bootstrap == itrc_[["bootstrap"]]) %>%
           dplyr::filter_(
             paste(model$signal,
                   "<=",
                   itrc_[[model$signal]])
           ) %>%
           dplyr::summarise(prob = sum(prob)))[["prob"]]
    itrc_ %>%
      dplyr::mutate(prob = prob_)
    }  %>%
      do.call(what = rbind,
              args = .) %>%
      dplyr::group_by_(
        model$signal,
        "max.signal"
      ) %>%
      dplyr::summarise(
        prob.mean = mean(prob),
        prob.sd = sd(prob)
      ) %>%
      dplyr::arrange_("max.signal",
                      model$signal) ->
    model$itrc

    model$itrc[1,] %>%
      dplyr::mutate_all(.funs = function(x){0}) %>%
      dplyr::mutate(prob.mean = 1) %>%
      rbind(model$itrc) %>%
      dplyr::ungroup() ->
      model$itrc

    model$itrc %>%
      dplyr::group_by_(
        model$signal
      ) %>%
      dplyr::summarise(
        itrc = max(prob.mean)
      ) ->
      model$itrc

    model$confusion.matrix %>%
      dplyr::arrange(as.numeric(class)) %>%
      dplyr::arrange_(paste("as.numeric(", model$signal, ")")) %>%
      dplyr::filter(max.signal == max(max.signal)) %>%
      reshape2::dcast(
        formula = paste( model$signal, "~", model$class),
        value.var = "prob") ->
      confusion.matrix
    confusion.matrix[which(is.na(confusion.matrix), arr.ind = TRUE)] <- 0
    signals <- confusion.matrix[,1]
    confusion.matrix <- confusion.matrix[,-1]
    confusion.matrix <-
      confusion.matrix[
        order(as.numeric(signals)),
        order(as.numeric(colnames(confusion.matrix)))]
    rownames(confusion.matrix) <-
      signals[order(as.numeric(signals))]
    model$confusion.matrix.wide <-
      confusion.matrix
    return(model)
  }
