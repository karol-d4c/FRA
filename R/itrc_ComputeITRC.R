ComputeITRC <-
  function(model,
           parallel_cores,
           cc_maxit = 100,
           lr_maxit = 1000,
           MaxNWts = 5000,
           ...
  ) {

    compuatations.task.list <-
      GetComputationsTasks(
        signal.list = (model$data %>%
                         dplyr::arrange_(model$signal) %>%
                         dplyr::distinct_(model$signal))[[model$signal]],
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

          df.res.newdata$data$class <- lr.fit

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
             counts.sum = sum(counts)))) %>%
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

    model$confusion.table %>%
      dplyr::left_join(
        (model$confusion.table %>%
           dplyr::group_by_(
             model$signal,
             "max.signal",
             "bootstrap") %>%
           dplyr::summarise(
             counts.sum = sum(counts)))) %>%
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

    return(model)
  }
