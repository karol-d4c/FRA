CalculateConfusionTable <-
  function(
    model,
    cols.list,
    signal.list,
    ...
  ){
    (model$specifictiy.bootstrap.table %>%
       dplyr::group_by_(
         model$signal,
         "max.signal",
         "bootstrap",
         cols.list$computation.task) %>%
       dplyr::summarise(
         counts.sum = sum(counts)))

    model$specifictiy.bootstrap.table %>%
      dplyr::left_join(
        (model$specifictiy.bootstrap.table %>%
           dplyr::group_by_(
             model$signal,
             cols.list$max.signal,
             cols.list$bootstrap,
             cols.list$computation.task) %>%
           dplyr::summarise(
             counts.sum = sum(counts)
             # .dots =
             #   setNames(
             #     nm = cols.list$counts.sum,
             #     object = paste("sum(", cols.list$counts, ")")
             #   )
             )),
        by = c(model$signal,
               cols.list$max.signal,
               cols.list$bootstrap,
               cols.list$computation.task)) %>%
      dplyr::mutate(
        prob.bootstrap = counts/counts.sum
      ) ->
      model$confusion.bootstrap.table


    model$confusion.bootstrap.table %>%
      dplyr::group_by_(
        model$signal,
        model$class,
        cols.list$max.signal
      ) %>%
      dplyr::summarise(
        prob = mean(prob.bootstrap),
        prob.sd = sd(prob.bootstrap)
      )  %>%
      dplyr::ungroup()  ->
      model$confusion.table

    ## add max.signal == 0
    model$confusion.table[1,] %>%
      dplyr::mutate_all(.funs = function(x){0}) %>%
      dplyr::mutate(prob = 1) %>%
      rbind(
        x = .,
        y = model$confusion.table) ->
      model$confusion.table

    ### add not existing points
    signal_class.df <-
      expand.grid(signal.list,
                  signal.list,
                  signal.list)
    colnames(signal_class.df)  <-
      c(model$signal,
        model$class,
        cols.list$max.signal)
    signal_class.df %>%
      dplyr::filter_(paste(model$signal, "<=", "max.signal")) %>%
      dplyr::filter_(paste(model$class, "<=", "max.signal")) ->
      signal_class.df
    signal_class.df$inner_join_id_ <-
      1:nrow(signal_class.df)

    signal_class.df %>%
      dplyr::inner_join(
        model$confusion.table,
        by = c(model$signal,
               model$class,
               cols.list$max.signal)) ->
      signal_class.inner_join.df

    signal_class.df[
      signal_class.df$inner_join_id_[
        -which(signal_class.df$inner_join_id_ %in%
                 signal_class.inner_join.df$inner_join_id_)],] ->
      signal_class.df

    model$confusion.table %>%
      rbind(
        (signal_class.df %>%
           dplyr::select_(
             paste("c(",
                   paste(model$signal,
                         model$class,
                         cols.list$max.signal,
                         sep = ","),
                   ")")
           ) %>%
           dplyr::left_join(
             y = model$confusion.table %>%
               dplyr::group_by_(cols.list$max.signal) %>%
               dplyr::summarise(prob = 0, prob.sd = 0),
             by = cols.list$max.signal
           ))) ->
      model$confusion.table
    return(model)
  }
