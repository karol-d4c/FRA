returnBootstrapData <-
  function(
    model,
    bootstrap_,
    ...
  ){
    model$data %>%
      dplyr::right_join(
        (model$bootstrap.samples.df %>%
           dplyr::filter(bootstrap %in% bootstrap_)),
        by = model$sample) %>%
      data.table::data.table() %>%
      return()
  }
