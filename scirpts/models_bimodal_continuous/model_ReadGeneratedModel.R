#### model_ReadGeneratedModel ####
model_ReadGeneratedModel <-
  function(
    output.path,
    model.name = "",
    model.light = FALSE,
    ...){
    res <- NULL
    data.raw.path <- getModelPath(output.path)
    if(file.exists(data.raw.path)){
      res <- readRDS(
        file = data.raw.path)
      if(model.light){
        res <-
          getModelLight(
            model = res,
            ...)
      }
      if(is.null(res[["model.name"]])){
        res[["model.name"]] <- model.name
        saveRDS(
          object = res,
          file = data.raw.path
        )
      }
    }
    return(res)
  }
