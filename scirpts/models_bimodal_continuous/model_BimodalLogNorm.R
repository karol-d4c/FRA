
#### log-normal distribution - increading mean -> increasing sd ####
sample_ModelBimodalLogNorm <-
  function(
    output.path = "resources/output/model/1d/continuous_logmvn/",
    time = 1,
    n = 10000,
    stims.num = 65,
    stim.list = seq(from = 1, length.out = stims.num, by =1),
    #twosignals close
    fun.distr.params =
      function(
        n.modes = 2,
        ...){
        mean.signal <- seq(from = 20, to = 40, length.out =  n.modes)#/2
        sd.signal <- (mean.signal - min(mean.signal))*0.45 + 5

        # mean.signal <- seq(from = 20, to = 60, length.out =  n.modes)#/2
        # sd.signal <- mean.signal*0 + 5#(mean.signal - min(mean.signal))*0.45 + 5


        #sd.signal <- (mean.signal - min(mean.signal))*0 + 2.5
        #mean.signal <- 2*mean.signal
        return(
          list( mean.signal = mean.signal,
                sd.signal = sd.signal))
      },
    # #twosignals imposed
    # fun.distr.params =
    #   function(
    #     n.modes = 2,
    #     ...){
    #     mean.signal <- seq(from = 20, to = 100, length.out =  n.modes)#/2
    #     sd.signal <- (mean.signal - min(mean.signal))*0.45 + 5
    #     #mean.signal <- 2*mean.signal
    #     return(
    #       list( mean.signal = mean.signal,
    #             sd.signal = sd.signal))
    #   },
    # twosignals distinct
    # fun.distr.params =
    #   function(
    #     n.modes = 2,
    #     ...){
    #     mean.signal <- seq(from = 20, to = 100, length.out =  n.modes)/2
    #     sd.signal <- (mean.signal - min(mean.signal))*0.45 + 5
    #     mean.signal <- 2*mean.signal
    #     return(
    #       list( mean.signal = mean.signal,
    #             sd.signal = sd.signal))
    #   },
    # #distinct function
    # fun.distr.params =  function(
    #   n.modes = 2,
    #   ...){
    #   mean.signal <- seq(from = 20, to = 100, length.out = n.modes)/2
    #   sd.signal <- (mean.signal - min(mean.signal))*0.1 + 2.5
    #   mean.signal <- 2*mean.signal
    #   return(
    #     list( mean.signal = mean.signal,
    #           sd.signal = sd.signal))
    # },
    ...)
  {
    n.modes <- 2
    distr.params <- fun.distr.params(n.modes = n.modes)
    p.list <- seq(from = 0, to = 100, length.out = stims.num)

    data.raw.list <- foreach::foreach(i = 1:stims.num) %do% {
      p <- p.list[i]
      signal <- stim.list[i]
      n.max <- round(n*p/100)
      Y.min <- rnorm(n = n - n.max,
                     mean = distr.params$mean.signal[1],
                     sd =  distr.params$sd.signal[1])
      Y.max <- rnorm(n = n.max,
                     mean = distr.params$mean.signal[2],
                     sd =  distr.params$sd.signal[2])
      return(data.frame(intensity = c(Y.min, Y.max),
                        Stim = signal,
                        Time = time,
                        CellID2  = paste(signal, 1:n, sep = "_")))
    }
    data.raw <- do.call(what = rbind, args = data.raw.list)
    stim.list <- (data.raw %>% dplyr::distinct(Stim))[["Stim"]]
    signal.list <- paste("X", stim.list, sep = "")

    data.raw$signal <- paste("X", data.raw$Stim, sep = "")
    data.raw$signal <- factor(
      x = data.raw$signal,
      levels = signal.list)

    return(data.raw)
  }

