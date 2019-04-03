#### normalize_data ####
normalize_data <-
  function(data,
                           normalize_factor = 65535){

  data.intensity_colnames <- grepl("Intensity", colnames(data)) &
    !grepl("Location", colnames(data))
  data[,data.intensity_colnames] <- data[,data.intensity_colnames]*normalize_factor
  return(list(data = data))
}

#### irf1 ####

folder.path <- "/media/knt/1624BB0024BAE1C31/KN/IFNsignalling/resources/"

experiment.filename <- "488-546/raw/data_quantify/CellsFiltered488.csv"


irf.filtering.fun <-
  function(data,
           ...){
    q.max <- quantile(x = data$Intensity_MaxIntensity_Alexa546, probs = .99)
    q.min <- quantile(x = data$Intensity_MaxIntensity_Alexa546, probs = .01)
    q.maf <- quantile(x = data$Intensity_MADIntensity_Alexa546/data$Intensity_MeanIntensity_Alexa546, probs = .99, na.rm = TRUE)
    return(
      data %>%
        dplyr::filter(Intensity_MaxIntensity_Alexa546 < q.max) %>%
        dplyr::filter(Intensity_MaxIntensity_Alexa546  > q.min) %>%
        dplyr::filter(Intensity_MADIntensity_Alexa546/Intensity_MeanIntensity_Alexa546  < q.maf))
  }
protein <- "irf1"
output.path <- paste(folder.path, "output", protein, "itrc", sep = "/")
dir.create(output.path, recursive = TRUE)
data.folder.path <- "/media/knt/1624BB0024BAE1C31/KN/IFNsignalling/resources/data-fish/"
experiments.list <- c(
  "2018-10-18-KZ-FISH17-2",
  "2018-10-25-KZ-FISH19-2",
  "2018-11-08-KZ-FISH22"
)
priming.list <- c("non-priming", "priming")
versions.list <- expand.grid(exp = experiments.list,
                             priming = priming.list,
                             stringsAsFactors = FALSE)


data.list.raw <- list()
data.list.raw[[experiments.list[[1]]]] <-
  read.table(
    file =
      paste(data.folder.path,
            experiments.list[[1]],
            experiment.filename,
            sep = "/"),
    sep = ",",
    header = TRUE
  ) %>%
  dplyr::filter(id != "B07") %>%
  irf.filtering.fun(data = .)  %>%
  data.table::data.table()


data.list.raw[[experiments.list[[2]]]] <-
  read.table(
    file =
      paste(data.folder.path,
            experiments.list[[2]],
            experiment.filename,
            sep = "/"),
    sep = ",",
    header = TRUE
  )  %>%
  irf.filtering.fun()  %>%
  data.table::data.table()

data.list.raw[[experiments.list[[3]]]] <-
  read.table(
    file =
      paste(data.folder.path,
            experiments.list[[3]],
            experiment.filename,
            sep = "/"),
    sep = ",",
    header = TRUE
  ) %>%
  irf.filtering.fun()  %>%
  data.table::data.table()

stim.list.rcc <- c(0,
                   0.05,
                   0.25,
                   5)
output.folder.path <- paste(folder.path, "output",  protein, "experiments/", sep = "/")
#### ####
signal_ <- "Stim"
sample_ <- "CellID2"
response_ <- "intensity"
parallel_cores <- 8
bootstrap <- TRUE
bootstrap.number <- 250
bootstrap.sample_size <- 100
rescale.fun <- function(x){log(x = x, base = 5)}
signal.list <- c(0,0.05,0.25,5)
g.list <- foreach(experiment_ = experiments.list) %dopar% {
  # experiment.path <- paste(output.path, experiment_, sep = "/")
  # dir.create(experiment.path, recursive = TRUE)
  (data.list.raw[[experiment_]] %>%
     data.frame() %>%
     normalize_data(
       data = .))[["data"]] %>%
    data.table::data.table() %>%
    dplyr::mutate(
      exp.id = experiment.id,
      well.name = id,
      priming = priming.1.1,
      Stim = stimulation.1.1,
      Time = time.1.1,
      intensity = Intensity_MeanIntensity_Alexa546,
      CellID2 = paste(experiment.id, id, ObjectNumber, sep = "_")
    ) %>%
    dplyr::select(
      exp.id,
      well.name,
      priming,
      Stim,
      Time,
      CellID2,
      intensity
    ) %>%
    dplyr::filter(Stim %in% signal.list) ->
    data.fish

  model.priming <-
    ITRC(data = data.fish %>% dplyr::filter(priming == 1000),
         signal = signal_,
         sample = sample_,
         response = response_,
         parallel_cores = parallel_cores,
         bootstrap.number = bootstrap.number,
         bootstrap.sample_size = bootstrap.sample_size,
         bootstrap = bootstrap)

  g.priming <-
    plotITRCWaves(
      model = model.priming,
      rescale.fun = rescale.fun,
      fill.guide_ = FALSE,
      title_ = paste("IFNB 1000 U/ml",
                     "itrc =",
            round((model.priming$itrc %>%
                     dplyr::filter(Stim == max(Stim)))[["itrc"]], digits = 2)),
      ylimits_ = c(0,3)
    )

  saveRDS(file =
            paste(output.path,
                  paste(experiment_, "model_priming.RDS", sep = "_"),
                  sep = "/"),
          object = model.priming)

  model.nonpriming <-
    ITRC(data = data.fish %>% dplyr::filter(priming == 0),
         signal = signal_,
         sample = sample_,
         response = response_,
         parallel_cores = parallel_cores,
         bootstrap.number = bootstrap.number,
         bootstrap.sample_size = bootstrap.sample_size,
         bootstrap = bootstrap)

  g.nonpriming <-
    plotITRCWaves(
      model = model.nonpriming,
      rescale.fun = rescale.fun,
      fill.guide_ = FALSE,
      title_ = paste("IFNB 0 U/ml", "itrc =",
                     round((model.nonpriming$itrc %>%
                              dplyr::filter(Stim == max(Stim)))[["itrc"]], digits = 2)),
      ylimits_ = c(0,3)
    )

  saveRDS(file =
            paste(output.path,
                  paste(experiment_, "model_nonpriming.RDS", sep = "_"),
                  sep = "/"),
          object = model.nonpriming)
  g <- cowplot::plot_grid(g.nonpriming, g.priming, nrow = 1, ncol = 2)
  ggplot2::ggsave(
    filename = paste(output.path,
                     paste(experiment_, "itrc.pdf", sep = "_"),
                     sep = "/"),
    plot = g,
    width = 8, height = 4)
  return(g)
}


g <- cowplot::plot_grid(plotlist = g.list,
                   nrow = 3, ncol = 1, labels = experiments.list)
ggplot2::ggsave(
  filename = paste(output.path,
                   paste("all", "itrc.pdf", sep = "_"),
                   sep = "/"),
  plot = g,
  width = 8, height = 12)


#### ####

data.list.raw[["merged"]] <-
  rbind(data.list.raw$`2018-10-25-KZ-FISH19-2`,
        data.list.raw$`2018-11-08-KZ-FISH22`)
experiments.list <- "merged"
