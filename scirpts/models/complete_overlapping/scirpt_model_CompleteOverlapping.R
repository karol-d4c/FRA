### ###
### Overlapping model
### ###
#### Library ####
source("scirpts/models/overlapping/model_Manuscript1D.R")
library(data.table)
library(dplyr)
library(future)
library(ITRC)
#### Model initialisation ####
model.name <- "manuscript1d"
output.path <- "/media/knt/sdb2/KN/ITRC/resources/model/complete_overlapping/2019-04-11-2/"
output.path <- "resources/model/complete_overlapping/2019-04-11-2/"

dir.create(path = output.path, recursive = TRUE)
ITRC.DEBUG <- TRUE
#### model parameters ####

stim.list <- c(0, 0.1, 1, 10, 100, 1000, 10000)
stim.list.rcc <- stim.list
alpha_ = 0
beta_ = 0
gamma_ = 0.75
delta_ = 5

data.model <-
  sample_ModelManuscript1D(
    output.path = output.path,
    time = 1,
    n = 50000,
    stim.list = stim.list.rcc,
    alpha = alpha_,
    beta = beta_,
    gamma = gamma_
  )
g <-
  ggplot2::ggplot(
    data = data.model %>%
      dplyr::filter(Stim %in%
                      stim.list.rcc),
    ggplot2::aes(
      x = intensity,
      group = Stim,
      fill = factor(Stim),
      color = factor(Stim))
  ) +
  ggplot2::geom_density(alpha = 0.5) +
  ITRC::theme_itrc()
g

signal <- "Stim"
sample <- "CellID2"
response <- "intensity"
parallel_cores = 2
bootstrap = TRUE
bootstrap.number = 8
bootstrap.sample_size = 1000
path.model <- paste(output.path, "model.rds", sep = "/")
bootstrap.test.sample <- TRUE
if(file.exists(path.model)){
  model <- readRDS(path.model)
}
force.run <- TRUE
if(is.null(model) | force.run){
  #plan(multiprocess)
  model <-
    ITRC(
      data = data.model,
      signal = signal,
      sample = sample,
      response = response,
      parallel_cores = parallel_cores,
      bootstrap.number = bootstrap.number,
      bootstrap = bootstrap,
      bootstrap.sample_size = bootstrap.sample_size,
      ITRC.DEBUG = ITRC.DEBUG,
      bootstrap.test.sample = bootstrap.test.sample
    )

  saveRDS(object = model,
          file = paste(output.path, "model.rds", sep = "/")
  )
}

#verify
sapply(1:6, function(i){sum(model$confusion.matrix[i,])})

theme.signal <-
  GetRescaledSignalTheme(
    model = model,
    rescale.fun = function(x){log(x = x, base = 10)})
theme.signal.reduced <- theme.signal
theme.signal.reduced$signals.rescale.df %>%
  dplyr::filter(Stim <= 1) ->
  theme.signal.reduced$signals.rescale.df

g.model <-
  ITRC::plotITRCWaves(
    model = model,
    theme.signal = theme.signal.reduced,
    confusion.signal.max = 1)

ggplot2::ggsave(
  filename = paste(output.path, "model_overlapping_itrc.pdf", sep = "/"),
  plot = g.model,
  width = 8,
  height = 6,
  useDingbats = FALSE
)
theme.signal <-
  GetRescaledSignalTheme(
    model = model,
    rescale.fun = function(x){log(x = x, base = 10)})

g.model.comparison <-
  ITRC::plotITRCWaves.Comparison(
    model = model,
    data = model$rc.sum,
    variable.to.compare = "intensity",
    theme.signal = theme.signal,
    data_raw_min = 5
    #signal.max = 1
  )

ggplot2::ggsave(
  filename = paste(output.path, "model_overlapping_itrccompare.pdf", sep = "/"),
  plot = g.model.comparison,
  width = 8,
  height = 6,
  useDingbats = FALSE
)

stim.list.rcc <- c(0,0.1,1)
g <-
  ggplot2::ggplot(
    data = model$data %>%
      dplyr::filter(Stim %in%
                      stim.list.rcc) %>%
      dplyr::left_join(theme.signal$signals.rescale.df),
    ggplot2::aes(
      x = intensity,
      group = Stim,
      fill = factor(Stim),
      color = factor(Stim))
  ) +
  ggplot2::geom_density(alpha = 0.5) +
  ITRC::theme_itrc() +
  ggplot2::scale_fill_manual(
    guide = FALSE,
    name = "Stimulation level",
    values = theme.signal$colors) +
  ggplot2::scale_color_manual(
    guide = FALSE,
    name = "Stimulation level",
    values = theme.signal$colors)
g

ggplot2::ggsave(
  filename = paste(output.path, "model_overlapping.pdf", sep = "/"),
  plot = g,
  width = 8,
  height = 6,
  useDingbats = FALSE
)
