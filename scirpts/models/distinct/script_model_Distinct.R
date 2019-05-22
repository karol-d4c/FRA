### ###
### Distinct model
### ###
#### Library ####
source("scirpts/models/model_Manuscript1D.R")
library(data.table)
library(dplyr)
library(ITRC)

#### Model initialisation ####
model.name <- "manuscript1d"
output.path <- "/media/knt/sdb2/KN/ITRC/resources/model/distinct/2019-04-12/"
#output.path <- "resources/model/distinct/testing/"

dir.create(path = output.path, recursive = TRUE)
#### model parameters ####

stim.list <- c(0, 0.1, 1, 10, 100, 1000, 10000)
stim.list.rcc <- stim.list
alpha_ = 7.5*10
beta_ = 0
gamma_ = 0.75
delta_ = 5

data.model <-
  sample_ModelManuscript1D(
    output.path = output.path,
    time = 1,
    n = 100000,
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
parallel_cores = 8
bootstrap = TRUE
bootstrap.number = 64
bootstrap.sample_size = 1000
path.model <- paste(output.path, "model.rds", sep = "/")
if(file.exists(path.model)){
  model <- readRDS(path.model)
}
if(is.null(model) | force.run){
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
    bootstrap.test.sample = TRUE,
    bootstrap.test.number = 4
  )
saveRDS(object = model,
        file = paste(output.path, "model.rds", sep = "/")
)
}

theme.signal <-
  GetRescaledSignalTheme(
    model = model,
    rescale.fun = function(x){log(x = x, base = 10)})

theme.signal.reduced <- theme.signal
theme.signal$signals.rescale.df %>%
  dplyr::filter(Stim <= 1) ->
  theme.signal.reduced$signals.rescale.df

ylim_ <- c(0,4)
g.model <-
  ITRC::plotITRCWaves(
    model = model,
    theme.signal = theme.signal.reduced,
    confusion.signal.max = 1,
    ylimits_ = ylim_)

ggplot2::ggsave(
  filename = paste(output.path, "model_overlapping_itrc.pdf", sep = "/"),
  plot = g.model,
  width = 6,
  height = 3,
  useDingbats = FALSE
)

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

g.xlim <- c(0, 50)
g.ylim <- c(0, 0.6)
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
    values = theme.signal$colors) +
  ggplot2::coord_cartesian(xlim = g.xlim, ylim = g.ylim)
g

ggplot2::ggsave(
  filename = paste(output.path, "model_overlapping.pdf", sep = "/"),
  plot = g,
  width = 6,
  height = 3,
  useDingbats = FALSE
)

