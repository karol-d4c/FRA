head(ITRC::data.itrc.cytof)

model.full <-
  ITRC::ITRC(
    data = ITRC::data.itrc.cytof %>%
      dplyr::filter(Stim != 50),# %>%  dplyr::filter(Stim <= 250),
    signal = "Stim",
    response = c("pSTAT1", "pSTAT3", "pSTAT4", "pSTAT5", "pSTAT6"),
    parallel_cores = 2,
    bootstrap.number = 10
  )

signals.rescale.df <- rescaleSignalsValues.DataFrame(
    model = model.full,
    col.to.rescale = model.full$signal,
    col.rescaled   = "signal_rescaled")
model.list <-list()
g.list <- list()
library(foreach)
library(dplyr)
foreach::foreach(Stim_ = as.numeric(signals.rescale.df$Stim[-1])) %do% {
model.list[[as.character(Stim_)]] <-
  ITRC::ITRC(
    data = ITRC::data.itrc.cytof %>%
      dplyr::filter(Stim != 50) %>%
      dplyr::filter(Stim <= Stim_),
    signal = "Stim",
    response = c("pSTAT1", "pSTAT3", "pSTAT4", "pSTAT5", "pSTAT6"),
    parallel_cores = 2,
    bootstrap.number = 100
  )

g.list[[as.character(Stim_)]] <-
  ITRC::plotITRCWaves(model = model.list[[as.character(Stim_)]],
              signals.rescale.df = signals.rescale.df,
              ylimits_ = c(0,4))
return()
} -> l
xlab_ <- "X"


g <-
  cowplot::plot_grid(
  plotlist = g.list,
  ncol = 1,
  rel_heights = c(1,1,1,1))

path <- "~/Documents/Projects/CC/notes/2019-02-20-ITRC/resources/CYTOF/IFNa2a/"
width_ <- 6
height_ <- 4*4
ggplot2::ggsave(filename = paste(path, "itrc_bcells.pdf", sep= "/"),
                plot = g,
                height = height_,
                width  = width_)

model.list.pS1 <- list()
model.list.pS1_no <- list()

model.list.pS3_no <- list()
model.list.pS1pS3_no <- list()
model.list.pS1pS3 <- list()

model.list.pS1[[as.character(Stim_)]] <-
  ITRC::ITRC(
    data = ITRC::data.itrc.cytof %>%
      dplyr::filter(Stim != 50) %>%
      dplyr::filter(Stim <= Stim_),
    signal = "Stim",
    response = c("pSTAT1"),
    parallel_cores = 2,
    bootstrap.number = 100
  )
model.list.pS1_no[[as.character(Stim_)]] <-
  ITRC::ITRC(
    data = ITRC::data.itrc.cytof %>%
      dplyr::filter(Stim != 50) %>%
      dplyr::filter(Stim <= Stim_),
    signal = "Stim",
    response = c("pSTAT3", "pSTAT4", "pSTAT5", "pSTAT6"),
    parallel_cores = 2,
    bootstrap.number = 100
  )

model.list.pS3_no[[as.character(Stim_)]] <-
  ITRC::ITRC(
    data = ITRC::data.itrc.cytof %>%
      dplyr::filter(Stim != 50) %>%
      dplyr::filter(Stim <= Stim_),
    signal = "Stim",
    response = c("pSTAT1", "pSTAT4", "pSTAT5", "pSTAT6"),
    parallel_cores = 2,
    bootstrap.number = 100
  )

model.list.pS1pS3[[as.character(Stim_)]] <-
  ITRC::ITRC(
    data = ITRC::data.itrc.cytof %>%
      dplyr::filter(Stim != 50) %>%
      dplyr::filter(Stim <= Stim_),
    signal = "Stim",
    response = c("pSTAT1", "pSTAT3"),
    parallel_cores = 2,
    bootstrap.number = 100
  )

signal.list <-
  (model$confusion.matrix %>%
     dplyr::distinct_(model$signal) %>%
     dplyr::arrange_(model$signal))[[model$signal]]

confusion.waves.all <-
  CalculateConfusionWaves(
    model = model.list[[as.character(Stim_)]],
    signal.list = signal.list)
confusion.waves.pS1_no <-
  CalculateConfusionWaves(
    model = model.list.pS1_no[[as.character(Stim_)]],
    signal.list = signal.list)
confusion.waves.pS3_no <-
  CalculateConfusionWaves(
    model = model.list.pS3_no[[as.character(Stim_)]],
    signal.list = signal.list)
confusion.waves.pS1 <-
  CalculateConfusionWaves(
    model = model.list.pS1[[as.character(Stim_)]],
    signal.list = signal.list)
confusion.waves.pS1pS3 <-
  CalculateConfusionWaves(
    model = model.list.pS1pS3[[as.character(Stim_)]],
    signal.list = signal.list)
confusion.waves.ps1_specific <-
  confusion.waves.all %>%
  dplyr::left_join(
    confusion.waves.pS1_no,
    by = c("Stim", "class", "signal_level", "class_level")
  ) %>%
  dplyr::mutate(itrc = itrc.x - itrc.y)

confusion.waves.ps3_specific <-
  confusion.waves.all %>%
  dplyr::left_join(
    confusion.waves.pS3_no,
    by = c("Stim", "class", "signal_level", "class_level")
  ) %>%
  dplyr::mutate(itrc = itrc.x - itrc.y)

confusion.waves.pS4pS5pS6_specific <-
  confusion.waves.all %>%
  dplyr::left_join(
    confusion.waves.pS1pS3,
    by = c("Stim", "class", "signal_level", "class_level")
  ) %>%
  dplyr::mutate(itrc = itrc.x - itrc.y)

confusion.waves.ps1_no_specific <-
  confusion.waves.all %>%
  dplyr::left_join(
    confusion.waves.pS1,
    by = c("Stim", "class", "signal_level", "class_level")
  ) %>%
  dplyr::mutate(itrc = itrc.x - itrc.y)

confusion.waves <- confusion.waves.ps1_no_specific

confusion.waves <- confusion.waves.ps1_specific
confusion.waves <- confusion.waves.ps3_specific
confusion.waves <- confusion.waves.pS4pS5pS6_specific
confusion.waves.list <- list(confusion.waves.ps1_specific,
                             confusion.waves.ps1_no_specific,
                             confusion.waves.ps3_specific,
                             confusion.waves.pS4pS5pS6_specific
                             )
foreach(confusion.waves = confusion.waves.list) %do% {
ggplot.data.itrc <-
  confusion.waves %>%
  dplyr::filter_(paste(model$signal, "==", model$class)) %>%
  dplyr::left_join(
    signals.rescale.df,
    by = model$signal
  ) %>%
  dplyr::mutate(
    type = "itrc")



x.itrc  <- "signal_rescaled"
y.itrc <- "itrc"
group.itrc <- "type"

g.plot <-
  ggplot2::ggplot() +
  ITRC::theme_itrc()

g.plot +
  ggplot2::geom_point(
    data = ggplot.data.itrc,
    mapping = ggplot2::aes_string(
      x = x.itrc,
      y = y.itrc)
  ) +
  ggplot2::geom_line(
    data = ggplot.data.itrc,
    mapping = ggplot2::aes_string(
      x = x.itrc,
      y = y.itrc,
      group = group.itrc)
  ) + ggplot2::ylim(c(-0.1,1)) ->
  g.plot
return(g.plot)
} -> g.specific.plot.list

g.specific.plot <-
  cowplot::plot_grid(
    plotlist = g.specific.plot.list,
    ncol = 2,
    nrow = 2,
    rel_heights = c(1,1),
    rel_widths = c(1,1))

width_ <- 2*6
height_ <- 2*4
ggplot2::ggsave(filename = paste(path, "itrc_specific_bcells.pdf", sep= "/"),
                plot = g.specific.plot,
                height = height_,
                width  = width_)
