### ###
### 2019-04-04 manuscript
### ###
library(ITRC)


#### initiaialisation ####
force.run <- TRUE
output.path <- "/media/knt/sdb2/KN/ITRC/resources/nfkb/testing/"
dir.create(path = output.path,
           recursive = TRUE)

signal = "signal"
sample = "sample"
response.ts =
  c("0",  "3",  "6",  "9",
    "12", "15", "18", "21",
    "24", "27", "30", "33",
    "36", "39", "42", "45",
    "48", "51", "54", "57",
    "60")
response.tp18 =
  c("18")
parallel_cores = 8
bootstrap = TRUE
bootstrap.number = 8
bootstrap.sample_size = 1000

#### ITRC model ####
## ts
path.model_ts <- paste(output.path, "model_ts.RDS", sep = "/")
model.ts <- NULL
if(file.exists(path.model_ts)){
  model.ts <- readRDS(path.model_ts)
}
if(is.null(model.ts) | force.run){
  model.ts <-
    ITRC(
      data = ITRC::data.itrc.nfkb.all,
      signal = signal,
      sample = sample,
      response = response.ts,
      parallel_cores = parallel_cores,
      bootstrap.number = bootstrap.number,
      bootstrap = bootstrap,
      bootstrap.sample_size = bootstrap.sample_size
    )
  saveRDS(object = model.ts,
          file = path.model_ts)
}
##tp18
path.model_tp18 <- paste(output.path, "model_tp18.RDS", sep = "/")
model.tp18 <- NULL
if(file.exists(path.model_tp18)){
  model.tp18 <- readRDS(path.model_tp18)
}
if(is.null(model.tp18) | force.run){
  model.tp18 <-
    ITRC(
      data = ITRC::data.itrc.nfkb.all,
      signal = signal,
      sample = sample,
      response = response.tp18,
      parallel_cores = parallel_cores,
      bootstrap.number = bootstrap.number,
      bootstrap = bootstrap,
      bootstrap.sample_size = bootstrap.sample_size
    )
  saveRDS(object = model.tp18,
          file = path.model_tp18)
}
#### ITRC plots ####
# col.rescaled <- "signal_rescaled"
# signals.rescale.df <-
#   rescaleSignalsValues.DataFrame(
#     model = model.ts,
#     col.to.rescale = model.ts$signal,
#     col.rescaled   = col.rescaled,
#     rescale.fun = function(x){log(x = x, base = 10)}
#   )

theme.signal <-
  ITRC::GetRescaledSignalTheme(
    model = model.ts,
    rescale.fun = function(x){log(x = x, base = 10)}
  )

g.nfkb.ts <-
  ITRC::plotITRCWaves(
    model = model.ts)

g.nfkb.ts.comparison <-
  ITRC::plotITRCWaves.Comparison(
    model = model.ts,
    data = model.ts$rc.sum,
    theme.signal  = theme.signal,
    variable.to.compare = "18",
    variable.to.rescale = "18",
    pallete.args = list(option = "B", end = 0.75, begin = 0.25),
    theme.data.line = list(
      color = "red",
      linetype = 3,
      size = 1.5
    ),
    theme.data.points = NULL
)
g.nfkb.ts.comparison

# g.nfkb.tp18 <-
#   ITRC::plotITRCWaves(
#     model = model.tp18)

#### ####
model.tp18$itrc %>%
  dplyr::left_join(
    signals.rescale.df,
    by = model.ts$signal) %>%
  dplyr::mutate(type = "itrc") ->
  model.tp18$itrc.rescaled

g.nfkb.ts.comparison +
  ggplot2::geom_line(
    data = model.tp18$itrc.rescaled,
    mapping = ggplot2::aes_string(
      x = col.rescaled,
      y = "itrc",
      group = "type"
    )
  ) +
  ggplot2::geom_point(
    data = model.tp18$itrc.rescaled,
    mapping = ggplot2::aes_string(
      x = col.rescaled,
      y = "itrc"
    )
  ) ->
  g.nfkb


ggplot2::ggsave(
  filename = paste(output.path, "nfkb_comparison_tstp.pdf", sep = "/"),
  plot = g.nfkb,
  width = 8,
  height = 6,
  useDingbats = FALSE
  )
