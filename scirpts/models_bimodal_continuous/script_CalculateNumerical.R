library(tidyverse)
library(SysBioSigTheme)
library(ITRC)
distr.min <- list(mu = 20, sd = 5)
distr.max <- list(mu = 60, sd = 5)

fun.distr.mix <-
  function(p){
    function(x){
      p*dnorm(x = x,
              mean = distr.min$mu,
              sd = distr.min$sd) +
        (1-p)*dnorm(x = x,
                    mean = distr.max$mu,
                    sd = distr.max$sd)}
  }

# fun.distr.mix.combine <-
#     function(x,p){
#       p*dnorm(x = x,
#               mean = distr.min$mu,
#               sd = distr.min$sd) +
#         (1-p)*dnorm(x = x,
#                     mean = distr.max$mu,
#                     sd = distr.max$sd)
#     }

fun.distr.mix.combine <-
  function(x,p){
    fun.distr.mix(p)(x)
  }


max.fun.distr <-
  function(p.list = seq(from = 0, to = 1, length.out = p.num)){
    function(x){
      (crossing(p = p.list, x = x) %>%
        dplyr::mutate(
          res = fun.distr.mix.combine(p = p, x = x)
        ) %>%
         dplyr::group_by(x) %>%
        dplyr::summarise(res = max(res)))[["res"]]
    }
  }

max.fun.distr.pair <-
  function(p.list = seq(from = 0, to = 1, length.out = p.num), p.stim, p.class){
    function(x){
      crossing(p = p.list, x = x) %>%
         dplyr::mutate(
           res = fun.distr.mix.combine(p = p, x = x)
         ) -> df.res
      df.res %>%
      dplyr::group_by(x) %>%
      dplyr::filter(res == max(res)) ->
        df.res.class

      x.classified <-
        (df.res.class %>%
        dplyr::filter(p == p.class))[["x"]]

      (df.res %>%
        dplyr::filter(p == p.stim) %>%
        dplyr::mutate(resclassified = dplyr::if_else(x %in% x.classified, res, 0)))[["resclassified"]]
    }
  }




p.num <- 9
#p.list <- seq(from = 0, to = 1, length.out = p.num)
p.list <- seq(from = 0, to = 1, length.out = p.num)[c(1,5,9)]
crossing(p.stim = p.list,
         p.class = p.list) ->
  p.df

foreach::foreach(p.i = 1:nrow(p.df)) %do% {
  res <-   integrate(f = max.fun.distr.pair(
    p.list = p.list,
    p.stim = p.df[p.i,]$p.stim,
    p.class = p.df[p.i,]$p.class),
    lower = -1000,
    upper =  1000,
    subdivisions = 100000)

  data.frame(
    Stim = p.df[p.i,]$p.stim,
    class = p.df[p.i,]$p.class,
    max.signal = max(p.df$p.stim),
    prob = res$value,
    prob.sd = res$abs.error
  )
} %>%
  do.call(what = rbind,
          args = .) ->
  df.confusion.matrix

df.confusion.matrix  %>%
  dplyr::mutate(prob = dplyr::if_else(prob >= 1, 1, prob)) %>%
  dplyr::mutate(prob = dplyr::if_else(prob <= 0, 0, prob)) ->
  df.confusion.matrix

foreach(p = p.list) %do% {
  res <- integrate(f = max.fun.distr(p.list = p.list[which(p.list <= p)]),
            lower = -1000,
            upper =  1000, subdivisions = 100000)
  data.frame(
    Stim = p,
    itrc = res$value
    #integrate.error = res$abs.error
  )
} %>%
  do.call(what = rbind,
          args = .) ->
  df.itrc



model <-
  list(data = data.frame(Stim = p.list),
       signal = "Stim",
       class  = "class",
       response = "response",
       sample = "sample",
       confusion.table = df.confusion.matrix,
       itrc = df.itrc
  )

class(model) <- "ITRCModel"


g.waves <- ITRC::plotITRCWaves(model = model, colors = viridis(n = length(p.list), end = 0.8), title_ = "", theme_fun.args = list(legend.position = "none"), xlab_ = "", ylab_ = "")

g.confusion.matrix <- ITRC::plotCofusionMatrix(model = model)



crossing(x = seq(from = 1, to = 80, length.out = 1000),
         p = p.list) %>%
  dplyr::mutate(
    y = fun.distr.mix.combine(x = x, p = p)
  ) ->
  df.plot

g.plots <-
  ggplot(data = df.plot,
         aes(x = x, y= y, color = factor(p), group = p)) +
  geom_line() +
  SysBioSigTheme::theme_sysbiosig(legend.position = "none") +
  scale_color_viridis(discrete = TRUE, name = "stimulation", end = 0.8)



cowplot::plot_grid(cowplot::plot_grid(g.plots, g.waves, rel_widths = c(1.5,1), nrow = 1), g.confusion.matrix, ncol = 1, rel_heights = c(1,1.5)) ->
  g.grid

output.path <- "resources/output/model_bimodal_continuous/1d/2019-10-30-bimodal_logmvn_bootstrap/"
dir.create(output.path)
ggsave(filename =  paste(output.path, "grid.pdf", sep = "/"), plot = g.grid, useDingbats = FALSE, width = 12, height = 8)
saveRDS(object = model, file = paste(output.path, "model.rds", sep = "/"))
