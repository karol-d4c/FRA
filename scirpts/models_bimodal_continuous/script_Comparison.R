### ###
### script comaprison
### ###


model.bimodal$itrc %>%
  dplyr::mutate(type = "bimodal") %>%
  rbind(.,
        model.cont$itrc %>%
          dplyr::mutate(type = "cont")
        )  %>%
  dplyr::mutate(method = "renyi-min-capacity") ->
  df.itrc_renyi

df.itrc <-
  rbind(df.itrc_renyi,
        df.itrc_cc)

g.itrc <-
  ggplot(data = df.itrc,
       mapping = aes(x = factor(Stim),
                     y = itrc,
                     group = interaction(method, type),
                     color = interaction(method, type))) +
  geom_point() +
  geom_line() +
  ITRC::theme_itrc() +
  ylim(c(1,2))


ggsave(filename = paste(output.path, "itrc.pdf", sep = "/"), plot = g.itrc, width = 8, height = 6)
saveRDS(object = df.itrc, file = paste(output.path, "itrc.rds", sep = "/"))
#### ####
df.itrc <- readRDS( file = paste(output.path, "itrc.rds", sep = "/"))
M <- length(unique(df.itrc$Stim))
df.itrc.edited <- df.itrc
signal.list <- seq(from = 0, to = stims.num - 1)
df.itrc.edited[which(df.itrc.edited$method == "renyi-min-capacity" &  df.itrc.edited$type == "bimodal"),]$itrc <- 0.075*signal.list + 1


df.itrc.edited %>%
  dplyr::left_join(
    (df.itrc.edited %>%
       dplyr::filter(method == "renyi-min-capacity") %>%
       dplyr::mutate(error =  1- itrc/(Stim+1)) %>%
       dplyr::select(Stim, type, error)),
    by = c("Stim", "type")) ->
  df.itrc.edited.error

g.itrc.error <-
  ggplot(data = df.itrc.edited.error,
         mapping = aes(x = 1- error,
                       y = log2(itrc),
                       group = interaction(method, type),
                       color = interaction(method, type))) +
  geom_point() +
  geom_line() +
  ITRC::theme_itrc() +
  ylim(c(0,1))

#### ####
ggplot(data = df.itrc_cc,
       mapping = aes(x = factor(Stim),
                     y = itrc,
                     group = interaction(method, type),
                     color = interaction(method, type))) +
  geom_point() +
  geom_line() +
  ITRC::theme_itrc() +
  ylim(c(1,2))
