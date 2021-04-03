##### WORKING WITH SIGMOID FUNCTIONS #####

library(tidyverse)

#__1. General sigmoid equation ####
x = seq(-10, 10, by = 0.1)
y = 1 / (1 + 1.3^(-x))

df_1 <- data.frame(temperature = x,
                   performance = y)
df_1 %>%
  ggplot() +
  geom_line(aes(x = temperature, y = performance), size = 1.5, color = 'blue') +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, by = 0.1))



sig_alpha <- function(ALPHA){
                             x <- seq(-10, 10, by = 0.1)
                             y <- 1 / (1 + ALPHA^(-x))
                             data.frame(ALPHA, x, y)
                            }
ALPHA <- seq(0, 1, by = 0.1)

a <- lapply(ALPHA, FUN = sig_alpha)
b <- data.frame(do.call(rbind, a))
b$ALPHA <- factor(b$ALPHA)
head(b)
str(b)


b %>%
  group_by(ALPHA) %>%
  ggplot() +
  geom_line(aes(x = x, y = y, color = ALPHA), size = 1) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1)) +
  theme_bw()


