##### STACKING HISTOGRAMS #####
library(tidyverse)
library(drc)

temp <- seq(0, 40, by = 1)
base <- dnorm(temp, mean = 20, sd = 4)
add <- dnorm(temp, mean = 25, sd = 3)

df <- data.frame(temp, base, add)

df <- df %>%
  mutate(comp = base + add) %>%
  mutate(comp_adj = comp * 100 / max(comp)) %>%
  filter(temp < 24)
  
df <- df[,c(1,5)]

df_1 <- data.frame(temp = (24:50), comp_adj = 100)

df <- rbind(df, df_1)

df %>%
  ggplot() +
  geom_line(aes(x = temp, y = comp_adj), size = 1.5, color = "grey") +
  geom_point(aes(x = temp, y = comp_adj), size = 1.5, color = "blue")

model <- drm(comp_adj ~ temp, fct = L.3(), data = df)
summary(model)
plot(predict(model))

fit <- lm(comp_adj ~ poly(temp, 15, raw=TRUE), data = df)
plot(predict(fit))




