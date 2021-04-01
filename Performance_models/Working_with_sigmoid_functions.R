##### WORKING WITH SIGMOID FUNCTIONS #####

#__1. General sigmoid equation ####
x = seq(-5, 5, by = 0.1)
y = 1 / (1 + exp(-x))

df_1 <- data.frame(temperature = x,
                   performance = y)
df_1 %>%
  ggplot() +
  geom_line(aes(x = temperature, y = performance), size = 1.5, color = 'blue') +
  theme_bw()