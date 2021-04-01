##### PERFORMANCE X TEMP RELATIONSHIPS #####

# 1. LIBRARIES USED #####
library(tidyverse)
library(drc)

# 2. PARABOLA FUNCTIONS ##### 
a <- -0.008 # dictates the direction and width of the parabola
T_opt <- 28 # temperature optimum of the species
P_max <- 1 # perforamce maximum

TEMP <- seq(0, 40, by = 1)
PERF <- a * (TEMP - T_opt)^2 + P_max

df_parab <- data.frame(TEMP, PERF)

df_parab %>%
  ggplot() +
  geom_vline(xintercept = 20, color = 'red', size = 1.25, linetype = 'dashed') +
  geom_vline(xintercept = 36, color = 'red', size = 1.25, linetype = 'dashed') +
  geom_vline(xintercept = 26, color = 'green', size = 1.25, linetype = 'dashed') +
  geom_vline(xintercept = 30, color = 'green', size = 1.25, linetype = 'dashed') +
  geom_vline(xintercept = T_opt, color = 'black', size = 1.25) +
  geom_line(aes(x = TEMP, y = PERF), color = 'blue', size = 1.5) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw()

# 3. SIGMOID FUNCTIONS #####
#__1. Sigmoid from 0 to T optimum ####
T_min <- 20
beta_1 <- 1.7

TEMP = seq(0, T_opt, by = 0.1)
PERF = 1 / (1 + beta_1^(-TEMP + T_min))
df_sig_1 <- data.frame(TEMP, PERF)

# max(df_sig_1$PERF) # maintain this value for written equation

df_sig_1 %>%
  mutate(PERF = PERF / max(PERF)) %>%
  ggplot() +
  geom_line(aes(x = TEMP, y = PERF), size = 1.5, color = 'blue') +
  theme_bw()

#__2. Sigmoid from T optimum to 40 ####
T_max <- 33
beta_2 <- 0.4

TEMP = seq(T_opt, 40, by = 0.1)
PERF = 1 / (1 + beta_2^(-TEMP + T_max))
df_sig_2 <- data.frame(TEMP, PERF)

# max(df_sig_2$performance) # maintain this value for written equation

df_sig_2 %>%
  mutate(PERF = PERF - max(PERF)) %>%
  ggplot() +
  geom_line(aes(x = TEMP, y = PERF), size = 1.5, color = 'blue') +
  theme_bw()

#__3. Sigmoids from 0 to 40 ####
TEMP = seq(0, 40, by = 1)
PERF <- ifelse(TEMP <= T_opt,
               (1 / (1 + beta_1^(-TEMP + T_min))) / max(df_sig_1$PERF),
               (1 / (1 + beta_2^(-TEMP + T_max))) / max(df_sig_2$PERF))

df_sig <- data.frame(TEMP, PERF)

df_sig %>%
  ggplot() +
  geom_vline(xintercept = T_min, color = 'red', size = 1.25, linetype = 'dashed') +
  geom_vline(xintercept = T_max, color = 'red', size = 1.25, linetype = 'dashed') +
  geom_vline(xintercept = 26, color = 'dark green', size = 1.25, linetype = 'dashed') +
  geom_vline(xintercept = 30, color = 'dark green', size = 1.25, linetype = 'dashed') +
  geom_vline(xintercept = T_opt, color = 'black', size = 1.25) +
  geom_line(aes(x = TEMP, y = PERF), size = 1.25, color = 'blue') +
  theme_bw()



# MISCELLANEOUS #####
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






