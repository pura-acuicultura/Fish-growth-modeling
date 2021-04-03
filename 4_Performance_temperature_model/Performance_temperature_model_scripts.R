##### PERFORMANCE X TEMP RELATIONSHIPS #####
# 1. LIBRARIES USED #####
library(tidyverse)

# 2. PARABOLA FUNCTIONS ##### 
alpha_1 <- -0.008 # dictates the direction and width of the parabola
T_opt <- 28 # temperature optimum of the species
P_max <- 1 # performance maximum

TEMP <- seq(0, 56, by = 0.1) # an array of temperatures between 0 and 2 * T_opt
PERF <- alpha_1 * (TEMP - T_opt)^2 + P_max # sigmoid formula
df_parab <- data.frame(TEMP, PERF) # arrange data frame

df_parab %>%
  ggplot() +
  geom_vline(xintercept = c(20, 36), color = 'red', size = 1) + # add tolerable range
  geom_vline(xintercept = c(26, 30), color = 'green', size = 1) + # add optimum range 
  geom_vline(xintercept = T_opt, color = 'blue', size = 1) + # add temperature optimum
  geom_line(aes(x = TEMP, y = PERF), color = 'black', size = 1) +
  scale_x_continuous(breaks = seq(0, 56, by = 4)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "temperature (C)", y = "relative performance (0-to-1)") +
  theme_bw()

# 3. SIGMOID FUNCTIONS #####
#__1. Sigmoid from 0 to T_opt ####
T_min <- 20 # tolerable low temperature of species
beta_1 <- 1.7 # dictates the 'slope' of the sigmoid curve

TEMP = seq(0, T_opt, by = 0.1)
PERF = 1 / (1 + beta_1^(-TEMP + T_min))
df_sig_1 <- data.frame(TEMP, PERF)

df_sig_1 %>%
  mutate(PERF = PERF / max(PERF)) %>%
  ggplot() +
  geom_vline(xintercept = T_min, color = 'red', size = 1) + # add tolerable low temp
  geom_vline(xintercept = 26, color = 'green', size = 1) + # add optimum low temp 
  geom_vline(xintercept = T_opt, color = 'blue', size = 1) + # add temperature optimum
  geom_line(aes(x = TEMP, y = PERF), size = 1, color = 'black') +
  scale_x_continuous(breaks = seq(0, 28, by = 4)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(x = "temperature (C)", y = "relative performance (0-to-1)") +
  theme_bw()

#__2. Sigmoid from T_opt to 56 ####
T_max <- 33 # tolerable high temperature of the species
beta_2 <- 0.4 # dictates es the 'slope' of the sigmoid curve

TEMP = seq(T_opt, 56, by = 0.1)
PERF = 1 / (1 + beta_2^(-TEMP + T_max))
df_sig_2 <- data.frame(TEMP, PERF)

df_sig_2 %>%
  mutate(PERF = PERF / max(PERF)) %>%
  ggplot() +
  geom_vline(xintercept = T_max, color = 'red', size = 1) + # add tolerable high temp
  geom_vline(xintercept = 30, color = 'green', size = 1) + # add optimum low temp 
  geom_vline(xintercept = T_opt, color = 'blue', size = 1) + # add temperature optimum
  geom_line(aes(x = TEMP, y = PERF), size = 1, color = 'black') +
  scale_x_continuous(breaks = seq(28, 56, by = 4)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(x = "temperature (C)", y = "relative performance (0-to-1)") +
  theme_bw()

#__3. Combined sigmoid from 0 to 56 ####
TEMP = seq(0, 56, by = 1)
PERF <- ifelse(TEMP <= T_opt,
               (1 / (1 + beta_1^(-TEMP + T_min))) / max(df_sig_1$PERF),
               (1 / (1 + beta_2^(-TEMP + T_max))) / max(df_sig_2$PERF))

df_sig <- data.frame(TEMP, PERF)

df_sig %>%
  ggplot() +
  geom_vline(xintercept = c(T_min, T_max), color = 'red', size = 1) + # add tolerable range
  geom_vline(xintercept = c(26, 30), color = 'green', size = 1) + # add optimum range
  geom_vline(xintercept = T_opt, color = 'blue', size = 1) + # add temperature optimum
  geom_line(aes(x = TEMP, y = PERF), size = 1, color = 'black') +
  scale_x_continuous(breaks = seq(0, 56, by = 4)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(x = "temperature (C)", y = "relative performance (0-to-1)") +
  theme_bw()

# 4. COMBINED SIGMOID/PARABOLA CURVE #####
alpha_2 <- -0.02 # dictates the direction and width of the parabola
T_opt <- 28 # temperature optimum of the species
P_max <- 1 # performance maximum

TEMP <- seq(T_opt, 56, by = 0.1) # an array of temperatures between 0 and 2 * T_opt
PERF <- alpha_2 * (TEMP - T_opt)^2 + P_max # sigmoid formula
df_parab_2 <- data.frame(TEMP, PERF) # arrange data frame

TEMP = seq(0, 56, by = 0.1)
PERF <- ifelse(TEMP <= T_opt,
               (1 / (1 + beta_1^(-TEMP + T_min))) / max(df_sig_1$PERF),
               (alpha_2 * (TEMP - T_opt)^2 + P_max) / max(df_parab_2$PERF))

df_sig_parab <- data.frame(TEMP, PERF)

df_sig_parab %>%
  ggplot() +
  geom_vline(xintercept = c(T_min, T_max), color = 'red', size = 1) + # add tolerable range
  geom_vline(xintercept = c(26, 30), color = 'green', size = 1) + # add optimum range
  geom_vline(xintercept = T_opt, color = 'blue', size = 1) + # add temperature optimum
  geom_line(aes(x = TEMP, y = PERF), size = 1, color = 'black') +
  scale_x_continuous(breaks = seq(0, 56, by = 4)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "temperature (C)", y = "relative performance (0-to-1)") +
  theme_bw()

# 5. HARD-CODED PERFORMANCE x TEMPERATURE FUNCTION #####

T_opt # temperature optimum = 28
beta_1 # beta coefficient = 1.7
T_min # tolerable temperature minimum = 20 
max(df_sig_1$PERF) # max performance of sigma curve = 0.9858672
alpha_2 # alpha coefficient = -0.02
P_max # performance maximum = 1
max(df_parab_2$PERF) # max performance of parabola curve = 1

TPF <- function(TEMP) {ifelse(TEMP <= 28,
                       (1 / (1 + 1.7^(-TEMP + 20))) / 0.9858672,
                       (-0.02 * (TEMP - 28)^2 + 1) / 1)}

c(TPF(20), TPF(28), TPF(33))
