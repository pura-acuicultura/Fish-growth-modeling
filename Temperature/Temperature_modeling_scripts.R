##### MODELING TEMPERATURE WITH SIN WAVES #####

library(tidyverse)

# 1. DEFINE WORKING TIME SCALE #####

date_ini <- as.Date("2010-01-01")
date_fin <- as.Date("2020-12-31")
temp_mean <- 25
temp_var <- 2
advance <- 0

DATE <- seq(date_ini, date_fin, by = 1)
DAY <- 1:length(DATE)
DAY <- (advance + 1):(advance + length(DATE))
df_temp <- data.frame(DATE, DAY)

df_temp %>%
  mutate(DAY_rad = -1 * DAY / 365.25 * 2 * pi) %>%
  mutate(TEMP = temp_mean + temp_var * sin(DAY_rad)) %>%
  ggplot(aes(x = DATE, y = TEMP)) +
  geom_line(color = "blue", size = 1) +
  scale_y_continuous(limits = c(0, 30)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%b-%y") +
  xlab("") +
  ylab("temperature (C)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

# 2. MULTI-YEAR PATTERNS #####
#___1. sinusodal patterns ####
df_temp %>%
  mutate(DAY_rad = -1 * DAY / 365.25 * 2 * pi) %>%
  mutate(TEMP_day = 8 + 4 * sin(DAY_rad)) %>%
  mutate(ANN_rad = -1 * DAY / 365.25 * 2 * pi * 1/10) %>%
  mutate(TEMP_ann = 1 * sin(ANN_rad)) %>%
  mutate(TEMP = TEMP_day + TEMP_ann) %>%
  ggplot(aes(x = DATE, y = TEMP)) +
  geom_line(color = "blue", size = 1) +
  scale_y_continuous(limits = c(0, 20)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%b-%y") +
  xlab("") +
  ylab("temperature (C)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

#___2. exponential increase patterns ####
df_temp %>%
  mutate(DAY_rad = -1 * DAY / 365.25 * 2 * pi) %>%
  mutate(TEMP_day  = 8 + 4 * sin(DAY_rad)) %>%
  mutate(ANN_rad = -1 * DAY / 365.25 * 2 * pi * 1/10) %>%
  mutate(TEMP_ann = 1 * sin(ANN_rad)) %>%
  mutate(TEMP = TEMP_day + TEMP_ann) %>%
  ggplot(aes(x = DATE, y = TEMP)) +
  geom_line(color = "blue", size = 1) +
  scale_y_continuous(limits = c(0, 20)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%b-%y") +
  xlab("") +
  ylab("temperature (C)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))
