##### Gullen-Holt modeling #####

library(tidyverse)
library(ggrepel)
library(drc)

setwd("~/R/git projects/Fish_growth_modeling/Nutrition_meta_analysis")

df <- read.csv(file = 'weight_data.csv', header = TRUE)
df <- df[-1,]

# 1. EXPERIMENTAL DURATION TRANSFORM ####

#___1. A simple model to start with ####
a <- 0.00000092145
b <- 3.43456

day <- seq(1, 356, 1)
weight <- a*day^b
df <- data.frame(day, weight)

df %>% ggplot() +
  geom_point(aes(x = day, y = weight), color = "blue") +
  theme_bw()

# calculate d_weight @ 250 days

pgf <- function(exp_day, exp_lgth){
  
                a <- 0.00000092145
                b <- 3.43456
                
                grwth_ini <- a * (exp_day - exp_lgth/2)^b
                grwth_fin <- a * (exp_day + exp_lgth/2)^b
                
                delta_g <- (grwth_fin - grwth_ini) / exp_lgth
                
                grwth_ini_1d <- a * (exp_day - 0.5)^b
                grwth_fin_1d <- a * (exp_day + 0.5)^b
                
                delta_g_1d <- grwth_fin_1d - grwth_ini_1d
                
                delta_g / delta_g_1d
                }

pgf(250, 10)

x_length <- 1:200
exp_days <- c(50, 100, 250, 300, 350)

a <- 0.00000092145
b <- 3.43456
exp_wghts <- round(a * exp_days^b, 0)

y <- outer(FUN = pgf, exp_days, x_length)
df <- data.frame(exp_lgth = x_length, t(y))
colnames(df) <- c('exp_lgth', paste("W", exp_wghts, sep = "_"))
head(df)

df %>%
  gather(key = "weight", value = "correction", -exp_lgth) %>%
  group_by(weight) %>%
  ggplot() +
  geom_point(aes(x = exp_lgth, y = correction, color = weight)) +
  theme_bw()



# 1. WEIGHT TRANSFORMATION #####
ref_id_levels <- unique(df$ref_id)

df_1 <- df %>%
  mutate(ref_id = factor(ref_id, levels = ref_id_levels)) %>%
  filter(notes == 'verified') %>%
  select(ref_id, diet, w_ini, w_fin, days) %>%
  mutate(w_ini = as.numeric(w_ini)) %>%
  mutate(w_fin = as.numeric(w_fin)) %>%
  mutate(days = as.numeric(days)) %>%
  mutate(growth = (w_fin - w_ini) / days) %>%
  mutate(weight = (w_fin + w_ini) / 2) %>%
  mutate(growth_l = log(growth)) %>%
  mutate(weight_l = log(weight))

head(df_1)
str(df_1)

df_1 %>%
  ggplot() +
  geom_point(aes(x = weight, y = growth, color = ref_id)) +
  # geom_text_repel(aes(x = weight, y = growth, label = ref_id),
  #                 max.overlaps = 100, size = 2.5) +
  geom_smooth(aes(x = weight, y = growth),
              method = 'lm', formula = y ~ x,
              color = 'black', linetype = 'dashed', se = FALSE) +
  theme_bw()

# identify outliers
df_1 %>%
  filter(weight > 400) %>%
  filter(growth < 4)

# fit a michaelis mentin curve to the data
model <- drm(growth ~ weight, fct = MM.2(), data = df_1)

df_1 %>%
  mutate(fit = predict(model)) %>%
  ggplot() +
  geom_point(aes(x = weight, y = growth, color = ref_id)) +
  geom_line(aes(x = weight, y = fit),
            color = "black", linetype = "dashed", size = 1) +
  theme_bw()

cor(df_1$growth, predict(model))

# log transformation

df_1 %>%
  ggplot() +
  geom_point(aes(x = weight_l, y = growth_l, color = ref_id)) +
  # geom_text_repel(aes(x = weight_l, y = growth_l, label = ref_id),
  #                 max.overlaps = 100, size = 2.5) +
  geom_smooth(aes(x = weight_l, y = growth_l),
              method = 'lm', formula = y ~ x,
              color = 'black', linetype = 'dashed', se = FALSE) +
  theme_bw()

cor(df_1$growth_l, df_1$weight_l)

head(df_1)







# 2. LENGTH TRANSFORMATION #####
ref_id_levels <- unique(df$ref_id)

df_2 <- df %>%
  mutate(ref_id = factor(ref_id, levels = ref_id_levels)) %>%
  filter(notes == 'verified') %>%
  select(ref_id, diet, w_ini, w_fin, days) %>%
  mutate(w_ini = as.numeric(w_ini)) %>%
  mutate(w_fin = as.numeric(w_fin)) %>%
  mutate(days = as.numeric(days)) %>%
  mutate(l_ini = (w_ini / 0.01065)^(1/3.258)) %>%
  mutate(l_fin = (w_fin / 0.01065)^(1/3.258)) %>%
  mutate(growth = (l_fin - l_ini) / days) %>%
  mutate(length = (l_fin + l_ini) / 2)

head(df_2, 100)
str(df_2)

df_2 %>%
  ggplot() +
  geom_point(aes(x = length, y = growth, color = ref_id)) +
  geom_text_repel(aes(x = length, y = growth, label = ref_id),
                  max.overlaps = 100, size = 2.5) +
  geom_smooth(aes(x = length, y = growth),
              method = 'lm', formula = y ~ x,
              color = 'black', linetype = 'dashed', se = FALSE) +
  theme_bw()

cor(df_2$growth, df_2$length)

df_2 %>%
  ggplot() +
  geom_point(aes(x = l_ini, y = w_ini))

df_2 %>%
  ggplot() +
  geom_point(aes(x = l_fin, y = w_fin))
