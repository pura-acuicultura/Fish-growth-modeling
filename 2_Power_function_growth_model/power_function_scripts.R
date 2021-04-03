##### Power function growth model #####

library(tidyverse)
library(gridExtra)

# 1. INTRODUCTION #####
#___1. A simple model to start with ####
a <- 0.00000092145
b <- 3.43456

day <- seq(1, 356, 1)
weight <- a*day^b
df <- data.frame(day, weight)

df %>% ggplot() +
  geom_point(aes(x = day, y = weight), color = "blue") +
  theme_bw()

#___2. Incorrectly modeled error ####
df %>% 
  mutate(error = rnorm(356, 0, 5)) %>%
  mutate(weight_2 = weight + error) %>%
  ggplot() +
  geom_point(aes(x = day, y = weight_2), color = "blue") +
  theme_bw()

# 2. FLIPPING THE MODEL TO DELTA WEIGHT AS A FUNCTION OF WEIGHT ####
#___1. Re-parameterize the model #### 
df <- df %>%
  mutate(delta_w = weight - lag(weight, 1)) %>%
  mutate(delta_d = day - lag(day, 1)) %>%
  mutate(delta_g = delta_w / delta_d) %>%
  mutate(weight_a = (weight + lag(weight, 1))/2) %>%
  drop_na()

df %>% ggplot() +
  geom_point(aes(x = weight_a, y = delta_g), color = 'blue') +
  labs(title = "weight gain as a function of weight") +
  labs(x = "weight (g)",
       y = "weight gain (grams/day)") +
  theme_bw()

#___2. Re-fit the model ####

fit <- lm(log(delta_w) ~ log(weight_a), data = df)
coef(fit)

cwg <- function(w_init){
          exp(-2.818561 + 0.7104807 * log(w_init))
          }

x <- seq(0, 500, by = 1)
y <- as.array(sapply(x, FUN = cwg))
df_1 <- data.frame(x, y)

df_1 %>%
  ggplot() +
  geom_point(aes(x = x, y = y), color = 'blue') +
  theme_bw()

# 3. PREDICT GROWTH USING THE CUMULATIVE FUNCTION #####

dog <- 356

w_cum <- vector(length = dog)
w_cum[1] <- a*1^b

for(i in 1:dog){
  w_cum[i+1] <- w_cum[i] + cwg(w_cum[i])
}

x <- 1:dog
y <- w_cum[-357]
df_2 <- data.frame(x, y)

df_2 %>%
  ggplot() +
  geom_point(aes(x = x, y = y), color = 'blue') +
  theme_bw()

# 4. ADD ERROR IN THE CONTEXT OF GROWTH #####

pop <- function(){
  dog <- 356
  w_cum <- vector(length = dog)
  w_cum[1] <- a*1^b
  
  for(i in 1:dog){
    w_cum[i+1] <- w_cum[i] + cwg(w_cum[i]) * rnorm(1, 1, 0.4)
  }
  w_cum[-357]
}

df_2 <- data.frame(x = 1:356, y = pop())

df_2 %>%
  ggplot() +
  geom_point(aes(x = x, y = y), color = 'blue') +
  ylim(c(0, 500)) +
  theme_bw()

# 5. CREATE A POPULATION OF INDIVIDUALS THROUGH RANDOM ERROR #####

n_pop <- 200

day <- rep(1:356)
weight <- replicate(n_pop, pop())
df_pop <- data.frame(cbind(day, weight))
colnames(df_pop) <- c("day", 1:n_pop)

GC <- df_pop %>%
  gather(key = "fish_id", value = "weight", -day) %>%
  mutate(fish_id = factor(fish_id, levels = as.character(1:n_pop))) %>%
  ggplot() +
  geom_line(aes(x = day, y = weight, color = fish_id)) +
  theme_bw() +
  theme(legend.position = "none")

HIST <- df_pop %>%
  gather(key = "fish_id", value = "weight", -day) %>%
  mutate(fish_id = factor(fish_id, levels = as.character(1:n_pop))) %>%
  filter(day == 300) %>%
  ggplot() +
  geom_histogram(aes(weight), bin.width = 10, fill = 'blue') +
  xlim(c(150, 400)) +
  theme_bw()

grid.arrange(GC, HIST, nrow = 1)
