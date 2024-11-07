
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))



# exercise 1 --------------------------------------------------------------

## draw a violin plot with PlantGrowth
df_pg <-PlantGrowth

df_pg %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(fill = NA,
              draw_quantiles = c(0.25, 0.5, .75)) + 
  geom_jitter(alpha = 0.5,
              width = .1)

## perform anova aov() with df_pg

pg <- aov(formula = weight ~ group,
          data = df_pg)

## Same?

df_pg <- PlantGrowth %>% 
  as_tibble()

df_pg %>% ggplot(aes(x = group,
                     y = weight)) +
  geom_jitter(alpha = .5,
              width = .1) +
  geom_violin(fill = NA,
              draw_quantiles = c(0.25, 0.5, .75))

fit <- aov(formula = weight ~ group,
           data = df_pg)
fit
# exercise 2a --------------------------------------------------------------


##calculate F value manually with df_pg

mu <- mean(df_pg$weight)

## between group
s_b <- df_pg %>% 
  group_by(group) %>% 
  summarize(mu_g = mean(weight), 
            dev_g = (mu_g - mu)^2, 
            n = n(),
            ss = dev_g * n) %>% 
  pull(ss) %>% 
  sum()

var_b <- s_b / (n_distinct(df_pg$group) -1)

##within group
s_w <- df_pg %>% 
  group_by(group) %>% 
  mutate(mu_g = mean(weight)) %>% 
  ungroup() %>% 
  mutate(dev_i = (weight - mu_g)^2) %>% 
  group_by(group) %>% 
  summarize(ss = sum(dev_i)) %>% 
  pull(ss) %>% 
  sum()

## What I did 
mu <- mean(df_pg$weight)
df_g <- df_pg %>% 
  group_by(group) %>% 
  summarize(mu_g = mean(weight), 
            dev_g = (mu_g - mu)^2, 
            n = n())

df_g <- df_g %>% 
  mutate(ss = dev_g * n)

s_b <- sum(df_g$ss)

n_g <- n_distinct(df_pg$group)

var_b <- s_b/ (n_g - 1)
var_w <- s_w / (nrow(df_pg) - n_distinct(df_pg$group))

f_value <- var_b / var_w

f_value


# exercise 2b -------------------------------------------------------------

## create a function to calculate F function
## add s_b and var_b
fsb <- function(data){
  s_b <- data %>% 
    group_by(group) %>% 
    summarize(mu_g = mean(weight), 
              dev_g =(mu_g - mu)^2,
              n = n(),
              ss = dev_g * n) %>%  
    pull(ss) %>% 
    sum()
  var_b <- s_b/(n_distinct(data$group)-1)
  s_w <- df_pg %>% 
    group_by(group) %>% 
    mutate(mu_g = mean(weight)) %>% 
    ungroup() %>% 
    mutate(dev_i = (weight - mu_g)^2) %>% 
    group_by(group) %>% 
    summarize(ss = sum(dev_i)) %>% 
    pull(ss) %>% 
    sum()
  
  var_w <- s_w / (nrow(df_pg) - n_distinct(df_pg$group))
  
  f_value <- var_b / var_w
  
  return(f_value)
  
}

fsb(data = df_pg)

summary(fit)
