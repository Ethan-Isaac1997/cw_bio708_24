

# Setup -------------------------------------------------------------------


rm(list = ls())
library(tidyverse)


# Sampling ----------------------------------------------------------------

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") # unit


df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))
df_h1

## Another tibble. (.) = the previous df that is made

h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, 
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))
df_h2


# Linking part to the whole -----------------------------------------------

# load csv data on R
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))
## 
mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

print(mu)

print(sigma2)

## randomly selecting 10 samples by using sample_n

df_i <- df_h0 %>% 
  sample_n(size = 10) # size specifies the number of rows to be selected randomly

print(df_i)

##Another way
df_i <- df_h0 %>% 
  sample_n(size = 10)

print(df_i)


## obtain 100 sets of 10 plant individuals (randomly selected) and estimate the mean and variance in each

# for reproducibility
set.seed(3)

mu_i <- sigma2_i <- NULL 
for (i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(size = 10) 
  mu_i[i] <- mean(df_i$height)
  sigma2_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
}

print(mu_i)
print(sigma2_i)


## Building histograms with using this data

#install.packages("patchwork") # install only once
library(patchwork)

df_sample <- tibble(mu_hat = mu_i, var_hat = sigma2_i)

## histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

## histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

## layout vertically
## possible only if "patchwork" is loaded
g_mu / g_var


# redo simulation ---------------------------------------------------------

###  for reproducibility
set.seed(3)

## redo simulations
mu_i <- var_i <- var_ub_i <- NULL 
for (i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(size = 10) 
  mu_i[i] <- mean(df_i$height)
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  var_ub_i[i] <- var(df_i$height)
}


## Create histograms
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

## histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

## histogram for variance
## scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

### histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_mu / g_var / g_var_ub



