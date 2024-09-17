
# Setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)




# Comparing Central Tendency Measures -------------------------------------
x <- exp(rnorm(n = 1000, 
               mean = 0,
               sd = 1))
## vector and mean median and geom mean
mu_x <- (mean(x))
geom_mu_x <- (prod(x)^(1/length(x)))
med_x <-(median(x))

## create tibble with vector x

df_x <- tibble(x = x)

## histogram with x

g_hist <- df_x %>% 
  ggplot(aes(x = x)) +
  geom_histogram()
## comparing CTM

z <- exp(rnorm(n = 1000, 
               mean = 0,
               sd = 1))
z_rev <- -z +max(z) +.01 

## arithmetic, geom, median

mu_a <- mean(z_rev)
mu_g <- exp(mean(log(z_rev)))
mu_m <- median(z_rev)
## histogram

df_z_rev <- tibble(z_rev = z_rev)       

g_hist <- df_z_rev %>% 
  ggplot(aes(x = z_rev)) +
  geom_histogram()

## vertical line

df_mu <- tibble(mu = c(mu_a, mu_g, mu_m),
                type = c("Arithmetic", "Geometric", "Median"))

g_hist + 
  geom_vline(data = df_mu,
             aes(xintercept = mu,
                 color = type)) +
  theme_bw()

# Comparing Variation Measures --------------------------------------------

## Convert the unit of w to “milligram” and create a new vector m
w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

m <- w * 1000



## Calculate SD and MAD for w and m.

(var_w <- sqrt(sum((w - mean(w))^2) / length(w)))

(sd_w <- sqrt(var_w))
(mad_w <- median(abs(w - median(w))))

(var_m <- sqrt(sum((m - mean(m))^2) / length(m)))
(sd_m <- sqrt(var_m))
(mad_m <- median(abs(m - median(m))))

## Calculate CV and MAD/Median for w and m

(cv_w <- sd_w / mean(w))
(mm_w <- mad_w / median(w))

(cv_m <- sd_m / mean(m))
(mm_m <- mad_m / median(m))
