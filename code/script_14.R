

# setup -------------------------------------------------------------------


rm(list = ls())
## source("code/set_library.R")
library(tidyverse)
library(patchwork)
## source(here::here("code/set_library.R"))



# AOV trial------------------------------------------

##ANOVA Analysis of Variance 
df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))
distinct(df_anova, lake)

# first argument is formula
# second argument is data frame for reference
# do not forget specify data = XXX! aov() refer to columns in the data frame
m <- aov(formula = length ~ lake,
         data = df_anova)

print(m)
summary(m)


# partition variability ---------------------------------------------------


df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency

# estimate overall mean
mu <- mean(df_anova$length)
mu

# estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group

print(df_g)


df_g <- df_g %>% 
  mutate(ss = dev_g * n)

print(df_g)
 
## summing across lakes

df_g$ss

s_b <- sum(df_g$ss)
print(s_b) ## sum of the squares for lakes from the aov function 


# within group function ---------------------------------------------------
## sum across lakes 
df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish

## sum across fish individuals
# filter() & slice(): show first 3 rows each group

df_i_ss <- df_i %>% 
  group_by(lake) %>% 
  summarize(ss = sum(dev_i))



s_w <- sum(df_i_ss$ss)
s_w


# variability to variance -------------------------------------------------

# n_distinct() count the number of unique elements

n_g <- n_distinct(df_anova$lake)

var_b <- s_b/n_distinct(df_anova$lake)-1

var_w <- s_b / (n_g - 1)

f_value <- var_b / var_w


## Null hypothesis 

x <- seq(0, 10, by = 0.1)
y <- df(x = x, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) + 
  geom_line() + # F distribution
  geom_vline(xintercept = f_value,
             color = "salmon") # observed F-statistic


## p-value
# pf() estimate the probability of less than q
# Pr(F0 > F) is 1 - Pr(F0 < F)
p_value <- 1 - pf(q = f_value, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)
print(p_value)