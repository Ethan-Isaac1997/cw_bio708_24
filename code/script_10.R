
# setup -------------------------------------------------------------------

rm(list = ls())
#source(here::here(code/set_library.R")
library(tidyverse)
library(patchwork)
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))



# class work --------------------------------------------------------------

##histogram

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean


##probability distribution function

# create the vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
## min/max(df_h0$height) takes the entire range of values

x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

# calculate probability density of each x value 
# finding the mean and standard deviation of the data set
# dnorm calculates the probability density for a given value.
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)
pd

# figure 
# labs() labels the axis label
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density", x= "Height (cm)")


## pnorm() calculates x between certain values

# probability of x < 10 

p10 <- pnorm(q = 10, mean = mu, sd = sigma)
p10

# probability of x < 20
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
p20

## probability between number/ x1- x2
# probability of 10 < x < 20
p20_10 <- p20 - p10
p20_10

## creating values that correlate with you figure
## floor takes the integer part of the value
## ceiling takes the next closest integer
## each bin has 1cm


x_min <- floor(min(df_h0$height)) 
x_max <- ceiling(max(df_h0$height)) 
bin <- seq(x_min, x_max, by = 1) 
x_min
x_max
## empty object for probability
p <- NULL 
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))
df_prob

bin[length(bin)] 
bin[-length(bin)]

## Building an overlay over a histogram

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) + # bin's center position
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "red") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "pink")


#  Discrete Variable ------------------------------------------------------

## PMF Probability mass function

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))


df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define bin width
                 center = 0) # relative position of each bin



## PMF to frequency distribution

# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)

# calculate probability mass
## dpois d= distribution, dlambda ect. 

lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)


## figure 
tibble(y = pm, x = x) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + 
  geom_point() + 
  labs(y = "Probability",
       x = "Count") 


## probability histogram and overlay frequency

df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) # prob x sample size

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq))
