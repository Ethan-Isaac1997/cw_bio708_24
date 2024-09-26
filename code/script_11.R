
# Setup -------------------------------------------------------------------


rm(list = ls())
#source(here::here(code/set_library.R")
library(tidyverse)
library(patchwork)





# ##1  --------------------------------------------------------------------


##1 
x <- rnorm(n = 50, mean = 10, sd = .2)

mu <- 10
mean <- 10
sd <- .2
sigma <- .2 
x1 <- rnorm(n = 50, mean = 10, sd = .2)

##2
x1_min <- floor(min(x1)) 
x1_max <- ceiling(max(x1)) 
bin <- seq(x1_min, x1_min, by = .2)

p <- NULL
for (i in 1:(length(bin)-1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) -pnorm(bin[i], mean = mu, sd = sigma)
}

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.05) %>% 
  mutate(freq = p * length(x1))


tibble(x1 = x1) %>% 
  ggplot(aes(x =x1)) +
  geom_histogram(binwidth = .1,
                 center = .05) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "blue") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "yellow")



# ##2 ---------------------------------------------------------------------
lambda <- 20
x2 <- rpois(n = 1000,
            lambda = 20)
x2_min <- floor(min(x2)) 
x2_max <- ceiling(max(x2)) 

bin <- seq(x2_min, x2_max, by = 1)

df_prob <- tibble(x = bin) %>% 
  mutate(p = dpois(bin, lambda = lambda),
         freq = p* length(x2))
                 

tibble(x2 = x2) %>% 
  ggplot(aes(x =x2)) +
  geom_histogram(binwidth = .1,
                 center = .05) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "blue") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "yellow")