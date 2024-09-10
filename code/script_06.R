
# setup -------------------------------------------------------------------


rm(list = ls())
library(tidyverse)



# central tendency --------------------------------------------------------

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

##calculate arithmetic mean in 2 ways

mean(x)
sum(x)/length(x)
median(x)

mean(y)
sum(y)/length(y)
median(y)

## Geometric mean, ^ is the same as **
prod(x)^(1/length(x))

prod(y)**(1/length(y))

## log exp(mean(log_y)) = prod(y)**(1/length(y))

log_y <- log(y)
exp(mean(log_y))

## median function

median(x)
median(y)

## manual median
med_y <- sort(y) 
index <- (length(y) + 1) / 2 
med_y <- y[index]
print(med_y)

# Variation ---------------------------------------------------------------

## calculate variance for x and y
## do it manually using sum length and power
# for x

  
sqd_x <- (x - mean(x))^2 ## numerator
sqd_x

sum_sqd_x <- sum(sqd_x) ## summation of the std

var_x <- sum_sqd_x / length(x) ## summation/ # of elements

print(var_x)

var_x <- sum((x - mean(x))^2) / length(x)
print(var_x)
## for y
sqd_y <- (y - mean(y))^2 
sum_sqd_y <- sum(sqd_y)
var_y <- sum_sqd_y / length(y)
print(var_y)

## another way
var_y <- sum((y - mean(y))^2) / length(y)
print(var_y)

## standard deviation

# for x
sd_x <- sqrt(var_x) 
print(sd_x)
## and
sqrt(var_x)
# for y
sd_y <- sqrt(var_y) 
print(sd_y)
## and
sqrt(var_y)

## quantile/percentile

x_24 <- quantile(x)
x_24
x_25 <- quantile(x, 0.25)
x_25
x_75 <- quantile(x, 0,75)
iqr_x <- abs(x_25 - x_75)

## use bracket 

yq <- quantile(y, c(0.25, 0.75))

iqr_y <- abs(yq[1] - yq[2])



# relative variance  ------------------------------------------------------
##coefficient variation

(cv_x <- sd_x / mean(x))


(cv_y <- sd_y / mean(y))

## IQR  median

abs(diff(quantile(x, c(.25, .75)))) / mean(x)

abs(diff(quantile(y, c(.25, .75)))) / mean(y)
