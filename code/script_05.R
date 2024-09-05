
# setup -------------------------------------------------------------------


rm(list = ls())
library(tidyverse)

iris <- as_tibble(iris)

# Function ----------------------------------------------------------------
##function() creates a new function

#without function
x <- rnorm(100, mea = 10, sd = 100)
y <- rnorm(100, mea = 10, sd = 100)
z <- rnorm(100, mea = 10, sd = 100)

sig <- sd(x)
mu <- sd(x)
cv <- sig / mu

#with function
x <- rnorm(100, mea = 10, sd = 100)
fun_cv <- function(x) {
  sd(x) / mean (x)
  
}
fun_cv(x) 
fun_cv(y)
fun_cv(z)
  
##standardize
x0 <- x - mean(x)
z <- x0 / sd(x0)


s <- rnorm(100, mea = 10, sd = 100)
scl <- function(s) {sd(s) / mean (s)
  
}
scl( s = x)

## random functio with two arguements


f0 <- function(phi, zeta) { 
  2* phi + rnorm(1) * zeta
  
}
f0(phi = 2, zeta = 3)
# apply family ------------------------------------------------------------

m <- matrix(rnorm(25), nrow = 5, ncol = 5)

## apply() - fro matrix mainly
apply(m, MARGIN = 1, FUN = mean)
apply(m, MARGIN = 2, FUN = mean)

## can be a function you define
apply(m, MARGIN = 2, FUN = fun_cv)

#for a data frame

apply( iris %>% 
         select(1:4),
       MARGIN = 2, FUN = mean)

##sapply() - for list

x <- rnorm(100)
y <- rnorm(50)
z<- rnorm(10)

## ugly code
l_xyz <- list(x, y, z)

# nice code
sapply(l_xyz, FUN = mean)

## lapply or list but output is a list too

x <- rpois(10, lambda = 5)
y <- rpois(100, lambda = 5)
z <- rep(letters[1:3], 10)

##create list of xyz

l_xyz <- list(x, y, z)

#remove unique elements
unique(x)
unique(y)
unique(z)

lapply(l_xyz, FUN = unique)

## try to get only the first element from each vector


lapply(l_xyz,
       FUN = function(x) {
         x[1]
       })

#2nd

first <- function(x) {
  return(x[1])
}

# for loop ----------------------------------------------------------------


## repeat work in {}
x <- seq(0,10, by = 0.25)
y <- NULL

for(i in 1:10) { 
  y[i]  <- 2 * x[i]
}

y <- NULL 
for(i in 1:10) {
  y <- 2 * x[i]
}
