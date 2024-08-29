
# setup -------------------------------------------------------------------

rm(list = ls())
# Matrix ------------------------------------------------------------------
## numeric 

nm1 <- cbind(rep(1, 4),
           rep(2, 4),
           rep(3, 4),
           rep(4, 4))
           
nm2 <- matrix(rep(1:4, each = 4),
               nrow= 4,
               ncol = 4,
               byrow = TRUE)
## character 
cm1 <- cbind(rep("a", 4),
             rep("b", 4),
             rep("c", 4),
             rep("d", 4))
             
cm2 <- matrix(rep(letters[1:4], each = 4),
               nrow = 4 ,
               ncol = 4 ,
               byrow = TRUE)
                    

set.seed(1)
x <- matrix(rnorm(100), nrow = 10, ncol = 10)
which (x>2)
mean(x[x>2])
# Vector ------------------------------------------------------------------
## numeric
v1 <- c(3, 6, 20)

v2 <- c(4, 50, length = 6)
v3 <- rep(1,20)

## character 
v4 <- c("a", "b", "c")
v5 <- c("a", 6)
v6 <- letters[1:20]
##element id

set.seed(1)
x <- rnorm(100)
x
##larger than
x > 2 
which(x >2)

# data frame ---------------------------------------------------------------
# Create data frame
x <- c("aquatic", "snakes") 
y <- c(2.5, 3.5, 8.5, 6.6, 10.2) #girth 
z <- c(2.5 , 3.0 , 3.1 , 3.4 , 4.5) #length
df0 <- data.frame(girth = y, length = z) 
df0

set.seed(1)
x <- rnorm(100, mean = 10, sd = 3)
y <- rpois(100, lambda = 10)
z <- rep(c("VA", "NC"), 50)
df0 <- data.frame(temperature = x, abundance = y, state = z)

va_t <- df0$temperature[df0$state == "VA"]
mu_va_t <- mean(va_t)
va_a <- df0$abundance[df0$state == "VA"]
mu_va_a <- mean(va_a)
nc_t <- df0$temperature[df0$state == "NC"]
mu_nc_t <- mean(nc_t)
nc_a <- df0$abundance[df0$state == "NC"]
mu_nc_a <- mean(nc_a)
## extra
v_va <- with(df0, temperature[state == "VA"])
v_nc <- with(df0, temperature[state == "NC"])

##Tapply
v_mu <- tapply(df0$temperature,
       INDEX = df0$state,
       FUN = mean)

v_mu <- with(df0,
     tapply(temperature,
            INDEX = state,
            FUN = mean))