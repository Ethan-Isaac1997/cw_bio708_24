c(1,2)
#test codes 
## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)
## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)

# exercise  ---------------------------------------------------------------

## vector
x <- c(1 ,2 , 3 , 4 , 5 , 6)
x
##character 

x <- c("a", "b", "c")
x

## logical

x <- c(TRUE, FALSE, FALSE)
x
## seq
x <- 1:5
x

##replicate

x <- rep(2,5)
x

x <- rep("a", 8)
x
## sequence by

x <- seq(2, 5, by = .5)
x

x <- seq(2, 5, by = 1)
x

x <- seq(2, 5, length = 14)
x
class(x)

typeof(x)

length(x)

sum(x)

mean(x)
## element ID

y <- c("a", "b", "c")
class(y)

length(y)

x <- c(2,2,3,2,5)
x[4] # access element #4

x <- c(2,2,3,2,5)
x[c(1,2)] # access element #1 and 2


x[c(2,4)] # access elements #2 and 4

x[2:4] # access elements #2-4

## matrix

#ex.1 cbind: combine objects by column
x <- cbind(c(1,2,3), c(4,5,6))
x

#ex.2 rbind: combine objects by row
x <- rbind(c(1,2,3), c(4,5,6))
x

#ex.3 matrix: specify elements and the number of rows (nrow) and columns (ncol)
x <- matrix(1:9, nrow = 3, ncol = 3)
x
class(x)
typeof(x)
dim(x)
colSums(x)
rowSums(x)

y <- matrix(c("a","b", "c", "d", "e", "f"), nrow = 3, ncol = 2)
y

class(y)
typeof(y)
dim(y)

##access 

x[2,3] # access an element in row #2 and colum #3
x[2,] # access elements in row #2
x[c(2,3),] # access elements in rows #2 and 3
x[,c(2,3)] # access elements in columns #2 and 3


# Dataframe ---------------------------------------------------------------


# Create data frame
x <- c("Pristine", "Pristine", "Disturbed", "Disturbed", "Pristine") # Lake type
y <- c(1.2, 2.2, 10.9, 50.0, 3.0) # TSS: total suspended solids (mg/L)
df0 <- data.frame(LakeType = x, TSS = y) # x is named as "LakeType" while y is named as "TSS"
df0

colnames(df0) # call column names

df0$LakeType # access LakeType

df0$TSS # access TSS

df0[,1] # access column #1

df0[1,] # access row #1

df0[c(2,4),] # access row #2 and 4
