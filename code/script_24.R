# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)





# Model Fit and Complexity  --------------------------------------------------------

set.seed(1) # for reproducibility

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)
x1


# create a design matrix
X <- model.matrix(~x1)
X
# expected values of y is a function of x
# %*% means matrix multiplication
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra
y_hat <- drop(X %*% b)
#### drop reduces it into a vector 



# add normal errors

y <- rnorm(n = n, mean = y_hat, sd = 0.5)


# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point()


####  correct model used to generate the data
m1 <- lm(y ~ x1, data = df0)

summary(m1)

### add a column x2 which is irrelevant for y
df0 <- df0 %>% 
  mutate(x2 = rnorm(n = nrow(.),
                    mean = 0,
                    sd = 1))


###### add x2 to the model
m2 <- lm(y ~ x1 + x2, data = df0)
summary(m2)


# Comparison Metrics ------------------------------------------------------

# Adjusted R-square for m1 without x2
sm1 <- summary(m1)
print(sm1$adj.r.squared)

# Adjusted R-square for m2 with x2
sm2 <- summary(m2)
print(sm2$adj.r.squared)

c(sm1$adj.r.squared, sm2$adj.r.squared)



# Likelihood ratio test ---------------------------------------------------

# log likelihood: correct model
logLik(m1)

# log likelihood: correct model
logLik(m2)

c(logLik(m1), logLik(m2))


# test = "Chisq" specifies a chi-square distribution
# as a distribution of LR
anova(m1, m2, test = "Chisq")

# AIC ---------------------------------------------------------------------

#### model comparison method.The higher the AIC the worse of a fit the model is.
#### how the model predicts better while adding more data points. AIC best used for field experiment. Formula ----> AIC=2*k - 2ln*L 


# AIC: correct model. better model because value is lower
AIC(m1)
# AIC: correct model. worse model
AIC(m2)
 
