
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))
##install.packages("skimr")



# lm trial ----------------------------------------------------------------

df_algae <- read_csv(here::here("data_raw/data_algae.csv"))
##skimr::skimr(df_algae)

df_algae


##create a scatter plot

df_algae %>% 
  ggplot(aes(x=conductivity,
             y= biomass))+
  geom_point() +
  theme_bw()

# lm() takes a formula as the first argument
# don't forget to supply your data

m <- lm(biomass ~ conductivity,
        data = df_algae)

m

# get estimates
alpha <- coef(m)[1]
beta <- coef(m)[2]
alpha
beta

## draw estimated regression line

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) # draw the line
## model residual is the distance from the line parallel to the y axis

##get the residual

eps <- df_algae$biomass - (alpha + beta*df_algae$conductivity)
eps

eps0 <- round(resid(m),4)
eps0

mean(eps == eps0)
ss <- sum(eps^2)
ss



# creating a matrix -------------------------------------------------------

# create matrix X produces same values as lm function
v_x <- df_algae %>% pull(conductivity)
X <- cbind(1, v_x)

# create a vector of y
Y <- df_algae %>% pull(biomass)

# %*%: matrix multiplication
# t(): transpose a matrix
# solve(): computes the inverse matrix
theta <- solve(t(X) %*% X) %*% t(X) %*% Y
print(theta)



# standard error and t value ---------------------------------------------------------

##  the Null Hypothesis in regression analysis is β=0, slope is 0

## Since θ0 = 0 the following code reproduces the reported t-values:

# extract coefficients
theta <- coef(m)

# extract standard errors
se <- sqrt(diag(vcov(m)))
vcov(m)
diag(vcov(m))


# t-value
t_value <- theta / se

t_value


# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)
p_beta


# visualize ---------------------------------------------------------------


eps0 <- round(resid(m),4)
eps0

mean(eps == eps0)
ss <- sum(eps^2)
ss

# add error column
df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) + 
  geom_segment(aes(x = conductivity, 
                   xend = conductivity,
                   y = biomass, 
                   yend = biomass - eps),
               linetype = "solid")


## coefficient of determination portion of variance explained
summary(m)
##r^2= .889
