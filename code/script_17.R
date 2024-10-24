
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# Task 1 ---------------------------------------------------------------------
df_iris <- iris

## create three separate data frames and perform regression for each species separately

m_s <- lm(Sepal.Width ~ Petal.Width,
        data = iris %>% 
          filter(Species == "setosa"))

m_v <- lm(Sepal.Width ~ Petal.Width,
        data = iris %>% 
          filter(Species == "virginica"))
m_ve <- lm(Sepal.Width ~ Petal.Width,
          data = iris %>% 
            filter(Species == "versicolor"))


m0 <- lm(Sepal.Width ~ 1,
        data = iris)

m1 <- lm(Sepal.Width ~ Petal.Width + Petal.Length + Species,
         data = iris)
(minteraction <- lm(Sepal.Width ~ Petal.Width + Petal.Length + Species,
                   data = iris))

summary(m0)
summary(m1)
summary(minteraction)




# Task 2 ------------------------------------------------------------------




## Calculate r^2 value

# residual variance
ss <- sum(resid(m_s)^2)
v_x <- df_iris %>% pull(Sepal.Width)
v_y <- df_iris %>% pull(Sepal.Length)

# null variance
ss_0 <- sum((v_y - mean(v_y))^2)

# coefficient of determination
r2 <- 1 - ss / ss_0

print(r2)

## for loop 
df_set <- iris %>% 
  filter(Species == "setosa")

ss_null <- sum(with(df_set, (Sepal.Width - mean(Sepal.Width)))^2)
ss <- sum(lm(Sepal.Width ~ )