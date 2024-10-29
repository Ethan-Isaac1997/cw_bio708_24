# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))





# 7.3.1 Normality Assumption ----------------------------------------------
help("shapiro.test")

# convert the data format to tibble
iris <- as_tibble(iris)

m_iris <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
eps <- resid(m_iris)

shapiro.test(eps)










# 7.3.2 Model Interpretation ----------------------------------------------
## “each species has a distinct intercept value.”

m_iris <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
summary(m_iris)

b <- coef(m_iris)
a<- NULL
#intercept setosa
a[1] <-  b[2]
#intercept for versicolor
a[2] <- b[1] +b[3]
#intercept for virginica
a[3] <- b[1] + b[4]


#plot
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))
y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)
iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))
 


# 7.3.3 Alternative Model -------------------------------------------------
## this shows the importance of showing all variables
p_iris <- lm(Petal.Length ~ Petal.Width, data = iris)

b <- coef(p_iris)
a<- NULL

#intercept setosa
a[1] <-  b[2]
#intercept for versicolor
a[2] <- b[1] +b[3]
#intercept for virginica
a[3] <- b[1] + b[4]


#plot
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                each = n_rep))
y_pred <- predict(p_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y =  Petal.Length)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))

