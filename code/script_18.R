# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))




# T-test ------------------------------------------------------------------
df_fl <- read_csv(here::here("data_raw/data_fish_length.csv"))
df_fl

# group means
v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)


# mu_a: should be identical to intercept
v_mu[1]
v_mu[2]


# mu_b - mu_a: should be identical to slope average difference 
v_mu[2] - v_mu[1]



# in lm(), letters are automatically converted to 0/1 binary variable.
# alphabetically ordered (in this case, a = 0, b = 1)
m <- lm(length ~ lake,
        data = df_fl)

summary(m)


## t.test

lake_a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)
lake_a

lake_b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)
lake_b

t.test(x = lake_b, y = lake_a, var.equal = TRUE)



# anova equivalence with lm ------------------------------------------------
df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))
print(df_anova)


# group means
v_mu <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)


## mu_1 is the intercept, mu2-mu1 is slope of b and mu3-mu1 is slope of c
print(c(v_mu[1], 
        v_mu[2] - v_mu[1], 
        v_mu[3] - v_mu[1])) 


## lm() output
m <- lm(length ~ lake,
        data = df_anova)

summary(m)

### annova 
m_aov <- aov(length ~ lake,
             data = df_anova)

summary(m_aov)



# multiple types of predictions -------------------------------------------

# convert the data format to tibble
iris <- as_tibble(iris)
print(iris)


## All the species in the data set
distinct(iris, Species)


# develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

summary(m_iris)



## create a data frame for prediction
# variable names must be identical to the original dataframe for analysis
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                           each = n_rep))

# make prediction based on supplied values of explanatory variables
y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

print(df_pred)


iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred)) # redefine y values for lines; x and color are inherited from ggplot()

 df_pred <- iris %>% 
  group_by(Species) %>% 
             reframe(Petal.Width) = seq(min(Petal.Width), max(Petal.Width), Length = 100)

