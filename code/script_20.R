# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))





# binomial model ----------------------------------------------------------
df_mussel <- read_csv(here::here("data_raw/data_mussel.csv"))
print(df_mussel)
# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line() +
  labs(y = "x",
       x = "logit(x)")





cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized) %>% 
  head()

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

## y_binom is inv.logit-transformed because predict() returns values in logit-scale
# make prediction
df_pred <- tibble(density = seq(min(df_mussel$density),
                                max(df_mussel$density),
                                length = 100))

# y_binom is inv.logit-transformed because predict() returns values in logit-scale
y_binom <- predict(m_binom, newdata = df_pred) %>% boot::inv.logit()

df_pred <- df_pred %>% 
  mutate(y_binom)

# calculate the proportion of fertilized eggs
df_mussel <- df_mussel %>% 
  mutate(prop_fert = n_fertilized / n_examined)

# plot
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")

 m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")
 
 summary(m_binom)

 
 
 ### make prediction
 df_pred <- tibble(density = seq(min(df_mussel$density),
                                 max(df_mussel$density),
                                 length = 100))
 
 df_pred <- df_mussel %>% 
   reframe(density = seq(min(density),
                         max(density),
                         length = 100))
## prediction
y <-  predict(m_binom,
         newdata = df_pred) %>% 
  boot::inv.logit()

y 
## different function same product##
my.inc.logit <- function(x) {
  exp(x) / (1+exp(x))
}


#draw predicted line

df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")  +
  geom_line(data = df_pred,
            aes(y = y_binom)) ## added section geom_point() +geom_line(data = df_pred,aes(y = y_binom))







# binomial distribution with variable number of trials --------------------

## create fake data with variable numbers

df_mussle <- df_mussel %>% 
  mutate(n_examined = rpois(nrow(.), lambda = 40))


## THIS code accounts for variability 
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")
