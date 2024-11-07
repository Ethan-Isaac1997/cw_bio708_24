# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))



# Lab Data --------------------------------------------------------------------

df_vpart <- read_csv(here::here("data_raw/data_vpart.csv"))
print(df_vpart)


# 8.4.1 GLM Model ---------------------------------------------------------------
 


with(df_vpart, mean(n_sp))
with(df_vpart, var(n_sp))

m_pois <- glm(n_sp ~ distance + cat_area + hull_area, data = df_vpart,
              family = "poisson")


summary(m_pois)

df_pred <- df_vpart %>% 
  reframe(distance = seq(min(distance),
                         max(distance),
                         length = 100),
          cat_area = mean(cat_area),
          hull_area = mean(hull_area)) %>% 
          mutate(log_y_pred = predict(m_pois,
                                      newdata = .),
                 y_pred = exp(log_y_pred))

## plotting
g_fish<- df_vpart %>% 
  ggplot(aes(x = distance,
             y = n_sp)) +
geom_point() +
  geom_line(data = df_pred,
            aes(y = y_pred)) + 
  labs(y = "fish species richness",
       x = "Distance to sea") +
  theme_bw()



# 8.4.2 effects on size --------------------------------------------------------------

 glm(df_vpart$n_sp ~ scale(df_vpart$distance) + scale(df_vpart$cat_area) +scale(df_vpart$hull_area),
    family = "poisson")

m_std <- glm(n_sp ~ scale(distance) + scale(cat_area) +scale(hull_area),
    family = "poisson",
    data = df_vpart)
