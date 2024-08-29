
# setup -------------------------------------------------------------------

rm(list = ls())

## install.package(tidyverse)
## add library 
library(tidyverse)
library(tidyverse)

iris <- as_tibble(iris)
iris


# row manipulation --------------------------------------------------------
## single match
filter(iris, Species == "Virginica")
## multiple match
filter(iris, Species %in% c("virginica", "versicolor"))

## except 
filter(iris, Species != "virginica")

## except multiple
filter(iris, !(Species %in% c("virginica", "versicolor")))


## greater than ">"
filter(iris, Sepal.Length > 5)

## equal & greater than ">="
filter(iris, Sepal.Length >= 5)

## less than "<"
filter(iris, Sepal.Length < 5)

## equal & less than "<="
filter(iris, Sepal.Length <= 5)

##Arrange

##arrange in an ascending order
arrange(iris, Sepal.Length)

## arrange in an descending order
arrange(iris, desc(Sepal.Length))


# column Manipulation -----------------------------------------------------

## select one column

select(iris, Sepal.Length)

## select multiple columns

select(iris, c(Sepal.Length, Sepal.Width))

##remove one column

select(iris, -Sepal.Length)

## remove multiple columns 

select(iris, -c(Sepal.Length, Sepal.Width))

## select/remove multiple columns with a start rule
# starts_with("x")

select(iris, starts_with("Sepal"))
select(iris, -starts_with("Sepal"))


## select/remove multiple columns with an end rule
# ends_with("x")

select(iris, ends_with("Width"))
select(iris, -ends_with("Width"))
       
##Mutate

# add a new column
x <- 1:150
mutate(iris, id = x)





# piping ------------------------------------------------------------------

## the following codes produce the same data frame
# apply functions separately
df_vir <- filter(iris, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)
print(df_vir_sl)


## piping 

iris %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)


# reshape -----------------------------------------------------------------


##pivot wider
iris_w <- iris %>% 
  mutate(id =rep(1:50, 3)) %>%
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id",
              values_from = "Sepal.Length",
              names_from = "Species")
print(iris_w)


##pivot_longer

iris_l <- iris_w %>% 
  pivot_longer(cols = c("setosa",
                        "versicolor",
                        "virginica"), 
               names_to = "Species", 
               values_to = "Sepal.Length") 

print(iris_l)


# Group Operation ---------------------------------------------------------
## grouping by "Species"
df_g <- iris %>% 
group_by(Species) %>% 
summarize(mu_sl = mean(Sepal.Length))

print(df_g)

## group by species then take mu and sd
df_g2 <- iris %>% 
group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length),
            sigma_sl = sd(Sepal.Length)) %>% 
  ungroup()

print(df_g2)
