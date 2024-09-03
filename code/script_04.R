
# Setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

iris <- as_tibble(iris)


# ggplot ------------------------------------------------------------------


##scatter plot
  iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point()
## Colored scatter plot.Color by species
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point()

## Changing color uniformly across species
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,)) +
  geom_point(color = "red")

## Plotting data frame
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

# Line graph 
df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line( color = "purple") +
  geom_point()
 
## histograms

iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(color = "yellow")

## changing bin width of histograms
iris %>% 
  ggplot(aes(x = Sepal.Length,)) +
  geom_histogram(color = 'purple', binwidth = 0.3)

iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50)
## box plot 

iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             color = Species)) +
  geom_boxplot()

## filling the box plot with colors by species
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot()
##transparency 
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species,)) +
  geom_boxplot(alpha = .66)

## adjusting the color of the boarders 
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(color = "darkblue", alpha = .66)


# favorite figures  -------------------------------------------------------
# heat map ----------------------------------------------------------------

# The mtcars dataset:
data <- as.matrix(mtcars)

# Default Heatmap
heatmap(data)
# Use 'scale' to normalize
heatmap(data, scale="column")
# No dendrogram nor reordering for neither column or row
heatmap(data, Colv = NA, Rowv = NA, scale="column")

# 1: native palette from R
heatmap(data, scale="column", col = cm.colors(256))
heatmap(data, scale="column", col = terrain.colors(256))

# 2: Rcolorbrewer palette
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data, scale="column", col = coul)
# violin -----------------------------------------------------------------


# Library
library(ggplot2)

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Most basic violin chart
p <- ggplot(data, aes(x=name, y=value, fill=name)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()
# Let's use the iris dataset as an example:
data_wide <- iris[ , 1:4]
library(tidyr)
library(ggplot2)
library(dplyr)
data_wide %>% 
  gather(key="MesureType", value="Val") %>%
  ggplot( aes(x=MesureType, y=Val, fill=MesureType)) +
  geom_violin()
