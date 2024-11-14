
# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)



# Binomial Distribution ---------------------------------------------------

### calculate the likelihood of observing the vector
y <- c(2,2, 0 ,0, 3,1,3,3,4,3)
prob <- seq(0, 1,  by =.01)
dbinom(x = y,
       size = 10,
       p = 1)

## loop version
lh <- NULL 
for (i in 1:length(prob)) {
  lh[i] <- dbinom( x = y,
                   size = 10,
                   prob = prob[i])
}



## find the best 
df_p <- tibble(prob = prob,
               lh = lh) %>% 
  arrange(desc(lh))
## sample mean

mean(y/10)

## From the set of p examined, find the parameter value that maximizes the likelihood


