a <- 3
a
(a <- 3)

(b <- c(1, 2, 3))
str(b)
(c <- c("a", "b", "c"))
str(c)
(d <- factor(c))

(df <- data.frame(b, c, d))
str(df)
a <- mean(x = c(1, 2, NA, 4), na.rm = TRUE)

iris <- iris

mean(iris$Sepal.Length[iris$Species == "setosa"])

library(tidyverse)
iris %>%
  filter(Species == "setosa") %>%
  pull(Sepal.Length) %>%
  mean()


data.frame(iris)
library(tidyverse)
iris %>% tibble()
