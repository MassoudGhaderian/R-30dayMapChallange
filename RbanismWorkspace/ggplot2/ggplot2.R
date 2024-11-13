install.packages("lintr")
install.packages("styler")

install.packages("ggplot2")

library(ggplot2)

head(mtcars)

summary(mtcars)

ggplot(data = mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "MPG vs, Weight of Cars",
       x = "Weight(1000 1bs)",
       y = "meiles per gallon") +
  theme_minimal()


ggplot(data = mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_line() +
  labs(title = "MPG vs, Weight of Cars",
       x = "Weight(1000 1bs)",
       y = "meiles per gallon") +
  theme_minimal()

ggplot(data = mtcars, aes(x = factor(cyl))) +
  geom_bar() +
  labs(title = "Count of Cars by Cylinder",
       x = "Number of Cylinders",
       y = "Count") +
  theme_minimal()

