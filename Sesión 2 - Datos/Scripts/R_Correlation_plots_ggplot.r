
# Load the required library
# NOTE: Please ignore the warning message.
library(ggplot2)  
# Scatter plot

slid.data <- carData::SLID
ggplot(slid.data, aes(education, wages)) + 
  geom_point(aes(color = language)) + 
  scale_x_continuous("Education") + 
  scale_y_continuous("Wages") + 
  theme_bw() + labs(title = "Scatterplot") + 
  facet_wrap(~language) + 
  labs(title="Scatter plot", 
       subtitle="Relationship between wages and education of people",
       caption="Dataset: SLID ")

# Load data
data(mtcars)
mtcars.data <- mtcars
# Convert cyl as a grouping variable
mtcars.data$cyl <- as.factor(mtcars.data$cyl)
# Inspect the data
head(mtcars.data[, c("wt", "mpg", "cyl", "qsec")], 4)

# Bubble chart
ggplot(mtcars.data, aes(x = wt, y = mpg)) + 
  geom_point(aes(color = cyl, size = qsec), alpha = 0.5) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 12))  + 
  labs(title = "Bubble chart",caption="Dataset: mtcars") 

# Load the required libraries
# NOTE: Please ignore the warning message
library(ggcorrplot)
library(faraway)
# Correlogram
data(ozone)
ozone.data <- ozone
# Find the correlation between variables
ozone.corr = cor(ozone.data)
# Convert the correlation between variables to heatmap
ggcorrplot(ozone.corr, hc.order = TRUE, type = "lower",
   lab = TRUE)+ 
    labs(title="Correlogram", 
         subtitle="Correlation matrix",
         caption="Dataset: Ozone ")
