
# Load the required library
# NOTE: Please ignore the warning message.
library(gapminder)
library(dplyr)
library(ggplot2)  

# Divering bars
data(gapminder)
# Loading data into a variable
gapminder.data <- gapminder
# Filtering by year and continent
gapminder.data <- gapminder.data %>%
  filter(year == 2007 & continent == "Europe") %>%
  mutate(median = median(gdpPercap),
# Calculate the difference between gdpPercap and the median gdpPercap
         diff = gdpPercap - median,
# If gdpPercap < median then assign it 'Below' else 'Above'
         type = ifelse(gdpPercap < median, "Below", "Above")) %>% 
# Sort the values by difference 
  arrange(diff) %>% 
  mutate(country = factor(country, levels = country))

# Plotting horizontal bar plot 
ggplot(gapminder.data, aes(x = country, y = diff, label = country)) + 
  geom_col(aes(fill = type), width = 0.5, alpha = 0.8)  +
  scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
  scale_fill_manual(labels = c("Above median", "Below median"), 
                    values = c("Above" = "#31a354", "Below" = "#000000")) + 
  labs(title = "Diverging bars",
       subtitle = "GDP per capita in 2007 for Europe",
       caption = "Dataset: Gapminder", x="Country", y="Difference") + 
  coord_flip()

# Area chart
data(airquality)
# Loading data into a variable
airquality.data <- airquality
# Grouping the mean wind by day
airquality.data %>% 
  group_by(Day) %>% 
  summarise(mean_wind = mean(Wind)) %>% 
  ggplot() + 
  geom_area(aes(x = Day, y = mean_wind)) + 
  labs(title = "Area chart of average wind per day",
       caption = "Dataset: airquality",
       y = "Mean Wind")
