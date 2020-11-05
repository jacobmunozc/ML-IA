
# Load the required libraries 
# NOTE: Please ignore the warning message.
library(ggplot2)  
library(gapminder)

# Box plot
data(gapminder)
# Load the data into a variable
gapminder.data <- gapminder
plot <- ggplot(data=gapminder.data, aes(x=continent, y=lifeExp, color=continent))
plot + 
    geom_boxplot(outlier.size=2) + 
    geom_jitter() + 
    labs(title="Box plot", 
         subtitle="Distribution of life expectancy by continent",
         caption="Source: Gapminder", x="Continent", y="Life Expectancy")

# Density plot
ggplot(data=gapminder.data, aes(x=lifeExp, fill=continent)) +
    geom_density(alpha=0.3)+ 
    labs(title="Density plot", 
         subtitle="Distribution of life expectancy by continent",
         caption="Dataset: Gapminder", x="Life Expectancy", y="Density")

# Violin plot
data(diamonds)
# Load the data into a variable
diamonds.data <- diamonds
# Plot segmented by diamond cut
ggplot(diamonds.data, aes(x = cut, y = price, fill = cut)) + 
  geom_violin() + scale_y_log10() + 
    labs(title="Violin plot", 
         caption="Dataset: diamonds", x="Cut", y="Price")
