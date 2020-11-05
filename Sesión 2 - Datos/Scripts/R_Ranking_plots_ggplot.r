
# Load the required libraries
# NOTE: Please ignore the warning message.
library(ggplot2)  
library(dplyr)
# Subset data to select top 20 counties in OH
data(midwest)
# Loading data into a variable
midwest.data <- midwest
top_20 <- midwest.data %>%
# Filter data by state
        filter(state == "OH") %>%
        select(county, percollege) %>%
# Arrange in descending order according to percollege number
        arrange(desc(percollege)) %>%
# Selecting the top 20
        top_n(20) %>%
# Order the counties by percent college educated
        arrange(percollege) %>% 
# Make the county variable a factor with the levels ordered accordingly
        mutate(county = factor(county, levels = .$county)) 

# Ordered bar chart
ggplot(top_20, aes(county, percollege)) +
        geom_bar(stat = "identity") +
        coord_flip()+ 
        labs(title="Ordered bar plot", 
         subtitle="Top 20 counties in Ohio for college educated students",
         caption="Dataset: midwest", x="County", y="Percollege")

# Dot plot
ggplot(top_20, aes(percollege, county)) +
        geom_point()+ 
        labs(title="Dot Plot", 
         subtitle="Top 20 counties in Ohio for college educated students",
         caption="Dataset: midwest", x="Percollege", y="County")

# Lollipop chart
ggplot(top_20, aes(percollege, county)) +
        geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
        geom_point()+ 
        labs(title="Lollipop chart", 
         subtitle="Top 20 counties in Ohio for college educated students",
         caption="Dataset: midwest", x="Percollege", y="County")
