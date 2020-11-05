
# Load the required library
# NOTE: Please ignore the warning message.
library(ggfortify)
# Time series plot from a time series object (ts)
data(USAccDeaths)
# Reading data into a variable
usaccdeaths.data <- USAccDeaths
autoplot(usaccdeaths.data) +
  labs(title="Total of accidental deaths in USA",caption="Dataset: USAccDeaths", x="Year", y="Total Deaths") 

# Load the required libraries
# NOTE: Please ignore the warning message.
library(astsa)
library(ggplot2)
# Time series plot for two time series
timeseries.data = data.frame(Time=c(time(gtemp_land)), gtemp=c(gtemp_ocean), gtempl=c(gtemp_land))
ggplot(data = timeseries.data, aes(x=Time, y=value, color=variable ))+
              ylab('Temperature Deviations')+                                 
              geom_line(aes(y=gtemp , col='Ocean Only'), size=1, alpha=.5)+
              geom_line(aes(y=gtempl, col='Land Only'),  size=1, alpha=.5)+ 
  labs(title="Plotting two time series at the same time",caption="Dataset: gtemp", x="Year") 
