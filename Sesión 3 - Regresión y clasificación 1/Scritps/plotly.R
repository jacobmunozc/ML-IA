# Plotly

wine <- read.csv('https://raw.githubusercontent.com/jacobmunozc/ML-IA/main/Sesi%C3%B3n%203%20-%20Regresi%C3%B3n%20y%20clasificaci%C3%B3n%201/Data/wine.csv')
winequality <- read.csv('https://raw.githubusercontent.com/jacobmunozc/ML-IA/main/Sesi%C3%B3n%203%20-%20Regresi%C3%B3n%20y%20clasificaci%C3%B3n%201/Data/winequality-red.csv')


gg <- ggplot(wine, aes(Proline,Flavanoids,color = factor(Type))) + geom_point()

require(plotly)

ggplotly(gg)


wine %>% 
  count(Type) %>% 
  plot_ly(x = ~Type, y = ~n) %>% 
  add_bars()


require(forecast)
wine %>% 
  count(Type) %>% 
  mutate(Type = fct_reorder(factor(Type),n,.desc = TRUE)) %>% 
  plot_ly(x = ~Type, y = ~n) %>% 
  add_bars()




