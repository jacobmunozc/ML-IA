# Plotly

wine <- read.csv('https://raw.githubusercontent.com/jacobmunozc/ML-IA/main/Sesi%C3%B3n%203%20-%20Regresi%C3%B3n%20y%20clasificaci%C3%B3n%201/Data/wine.csv')
winequality <- read.csv('https://raw.githubusercontent.com/jacobmunozc/ML-IA/main/Sesi%C3%B3n%203%20-%20Regresi%C3%B3n%20y%20clasificaci%C3%B3n%201/Data/winequality-red.csv')

require(ggplot2)
gg <- ggplot(wine, aes(Proline,Flavanoids,color = factor(Type))) + geom_point()
gg

require(plotly)
ggplotly(gg)

require(dplyr)
wine %>% 
  count(Type) %>% 
  plot_ly(x = ~Type, y = ~n) %>% 
  add_bars()


require(forcats)
wine %>% 
  count(Type) %>% 
  mutate(Type = fct_reorder(factor(Type),n,.desc = TRUE)) %>% 
  plot_ly(x = ~Type, y = ~n) %>% 
  add_bars()


wine %>% 
  plot_ly(x = ~Total.phenols) %>% 
  add_histogram()


wine %>% 
  plot_ly(x = ~Total.phenols) %>% 
  add_histogram(nbinsx = 10)

wine %>% 
  plot_ly(x = ~Total.phenols) %>% 
  add_histogram(nbinsx = list(start = .0, end = 4, size = .25))



winequality %>% 
  plot_ly(x = ~residual.sugar, y = ~fixed.acidity) %>% 
  add_markers()


table(winequality$quality)

winequality %>% 
  mutate(Type = ifelse(5*row_number() < nrow(winequality),'Tinto','Blanco'),
         quality_label = case_when(
           quality %in% 3:4 ~ 'Baja',
           quality %in% 5:7 ~ 'Media',
           TRUE ~ 'Alta'
         )) %>%
  count(Type, quality_label) %>% 
  plot_ly(x = ~Type, y = ~n, color = ~quality_label) %>% 
  add_bars() %>% 
  layout(barmode = 'stack')

winequality %>% 
  mutate(quality_label = case_when(
           quality %in% 3:4 ~ 'Baja',
           quality %in% 5:7 ~ 'Media',
           TRUE ~ 'Alta'
         )) %>% 
  plot_ly(x = ~quality_label, y = ~alcohol) %>% 
  add_boxplot()





