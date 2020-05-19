

data("iris")

head(iris)



library(dplyr)
library(ggplot2)
library(ggExtra)

i = iris %>% filter(Species == 'setosa' | Species == 'versicolor')

i %>% ggplot(aes(x = Sepal.Length, y = Petal.Width)) +
  geom_point() +
  facet_wrap(~ Species) +
  ggtitle('Iris Data Set', subtitle = 'ONly Two Flowers') +
  geom_smooth(method = lm, col = 'seagreen') +
  scale_x_continuous('S Length', limits = c(0,8)) +
  scale_y_continuous('P Width' , limits = c(0,2)) +
  theme_test()


cars = read.csv('Car_sales.csv')

cars %>% filter(Fuel_efficiency <= 22) %>%
  ggplot(aes(x = Manufacturer, fill = Vehicle_type)) + 
  geom_bar() +
  coord_flip() +
  facet_grid( . ~ Vehicle_type) # Change position accordingly '~' .




# Pie Chart 

cars %>% 
  ggplot(aes(x = Manufacturer, fill = Manufacturer)) +
  geom_bar() +
  coord_polar(theta = 'x')



## Histogram

cars %>% 
  ggplot(aes(x  = Length, fill = Vehicle_type)) +
  geom_histogram(alpha = 0.8, color = 'blue', aes(y  = ..density..)) +
  facet_wrap( ~ Vehicle_type) +
  geom_density(alpha = 1)



# Box plot

cars %>% 
  ggplot(aes(x = Manufacturer, y = Sales_in_thousands, fill = Manufacturer)) +
  geom_boxplot() 
 # scale_fill_manual(values = c('grey', 'grey' .... 'pink')) <-- Manualy fill col


# Violin Plot

cars %>% 
  ggplot(aes(x  = Vehicle_type, y = Fuel_capacity, fill = Vehicle_type)) +
  geom_violin(adjust = 2)


# Dot Plot

sales_avg = cars %>% 
  group_by(Manufacturer) %>% 
  summarise(avg = mean(Sales_in_thousands), )

sales_avg %>% ggplot(aes(x = avg, y = reorder(Manufacturer, avg))) +
  geom_point(color =  'red')




# Scatter plot

cars %>% 
  ggplot(aes(x = Fuel_efficiency, y = Engine_size, fill = Vehicle_type)) +
  geom_point() +
  stat_smooth() +
  facet_wrap( ~ Vehicle_type)


# 2D Dencity plot
# Log spread

cars %>%
  ggplot(aes(x = Fuel_capacity, y = Fuel_efficiency)) +
  geom_point() +
 # scale_x_log10() +
 # scale_y_log10() +
  stat_density2d(aes(color = ..level..))


# Polar plot 

cars %>%
  ggplot(aes(x = Engine_size, fill = Vehicle_type)) +
  geom_histogram(color = 'blue') +
  coord_polar() +
  facet_wrap( ~ Vehicle_type)




# Marginal Plot

margin_plot =  cars %>% 
  ggplot(aes(x = Fuel_efficiency, y = Sales_in_thousands, fill = Vehicle_type)) +
  geom_point() +
  stat_smooth() 

ggMarginal(margin_plot, type = 'histogram')
  




