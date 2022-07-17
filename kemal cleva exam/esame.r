library(gapminder)
library(readr)
library(dplyr)
library(ggplot2)
library(modelr)

data1 <- read_csv("C:\\Users\\Kemal\\Desktop\\r exercises\\kemal cleva exam\\inequality-of-incomes-chartbook.csv")
data1t <- read_csv("C:\\Users\\Kemal\\Desktop\\r exercises\\kemal cleva exam\\inequality-of-incomes-chartbook.csv")


# Quanto è aumentata la disparità del reddito negli anni? 
# Gini ratio, è una misura della dispersione statistica destinata a rappresentare 
# la disuguaglianza di reddito
# o la disuguaglianza di ricchezza all'interno di una nazione.

ggplot(data = data1, mapping = aes(x = Year, y = Gini, color = Code)) +
  geom_point() #scatterplot delle nazioni'


# 1. Modello sui dati brutti
mod1 = lm(Gini ~ Year, data = data1)

summary(mod1)

ggplot(data1, aes(Code, Gini)) + 
  geom_boxplot()  #boxplot di Gini su nazione

mod <- lm(Gini ~ Year, data = data1) 


sim1 <- data_grid(data1, Gini)
sim1 <- add_predictions(data1, mod) 
sim1 

grid <- data1 %>% 
  data_grid(Code) %>% 
  add_predictions(mod, "Gini")
grid 

ggplot(data1, aes(x = Year,y = Gini)) +
  geom_point() + # observed values'
  geom_line(data = sim1, mapping = aes(y = pred), colour = "red")  #

# 1.2 Residuo delle nazioni su questo modello

data1 <- data1 %>% 
  add_residuals(mod)



data1 %>%
  group_by(Code) %>%
  summarise(mean = mean(resid)) %>%
  arrange(mean)


#------------------------------------------------------------seconda parte-------------------------------

data1t <- data1t %>%
           filter(Year >= 1960)

data1t

ggplot(data = data1t, mapping = aes(x = Year, y = Gini, color = Code)) +
  geom_point() #scatterplot delle nazioni'


mod1 = lm(Gini ~ Year, data = data1t)

summary(mod1)

ggplot(data1t, aes(Code, Gini)) + 
  geom_boxplot()  #boxplot di Gini su nazione

mod <- lm(Gini ~ Year, data = data1t) 


sim1 <- data_grid(data1t, Gini)
sim1 <- add_predictions(data1t, mod) 
sim1 

grid <- data1t %>% 
  data_grid(Code) %>% 
  add_predictions(mod, "Gini")
grid 

ggplot(data1t, aes(x = Year,y = Gini)) +
  geom_point() + # observed values'
  geom_line(data = sim1, mapping = aes(y = pred), colour = "red")  #

# 1.2 Residuo delle nazioni su questo modello

data1 <- data1t %>% 
  add_residuals(mod)



data1t %>%
  group_by(Code) %>%
  summarise(mean = mean(resid)) %>%
  arrange(mean)




#------------------------------------------------------------terza parte-------------------------------

data1t <- data1t %>%
  filter(Code != ARG & Code != BRA & Code != USA & Code != JPN)
  

data1t

ggplot(data = data1t, mapping = aes(x = Year, y = Gini, color = Code)) +
  geom_point() #scatterplot delle nazioni'



# 1. Modello sui dati brutti
mod1 = lm(Gini ~ Year, data = data1t)

summary(mod1)

ggplot(data1t, aes(Code, Gini)) + 
  geom_boxplot()  #boxplot di Gini su nazione

mod <- lm(Gini ~ Year, data = data1t) 


sim1 <- data_grid(data1t, Gini)
sim1 <- add_predictions(data1t, mod) 
sim1 

grid <- data1t %>% 
  data_grid(Code) %>% 
  add_predictions(mod, "Gini")
grid 

ggplot(data1t, aes(x = Year,y = Gini)) +
  geom_point() + # observed values'
  geom_line(data = sim1, mapping = aes(y = pred), colour = "red")  #

# 1.2 Residuo delle nazioni su questo modello

data1 <- data1t %>% 
  add_residuals(mod)



data1t %>%
  group_by(Code) %>%
  summarise(mean = mean(resid)) %>%
  arrange(mean)

