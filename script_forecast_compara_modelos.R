
# Script comparativo das acuracias Prophet x ARIMA x ETS x 


library(quantmod)

CORN <- getSymbols("CORN", auto.assign = FALSE,
                   from = "1994-01-01", end = Sys.Date())
glimpse(CORN)

library(tsibble)
library(tsibbledata)
library(dplyr)
library(tidyverse)
library(data.table)

CORN <- as.data.table(CORN)
CORN <- as_tsibble(CORN)
glimpse(CORN)
class(CORN)

# Tira os gaps implicitos

library(tidyr)

CORN <- CORN %>%
  fill_gaps() %>%
  fill(c(CORN.Open, CORN.High, CORN.Low, CORN.Close, CORN.Volume, CORN.Adjusted), .direction = "down")
print(CORN)

library(fabletools)
autoplot(CORN, CORN.Close)

#-----------------------
#Treino

library(fpp3)

glimpse(CORN)


treino <- CORN %>%
  filter(year(index) <= 2020)

# Tira os gaps implicitos

treino <- treino %>%
  fill_gaps() %>%
  fill(c(CORN.Open, CORN.High, CORN.Low, CORN.Close, CORN.Volume, CORN.Adjusted), .direction = "down")
print(treino)




ajuste <- treino %>%
   model(
    arima = ARIMA(CORN.Close, stepwise = FALSE, approximation = FALSE),
    ets = ETS(CORN.Close),
    stl = STL(CORN.Close),
    theta = THETA(CORN.Close),
    snaive = SNAIVE(CORN.Close),
    nntwk = NNETAR(CORN.Close),
    prophet = prophet(CORN.Close ~ growth("linear") + season(period = "day", order = 10) + 
                                                      season(period = "week", order = 5))
  ) %>%
  mutate(combinadas = 
           (arima + ets + stl + theta + snaive + nntwk + prophet) / 7 ) # Media aritmetica simples. Nao use a funcao mean()

ajuste_fcst <- ajuste %>%
                  forecast(h = 365)

ajuste_fcst %>%
  autoplot(CORN)
    
ajuste_fcst %>% accuracy(CORN)










