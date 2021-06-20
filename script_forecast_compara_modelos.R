
# Script comparativo das acuracias Prophet x ARIMA x ETS x modelos


library(quantmod)

CORN <- getSymbols("CORN", auto.assign = FALSE,
                   from = "1994-01-01", end = Sys.Date())
glimpse(CORN)

periodicity(CORN) # avalia a disponibilidade da série

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

library(plotly)
library(fabletools)

ggplotly(
autoplot(CORN, CORN.Close) + xlab("Tempo") + ggtitle("Cotações de fechamento do ativo CORN") 
)

#-----------------------
#Treino

library(fpp3)


treino <- CORN %>%
  filter(year(index) <= 2020)

tail(treino)

# Tira os gaps implicitos da amostra de treino

treino <- treino %>%
  fill_gaps() %>%
  fill(c(CORN.Open, CORN.High, CORN.Low, CORN.Close, CORN.Volume, CORN.Adjusted), .direction = "down")
print(treino)


library(fable)
library(fable.prophet)

ajuste <- treino %>%
   model(
    arima = ARIMA(CORN.Close, stepwise = FALSE, approximation = FALSE),
    ets = ETS(CORN.Close),
    prophet = prophet(CORN.Close ~ growth("linear") + season(period = "day", order = 10) + 
                                                      season(period = "week", order = 5))
  ) %>%
  mutate(combinadas = 
           (arima + ets + prophet) / 3 ) # Media aritmetica simples. Nao use a funcao mean()

ajuste_fcst <- ajuste %>%
                  forecast(h = 365)


ajuste_fcst %>%
  autoplot(CORN, level = c(95))


ajuste_fcst %>% accuracy(CORN)










