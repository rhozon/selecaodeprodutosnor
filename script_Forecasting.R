

# Coleta os dados


library(quantmod)
library(dygraphs)

CORN <- getSymbols(params$symbol1, auto.assign = FALSE,
                   from = "1994-01-01", end = Sys.Date())

CN21.CBT <- getSymbols(params$symbol2, auto.assign = FALSE,
                       from = "2018-01-01", end = Sys.Date())

Cz21.CBT <- getSymbols(params$symbol3, auto.assign = FALSE,
                       from = "2018-01-01", end = Sys.Date())

CU21.CBT <- getSymbols(params$symbol4, auto.assign = FALSE,
                       from = "2018-01-01", end = Sys.Date())

CH22.CBT <- getSymbols(params$symbol5, auto.assign = FALSE,
                       from = "2018-01-01", end = Sys.Date())

CK22.CBT <- getSymbols(params$symbol6, auto.assign = FALSE,
                       from = "2018-01-01", end = Sys.Date())

ZC_F <- getSymbols(params$symbol7, auto.assign = FALSE,
                   from = "2018-01-01", end = Sys.Date())

# ===============================================================

# Carrego as biblios

library(tidyverse)
library(tidyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(forecast)
library(prophet)
library(vars)
library(tseries)
library(DT)
library(fpp2)
library(tsbox)
library(lubridate)
library(data.table)


# ===============================================================

glimpse(CORN) # Perfil dos dados

dygraph(Cl(CORN))


# Manipulo pra selecionar as vars necessarias 

CORN_df <- as.data.table(CORN)
glimpse(CORN_df)

CORN_df <- CORN_df %>%
  rename(Date = "index")

glimpse(CORN_df)


# Transformo em serie temporal

CORN <- ts(CORN_df[-1], start = c(1994, 1), end = c(2021, 5), frequency = 12)

ggplotly(
autoplot(CORN[,"CORN.Close"]) # Preços do ativo
)

ggplotly(
  autoplot(Delt(log(CORN[,"CORN.Close"])))+ # Retornos das cotações de fechamento 
  abline(h = 0, col = "red")
    )


head(CORN)

tail(CORN)

glimpse(CORN)


# Inspeção do padrão sazonal da série de preços


ggplotly(
ggsubseriesplot(CORN[, "CORN.Close"]) +
  ylab("US$/bushel") +
  ggtitle("Seasonal subseries plot: CORN.Close"))

# Gráfico do preço x volume

ggplotly(
autoplot(CORN[,c("CORN.Close","CORN.Volume")], facets=TRUE) +
  xlab("Período") + ylab("") +
  ggtitle("Preço e Volume de Negociação do ativo CORN")
)

ggplotly(
qplot(CORN.Close, CORN.Volume, data=as.data.frame(CORN)) +
  ylab("Cotação") + xlab("Volume negociado"))


GGally::ggpairs(as.data.frame(CORN[,c("CORN.Close","CORN.Volume")]))


# Decomposição 

#  The classical approach to this task, implemented in the function decompose(), is to take a simple symmetric filter as illustrated above for extracting the trend and derive the seasonal component by averaging the trend-adjusted observations from corresponding periods. A more sophisticated approach that also accommodates time-varying seasonal components is seasonal decomposition via loess smoothing (Cleveland, Cleveland, McRae, and Terpenning 1990). It is available in the function stl() and iteratively finds the seasonal and trend components by loess smoothing of the observations in moving data windows of a certain size. Both methods are easily applied in R using (in keeping with the original publication, we employ logarithms)
dd_stl_Close <- stl(log(CORN[, "CORN.Close"]), s.window = 13)
plot(dd_stl)
dd_stl_Volume <- stl(log(CORN[, "CORN.Volume"]), s.window = 13)
plot(dd_stl_Volume)

# =============================================================

# Padrão de estacionariedade das séries

# test for unit root and number of differences required, you can also test for seasonality with nsdiffs
ndiffs(CORN[,"CORN.Close"], alpha=0.05, test=c("kpss"))
ndiffs(CORN[,"CORN.Volume"], alpha=0.05, test=c("kpss"))

# Função log retornos
ret<-function(x,k=1){
  return(diff(log(x),k))
}

retornos_precos <- ret(CORN[,"CORN.Close"])

plot(retornos_precos)

retornos_volume <- ret(CORN[,"CORN.Volume"])

plot(retornos_volume)

library(lmtest)
grangertest(CORN[,"CORN.Close"] ~ CORN[,"CORN.Volume"], order=4)
grangertest(CORN[,"CORN.Volume"] ~ CORN[,"CORN.Close"], order=4)

 # Funções de autocorrelação
ggplotly(
  ggAcf(CORN[,"CORN.Close"]) 
)

ggplotly(
  ggAcf(CORN[, "CORN.Volume"])
)

# Testes de raiz unitária a integração das séries

Acf(CORN[,"CORN.Close"]) # Função de autocorrelação
pacf(CORN[,"CORN.Close"]) # Função de autocorrelação parcial
Acf(diff(CORN[,"CORN.Close"])) # Função de autocorrelação
pacf(diff(CORN[,"CORN.Close"])) # Função de autocorrelação parcial


Acf(CORN[,"CORN.Volume"])
pacf(CORN[,"CORN.Volume"])
Acf(diff(CORN[,"CORN.Volume"]))
pacf(diff(CORN[,"CORN.Volume"]))

adf.test(CORN[,"CORN.Close"])
adf.test(CORN[,"CORN.Volume"])
adf.test(diff(CORN[,"CORN.Close"]))
adf.test(diff(CORN[,"CORN.Volume"]))

# Teste de reversão à m[édia. The output from adf.test includes a p-value. Conventionally, if p < 0.05, the time series is likely mean reverting, whereas a p > 0.05 provides no such evidence.

pp.test(CORN[,"CORN.Close"], type = "Z(t_alpha)")
pp.test(CORN[,"CORN.Volume"], type = "Z(t_alpha)")
pp.test(diff(CORN[,"CORN.Close"]), type = "Z(t_alpha)")
pp.test(diff(CORN[,"CORN.Volume"]), type = "Z(t_alpha)")


# Teste de estacionariedade em nível

kpss.test(CORN[,"CORN.Close"])
kpss.test(CORN[,"CORN.Volume"])
#Hence the KPSS test also points to nonstationarity of the pepper price series. (Again, a warning is issued, as the p value is interpolated from the four critical values provided by Kwiatkowski et al. 1992, it is suppressed here.)


Box.test(CORN[,"CORN.Close"], lag = 24, fitdf = 0, type = "Lj") # a p−value greater than 0.05 suggests that the data are not significantly different from white noise. (ou seja, se o valor-p da Ljung-Box for maior que 0.05 ou 5% temos uma série do tipo ruído branco)
Box.test(CORN[,"CORN.Volume"], lag = 24, fitdf = 0, type = "Lj")

# Teste de estacionariedade em primeiras diferenças

Box.test(diff(CORN[,"CORN.Close"]), lag = 24, type="Ljung-Box")
ggAcf(diff(CORN[,"CORN.Close"]))

Box.test(diff(CORN[,"CORN.Volume"]), lag = 24, type="Ljung-Box")
ggAcf(diff(CORN[,"CORN.Volume"]))

# Gráficos em nível e em diferenças

ggplotly(
  autoplot(diff(CORN[,"CORN.Close"]))+
   xlab("Período") + ylab("") +  
   ggtitle("Preço em primeira diferença do ativo CORN")
)

ggplotly(
  autoplot(diff(CORN[,"CORN.Volume"]))+
    xlab("Período") + ylab("") +  
    ggtitle("Volume de negociação em primeira diferença do ativo CORN")
)


# Teste de correlação cruzada entre volume e preco
Ccf(diff(CORN[,"CORN.Close"]), diff(CORN[,"CORN.Volume"]), main = "Fechamento vs. Volume")

#Every vertical line shows the correlation between the two time series at some lag, as indicated along the x-axis. If a correlation extends above or below the dotted lines, it is statistically significant.
#Notice that the correlation at lag 12 is aprox –0.11, which is the simple correlation between the variables
#Evidently there is some “ripple effect” in the day-to-day volume and commoditie price because changes today are correlated with changes tomorrow. Discovering this sort of relationship is useful to short-term forecasters such as market analysts and bond traders.



# Avaliação da presença de cointegração entre volume e preço

# The very nature of the two pepper series already suggests that they possess common features. Having evidence for nonstationarity, it is of interest to test for a common nonstationary component by means of a cointegration test. A simple method to test for cointegration is the two-step method proposed by Engle and Granger (1987). It regresses one series on the other and performs a unit root test on the residuals. This test, often named after Phillips and Ouliaris (1990), who provided the asymptotic theory, is available in the function po.test() from the package tseries. 
po.test(log(CORN[,5:6]))

# suggesting that both series are not cointegrated in loglevel

# The standard tests proceeding in a symmetric manner stem from Johansen’s full-information maximum likelihood approach (Johansen 1991). For a pth-order cointegrated vector autoregressive (VAR) model, the error correction form is (omitting deterministic components)
# Teste JJ

# The relevant tests are available in the function ca.jo() from the package urca. The basic version considers the eigenvalues of the matrix Π in the preceding equation. We again refer to Hamilton (1994) for the methodological background. Here, we employ the trace statistic—the maximum eigenvalue, or “lambdamax”, test is available as well—in an equation amended by a constant term (specified by ecdet = "const"), yielding 

library(urca)
teste_jj <- ca.jo(log(CORN[,5:6]), ecdet = "const", type = "trace")
summary(teste_jj)

# The null hypothesis of no cointegration is rejected; hence the Johansen test confirms the results from the initial two-step approach.


# Teste de Quebra Estrutural

dd_ocus <- efp(log(CORN[,"CORN.Close"]) ~ log(CORN[, "CORN.Volume"]), data = CORN, type = "OLS-CUSUM")
sctest(dd_ocus)

# The associated structural change test, by default considering the maximum absolute deviation of the empirical fluctuation process from zero and given by is significant at the default 5% level, signaling that the model parameters are not stable throughout the entire sample period. 

plot(dd_ocus)

# Teste de Chow

# Tests based on F statistics, the second class of tests in strucchange, are designed to have good power for single-shift alternatives (of unknown timing). The basic idea is to compute an F statistic (or Chow statistic) for each conceivable breakpoint in a certain interval and reject the null hypothesis of structural stability if any of these statistics (or some other functional such as the mean) exceeds a certain critical value (Andrews 1993; Andrews and Ploberger 1994). Processes of F statistics can be fitted with Fstats(), employing an interface similar to efp(). The resulting “Fstats” objects can again be assessed by the corresponding sctest() method or graphically by the plot() method. The code chunk

dd_fs <- Fstats(log(CORN[,"CORN.Close"]) ~ log(CORN[, "CORN.Volume"]), data = CORN, from = 0.1)
plot(dd_fs)
sctest(dd_fs)

# Modelo Dating Structural Changes

# Returning to the CORN series, we estimate the breakpoints for a SARIMA-type model with a minimal segment size of 10% using
dd_bp <- breakpoints(log(CORN[,"CORN.Close"]) ~ log(CORN[, "CORN.Volume"]), data = CORN, h = 0.1)
plot(dd_bp)

# The RSS and BIC as displayed by plot(dd_bp) are shown in the left panel of Figure 6.12. Although the RSS drops clearly up to m = 3 breaks, the BIC is minimal for m = 0 breaks. This is not very satisfactory, as the structural change tests clearly showed that the model parameters are not stable. As the BIC was found to be somewhat unreliable for autoregressive models by Bai and Perron (2003), we rely on the interpretation from the visualization of the structural change tests and use the model with m = 2 breaks. Its coefficients can be extracted via 

coef(dd_bp, breaks = 2)

# The observed and fitted series, along with confidence intervals for the breakpoints, are shown in the right panel of Figure 6.12 as generated by

plot(log(CORN[,"CORN.Close"]))
lines(fitted(dd_bp, breaks = 2), col = 4)
lines(confint(dd_bp, breaks = 2))

# Estrutura da Série Temporal

# Structural time series models are state-space models utilizing a decomposition of the time series into a number of components that are specified by a set of disturbance variances. Thus, these models may be considered error component models for time series data. Harvey (1989) and Durbin and Koopman (2001) are standard references.

dd_struct_Close <- StructTS(log(CORN[,"CORN.Close"]))

plot(cbind(fitted(dd_struct_Close), residuals(dd_struct_Close)))

dd_struct_Vol <- StructTS(log(CORN[,"CORN.Volume"]))

plot(cbind(fitted(dd_struct_Vol), residuals(dd_struct_Vol)))



# ===============================================================


# Testagem de modelos
CORN <- ts(CORN, start = c(1994, 1), end = c(2021, 5), frequency = 12)


# Separo treino e teste

treinoa <- subset(CORN, end = length(CORN) - 24)

treino <- window(log(CORN[,c("CORN.Close","CORN.Volume")]), end = c(2019,4)) # Pego o período onde o gráfico aponta inversão de trajetoria
tail(treino)

teste <- window(log(CORN[,c("CORN.Close","CORN.Volume")]), start = 2020)
tail(teste)

glimpse(treino)

# Comparo os modelos 

horizonte <- 24 # Horizonte de projeção para os proximos 24 meses

SNAIVE <- snaive(diff(CORN[,"CORN.Close"]), h = horizonte)
accuracy(SNAIVE)
summary(SNAIVE)
autoplot(SNAIVE)
checkresiduals(SNAIVE) # In this case, the Ljung-Box test has a p−value well above the 0.05 threshold, so there is no problem with the autocorrelations - they look like what you would expect from white noise. That is confirmed by the ACF plot. The histogram looks pretty close to a normal curve, although there is possibly one outlier on the negative side. Make a habit of always checking your residuals before proceeding to produce the forecasts.

SES <- ses(diff(CORN[,"CORN.Close"]), h = horizonte) # Simple Exponential Smoothing
accuracy(SES)
summary(SES)
autoplot(SES)+autolayer(fitted(SES))
checkresiduals(SES)

HOLT <- holt(diff(CORN[,"CORN.Close"]), h = horizonte) 
accuracy(HOLT)
summary(HOLT)
autoplot(HOLT)+autolayer(fitted(HOLT))
checkresiduals(HOLT)

HOLT_aditivo <- hw(diff(CORN[,"CORN.Close"]), seasonal = "additive", h = horizonte)
accuracy(HOLT_aditivo)
summary(HOLT_aditivo)
autoplot(HOLT_aditivo)+autolayer(fitted(HOLT_aditivo))
checkresiduals(HOLT_aditivo)

ARIMA <- auto.arima(diff(CORN[,"CORN.Close"]), stepwise = FALSE)
accuracy(ARIMA)
summary(ARIMA)
ARIMA %>% forecast() %>% autoplot()
checkresiduals(ARIMA)

TBATS <- tbats(diff(CORN[,"CORN.Close"]))
accuracy(TBATS)
summary(TBATS)
TBATS %>% forecast() %>% autoplot()
checkresiduals(TBATS)

# Modelo NNETAR

NNETAR <- nnetar(CORN[,"CORN.Close"], lambda=0)
accuracy(NNETAR)
summary(NNETAR)
autoplot(forecast(NNETAR,h=horizonte))
checkresiduals(NNETAR)

# Modelo VAR

CORN[1:5,5:6] # Checagem das colunas necessarias

VARselect(CORN[,5:6], lag.max=8, type="const")[["selection"]] # Critérios de defasagem do VAR

var1 <- VAR(CORN[,5:6], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic") # The null hypothesis of no autocorrelation is not rejected since the p-value of 0.4484 is higher than the significance level of 0.05

var2 <- VAR(CORN[,5:6], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

var3 <- VAR(CORN[,5:6], p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")

forecast(var3) %>%
  autoplot() + xlab("Mês")


# Estimativa do modelo VAR

DCORN.Volume <- diff(CORN[,"CORN.Volume"])
DCORN.Close <- diff(CORN[,"CORN.Close"])
varmat <- as.matrix(cbind(DCORN.Volume,DCORN.Close))
varfit <- VAR(varmat) # `VAR()` from package `vars`
summary(varfit)

# Estimativa da Impulso-Resposata

impresp <- irf(varfit)
plot(impresp)
# The interpretation of Figures 13.4 is straightforward: an impulse (shock) to  Dc  at time zero has large effects the next period, but the effects become smaller and smaller as the time passes. The dotted lines show the 95 percent interval estimates of these effects. The VAR function prints the values corresponding to the impulse response graphs.

fevd(varfit) # Decomposicao da variancvia

plot(fevd(varfit))
# Forecast variance decomposition estimates the contribution of a shock in each variable to the response in both variables. Figure 13.5 shows that almost 100 percent of the variance in  Dc  is caused by  Dc  itself, while only about 80 percent in the variance of  Dy  is caused by  Dy  and the rest is caused by  Dc . The  R  function fevd() in package vars allows forecast variance decomposition.

# Modelo GARCH

library(fGarch)

GARCH11 <- garchFit(data = CORN[,"CORN.Close"], trace = FALSE)

# Comparo o ajuste de um SNAIVE

training <- window(diff(CORN[,"CORN.Close"]), end = c(2020,12))
test <- window(diff(CORN[,"CORN.Close"]), start = c(2021,1))
snaive_treino <- snaive(training, h = horizonte)
autoplot(snaive_treino) + autolayer(test, series = "Test data")
as_tibble(accuracy(snaive_treino, test))

naive_treino <- naive(training, h = horizonte)
autoplot(naive_treino) + autolayer(test, series = "Test data")
as_tibble(accuracy(naive_treino, test))
# As medidas do conjunto de treinamento são baseadas nos resíduos, enquanto as medidas do conjunto de teste são baseadas nos erros de previsão. Na maioria dos casos, estamos interessados nas medidas de erro do conjunto de teste. Por si só, eles não nos dizem muito. Mas quando comparamos diferentes métodos de previsão nos mesmos dados, eles serão muito úteis para nos dizer o que funciona e o que não funciona.


# Estou calculando a raiz do erro quadrático médio para diferentes horizontes de previsão com base na validação cruzada de séries temporais.

sq <- function(u){u^2}
for(h in 1:24)
{
  diff(CORN[,"CORN.Close"]) %>%
    tsCV(forecastfunction = snaive, h = h) %>%
    sq() %>% 
    mean(na.rm = TRUE) %>%
    print()
}

# No entanto, eu precisei escrever minha própria função quadrática porque a abordagem usual não funciona dentro de uma sequência de pipes. Além disso, preciso terminar com o comando print ou nada será impresso. Observe como o RMSE aumenta com o horizonte de previsão. Quanto mais adiante projetamos, menos precisas são nossas previsões.
# Em resumo, a validação cruzada de séries temporais é muito útil para selecionar um bom modelo de previsão. Em geral, uma boa regra é escolher o modelo com o menor RMSE. Se um determinado horizonte de previsão for de interesse, calcule o RMSE com validação cruzada nesse horizonte. Dessa forma, você está escolhendo o melhor modelo de previsão para o seu propósito.
































