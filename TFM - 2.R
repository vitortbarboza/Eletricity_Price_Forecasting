rm(list=ls()) 

##### Bibliotecas #####
library(readxl)
library(astsa)
library(forecast)
library(fUnitRoots)
library(timeDate)
library(tseries)
library(xts)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MLmetrics)
library(writexl)
library(dummies)
library(strucchange)

?auto.arima

##### Dados #####

setwd('C:/Users/temp/Desktop/Milestone/Fase 1/Dados')#computador pessoal

dados_agrup <- read_xlsx('dados_agrup_semana.xlsx')

preco = ts(dados_agrup$`Preço Médio Portugal`, 
           freq=365.25/7, 
           start=decimal_date(ymd("2018-04-01")))

preço_treino <- head(preco, round(length(preco) * 0.95))
h <- length(preco) - length(preço_treino)
preço_teste <- tail(preco, h)

lpreço_portugal <- log(preco)

lpreço_treino <- head(lpreço_portugal, round(length(lpreço_portugal) * 0.95))
h <- length(lpreço_portugal) - length(lpreço_treino)
lpreço_teste <- tail(lpreço_portugal, h)


##### Exploratory Data Analysis #####

autoplot(preco) +
  ggtitle("Preço Semanal Energia em Portugal EUR/MWh ") +
  xlab("Year") +
  ylab("Valor em Euros")

mstl(preco, lambda = "auto") %>% autoplot()+
  ggtitle('Decomposição da Série Preço Semanal Energia em Portugal EUR/MWh')+
  xlab('Year')

##### Gráficos de Autocorrelação e Estacionaridade #####

par(mfrow=c(2:1))
acf(coredata(lpreço_portugal), main="Função de Autocorrelação", 20)
pacf(coredata(lpreço_portugal),main="Função de Autocorrelação Parcial", 20)

adf.test(lpreço_portugal)

acf(coredata(diff(lpreço_portugal)), main="Função de Autocorrelação com a Série Diferenciada", 20)
pacf(coredata(diff(lpreço_portugal)),main="Função de Autocorrelação Parcial com a Série Diferenciada", 20)

acf(diff(lpreço_portugal), main="Função de Autocorrelação com a Série Diferenciada", 20)
pacf(coredata(diff(lpreço_portugal)),main="Função de Autocorrelação Parcial com a Série Diferenciada", 20)


adf.test(diff(lpreço_portugal))

##### Modeling #####


d=1
for(p in 1:3){
  for(q in 1:3){
    if(p+d+q<=11){
      model<-arima(x=lpreço_treino,order = c((p),d,(q)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}




## Model 1 ## 
model_arima1 <- auto.arima(lpreço_treino) #(2,1,2)

model_arima1

checkresiduals(model_arima1)

for (h in 1:100){
  #h# 
  show(h)
  show(Box.test(model_arima1$residuals, lag = h, type = "Ljung-Box", fitdf = 0))}

previsao_arima1 <- forecast(model_arima1, h=8)
MAPE(previsao_arima1$mean, lpreço_teste)*100 #7.41%
RMSE(previsao_arima1$mean, lpreço_teste) # 0.28

autoplot(previsao_arima1$mean)+autolayer(lpreço_teste)+
  ggtitle('Comparação entre os dados teste com a previsão')

plot(previsao_arima1)
plot(previsao_arima2)


dummy_treino <- dados_agrup$Dummy[1:152]
dummy_teste <- dados_agrup$Dummy[153:160]

model_teste <- auto.arima(lpreço_treino,xreg=reg_treino)
model_teste
previsao_teste <- forecast(model_teste, h=8,xreg=dummy_teste)
MAPE(previsao_teste$mean, lpreço_teste)*100 #7.41%

## Model 2 ##
model_arima2 <- Arima(lpreço_treino, order=c(1,1,0)) #(2,1,0)
model_arima2

checkresiduals(model_arima2)

for (h in 1:100){
  #h# 
  show(h)
  show(Box.test(model_arima2$residuals, lag = h, type = "Ljung-Box", fitdf = 0))}

previsao_arima2 <- forecast(model_arima2, h=8)
previsao_arima2

MAPE(previsao_arima2$mean, lpreço_teste)*100 #8.27%
RMSE(previsao_arima2$mean, lpreço_teste) # 0.33


model_teste <- auto.arima(lpreço_treino, d=1)

## Preparaçao Arimax ##

reg <- cbind(dados_agrup$`lEnergia total (MWh)`,
             dados_agrup$`Temperatura média (º F)`,dados_agrup$`Velocidade média do vento (mph)`)
colnames(reg) <- c("Volume Energia", "Temperatura", "Velocidade Vento")


reg <- cbind(dados_agrup$`lEnergia total (MWh)`,dados_agrup$`Velocidade média do vento (mph)`)
colnames(reg) <- c("Volume Energia", "Velocidade Vento")


reg_treino <- reg[1:152,]
reg_teste <- reg[153:160,]

for(p in 1:4){
  for(q in 1:4){
    if(p+d+q<=11){
      model<-arima(x=lpreço_treino, xreg= reg_treino,
                   order = c((p),1,(q)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}


## Model 3 ##

model_arimax1 <- Arima(lpreço_treino, order=c(0,1,2),xreg=reg_treino)
model_arimax1
checkresiduals(model_arimax1)




model_arimax1 <- auto.arima(lpreço_treino,xreg=reg_treino,
                       stepwise=FALSE,approx=FALSE)
?Arima
model_arimax1
checkresiduals(model_arimax1)

for (h in 1:100){
  #h# 
  show(h)
  show(Box.test(model_arimax1$residuals, lag = h, type = "Ljung-Box", fitdf = 0))}

previsao_arimax1 <- forecast(model_arimax1, h=8,xreg=reg_teste)

MAPE(previsao_arimax1$mean, lpreço_teste)*100 #10.35%
RMSE(previsao_arimax1$mean, lpreço_teste) # 0.44

## Model 4 ##

model_arimax2 <- Arima(lpreço_treino, order=c(2,1,2),xreg=reg_treino)
model_arimax2

checkresiduals(model_arimax2)

for (h in 1:100){
  #h# 
  show(h)
  show(Box.test(model_arimax2$residuals, lag = h, type = "Ljung-Box", fitdf = 0))}


previsao_arimax2 <- forecast(model_arimax2, h=8,xreg=reg_teste)

plot(previsao_arimax1)
plot(previsao_arimax2)

MAPE(previsao_arimax2$mean, lpreço_teste)*100 #10.27%
RMSE(previsao_arimax2$mean, lpreço_teste) # 0.34

autoplot(previsao_arimax1) + autolayer(lpreço_teste)


autoplot(previsao_arimax1$mean)+autolayer(lpreço_teste)+
  ggtitle('Comparação entre os dados teste com a previsão')

autoplot(previsao_arimax1)+autolayer(lpreço_teste)+
  ggtitle('Comparação entre os dados teste com a previsão')


previsao_arimax1 %>%
  autoplot() +
  geom_line(
    aes(
      x = as.numeric(time(lpreço_teste)),
      y = as.numeric(lpreço_teste)
    ),
    col = "red"
  )

par(mfrow=c(2:1))
shapiro.test(dados_agrup$`Preço Médio Portugal`)
shapiro.test(lpreço_portugal)

shapiro.test(dados_agrup$`Velocidade média do vento (mph)`)
shapiro.test(dados_agrup$`Temperatura média (º F)`)
shapiro.test(dados_agrup$`lEnergia total (MWh)`)

hist(dados_agrup$`Preço Médio Portugal`)
plot(dados_agrup$`Preço Médio Portugal`,dados_agrup$`Velocidade média do vento (mph)`, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)


cor.test(dados_agrup$`Preço Médio Portugal`, dados_agrup$`Velocidade média do vento (mph)`,  method="spearman")
cor.test(lpreço_portugal, dados_agrup$`Velocidade média do vento (mph)`,  method="spearman")

cor.test(dados_agrup$`Preço Médio Portugal`, dados_agrup$`Temperatura média (º F)`,  method="spearman")

cor.test(dados_agrup$`Preço Médio Portugal`, dados_agrup$`lEnergia total (MWh)`,  method="spearman")
