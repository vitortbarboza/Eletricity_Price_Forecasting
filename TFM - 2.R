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

preco = ts(dados_agrup$`Pre�o M�dio Portugal`, 
           freq=365.25/7, 
           start=decimal_date(ymd("2018-04-01")))

pre�o_treino <- head(preco, round(length(preco) * 0.95))
h <- length(preco) - length(pre�o_treino)
pre�o_teste <- tail(preco, h)

lpre�o_portugal <- log(preco)

lpre�o_treino <- head(lpre�o_portugal, round(length(lpre�o_portugal) * 0.95))
h <- length(lpre�o_portugal) - length(lpre�o_treino)
lpre�o_teste <- tail(lpre�o_portugal, h)


##### Exploratory Data Analysis #####

autoplot(preco) +
  ggtitle("Pre�o Semanal Energia em Portugal EUR/MWh ") +
  xlab("Year") +
  ylab("Valor em Euros")

mstl(preco, lambda = "auto") %>% autoplot()+
  ggtitle('Decomposi��o da S�rie Pre�o Semanal Energia em Portugal EUR/MWh')+
  xlab('Year')

##### Gr�ficos de Autocorrela��o e Estacionaridade #####

par(mfrow=c(2:1))
acf(coredata(lpre�o_portugal), main="Fun��o de Autocorrela��o", 20)
pacf(coredata(lpre�o_portugal),main="Fun��o de Autocorrela��o Parcial", 20)

adf.test(lpre�o_portugal)

acf(coredata(diff(lpre�o_portugal)), main="Fun��o de Autocorrela��o com a S�rie Diferenciada", 20)
pacf(coredata(diff(lpre�o_portugal)),main="Fun��o de Autocorrela��o Parcial com a S�rie Diferenciada", 20)

acf(diff(lpre�o_portugal), main="Fun��o de Autocorrela��o com a S�rie Diferenciada", 20)
pacf(coredata(diff(lpre�o_portugal)),main="Fun��o de Autocorrela��o Parcial com a S�rie Diferenciada", 20)


adf.test(diff(lpre�o_portugal))

##### Modeling #####


d=1
for(p in 1:3){
  for(q in 1:3){
    if(p+d+q<=11){
      model<-arima(x=lpre�o_treino,order = c((p),d,(q)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}




## Model 1 ## 
model_arima1 <- auto.arima(lpre�o_treino) #(2,1,2)

model_arima1

checkresiduals(model_arima1)

for (h in 1:100){
  #h# 
  show(h)
  show(Box.test(model_arima1$residuals, lag = h, type = "Ljung-Box", fitdf = 0))}

previsao_arima1 <- forecast(model_arima1, h=8)
MAPE(previsao_arima1$mean, lpre�o_teste)*100 #7.41%
RMSE(previsao_arima1$mean, lpre�o_teste) # 0.28

autoplot(previsao_arima1$mean)+autolayer(lpre�o_teste)+
  ggtitle('Compara��o entre os dados teste com a previs�o')

plot(previsao_arima1)
plot(previsao_arima2)


dummy_treino <- dados_agrup$Dummy[1:152]
dummy_teste <- dados_agrup$Dummy[153:160]

model_teste <- auto.arima(lpre�o_treino,xreg=reg_treino)
model_teste
previsao_teste <- forecast(model_teste, h=8,xreg=dummy_teste)
MAPE(previsao_teste$mean, lpre�o_teste)*100 #7.41%

## Model 2 ##
model_arima2 <- Arima(lpre�o_treino, order=c(1,1,0)) #(2,1,0)
model_arima2

checkresiduals(model_arima2)

for (h in 1:100){
  #h# 
  show(h)
  show(Box.test(model_arima2$residuals, lag = h, type = "Ljung-Box", fitdf = 0))}

previsao_arima2 <- forecast(model_arima2, h=8)
previsao_arima2

MAPE(previsao_arima2$mean, lpre�o_teste)*100 #8.27%
RMSE(previsao_arima2$mean, lpre�o_teste) # 0.33


model_teste <- auto.arima(lpre�o_treino, d=1)

## Prepara�ao Arimax ##

reg <- cbind(dados_agrup$`lEnergia total (MWh)`,
             dados_agrup$`Temperatura m�dia (� F)`,dados_agrup$`Velocidade m�dia do vento (mph)`)
colnames(reg) <- c("Volume Energia", "Temperatura", "Velocidade Vento")


reg <- cbind(dados_agrup$`lEnergia total (MWh)`,dados_agrup$`Velocidade m�dia do vento (mph)`)
colnames(reg) <- c("Volume Energia", "Velocidade Vento")


reg_treino <- reg[1:152,]
reg_teste <- reg[153:160,]

for(p in 1:4){
  for(q in 1:4){
    if(p+d+q<=11){
      model<-arima(x=lpre�o_treino, xreg= reg_treino,
                   order = c((p),1,(q)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}


## Model 3 ##

model_arimax1 <- Arima(lpre�o_treino, order=c(0,1,2),xreg=reg_treino)
model_arimax1
checkresiduals(model_arimax1)




model_arimax1 <- auto.arima(lpre�o_treino,xreg=reg_treino,
                       stepwise=FALSE,approx=FALSE)
?Arima
model_arimax1
checkresiduals(model_arimax1)

for (h in 1:100){
  #h# 
  show(h)
  show(Box.test(model_arimax1$residuals, lag = h, type = "Ljung-Box", fitdf = 0))}

previsao_arimax1 <- forecast(model_arimax1, h=8,xreg=reg_teste)

MAPE(previsao_arimax1$mean, lpre�o_teste)*100 #10.35%
RMSE(previsao_arimax1$mean, lpre�o_teste) # 0.44

## Model 4 ##

model_arimax2 <- Arima(lpre�o_treino, order=c(2,1,2),xreg=reg_treino)
model_arimax2

checkresiduals(model_arimax2)

for (h in 1:100){
  #h# 
  show(h)
  show(Box.test(model_arimax2$residuals, lag = h, type = "Ljung-Box", fitdf = 0))}


previsao_arimax2 <- forecast(model_arimax2, h=8,xreg=reg_teste)

plot(previsao_arimax1)
plot(previsao_arimax2)

MAPE(previsao_arimax2$mean, lpre�o_teste)*100 #10.27%
RMSE(previsao_arimax2$mean, lpre�o_teste) # 0.34

autoplot(previsao_arimax1) + autolayer(lpre�o_teste)


autoplot(previsao_arimax1$mean)+autolayer(lpre�o_teste)+
  ggtitle('Compara��o entre os dados teste com a previs�o')

autoplot(previsao_arimax1)+autolayer(lpre�o_teste)+
  ggtitle('Compara��o entre os dados teste com a previs�o')


previsao_arimax1 %>%
  autoplot() +
  geom_line(
    aes(
      x = as.numeric(time(lpre�o_teste)),
      y = as.numeric(lpre�o_teste)
    ),
    col = "red"
  )

par(mfrow=c(2:1))
shapiro.test(dados_agrup$`Pre�o M�dio Portugal`)
shapiro.test(lpre�o_portugal)

shapiro.test(dados_agrup$`Velocidade m�dia do vento (mph)`)
shapiro.test(dados_agrup$`Temperatura m�dia (� F)`)
shapiro.test(dados_agrup$`lEnergia total (MWh)`)

hist(dados_agrup$`Pre�o M�dio Portugal`)
plot(dados_agrup$`Pre�o M�dio Portugal`,dados_agrup$`Velocidade m�dia do vento (mph)`, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)


cor.test(dados_agrup$`Pre�o M�dio Portugal`, dados_agrup$`Velocidade m�dia do vento (mph)`,  method="spearman")
cor.test(lpre�o_portugal, dados_agrup$`Velocidade m�dia do vento (mph)`,  method="spearman")

cor.test(dados_agrup$`Pre�o M�dio Portugal`, dados_agrup$`Temperatura m�dia (� F)`,  method="spearman")

cor.test(dados_agrup$`Pre�o M�dio Portugal`, dados_agrup$`lEnergia total (MWh)`,  method="spearman")
