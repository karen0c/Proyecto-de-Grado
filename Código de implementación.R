library(readxl)
library(stats)
library(tseries)
library(fBasics)
library(forecast)
library(lmtest)
library(PerformanceAnalytics)
library(quantmod)
library(dplyr)
library(lubridate)
library(TSA)
library(dygraphs)
library(urca)
library(vars)
library(stats)
library(ggplot2)
library(tsDyn)
library(Metrics)
library(lmtest)
library(forecast)


################## MODELO UNIVARIADO ARIMA ###############################
#Carga de base de datos, se selecciona la hoja donde solo se encuentra la serie de precios nacional
Base <- read_excel("BASE DATOS R.xlsx", sheet = 1)

Date<-as_date(Base$Date)
Price<-as.numeric(Base$Precio)

Datos<-data.frame(Date,Price)

#GráficoPrecios
plot(Datos, main="", sub="Figura 1: Precio histórico nacional del Cacao (COP/kg)", type = "l", col="orange")

#-Estadisticas básicas 
basicStats(Price) ## Resumen estadísticos

#Análisis de autocorrelación
acf(Price,main="",sub="Figura 2: Función de Autocorrelación Simple")
pacf(Price, main="", sub="Figura 3: Función de Autocorrelación Parcial")

#Prueba Dickey-Fuller
adf.test(Price)
#p valor mayor a 0,05 por lo que la hipótesis nula no se rechaza, la serie no es estacionaria.

#-----Serie diferenciada
difserie<-diff(Price)

plot(difserie,sub="Figura 4: Serie en primera diferencia", xlab="Tiempo",ylab="Primera diferencia" ,type = "l", col="orange")

#histograma de serie en primera diferencia:
par(mfrow=c(1,1))
hist(difserie)

#ACF y PACF de la serie en primera diferencia
acf(difserie,main="",sub="Figura 5: AFC en Primera Diferencia")#Rezagos 5 y 6 significativos
pacf(difserie,main="",sub="Figura 6: PACF en Primera Diferencia")#rezagos 5 y 7 significativos


adf.test(difserie)
#con un p valor menor a 0,05 por lo que la hipótesis nula se rechaza. La serie en primera diferencia es estacionaria.

#estadísticas básicas de la serie en primera diferencia
basicStats(difserie)

#------Analisis_momentos
t.test(difserie) # h0: media de retornos = 0 #

s3=skewness(difserie)#sesgo
T=length(difserie)
t3=s3/sqrt(6/T)
pp=2*pt(abs(t3), T-1, lower=FALSE)
pp  # h0: sesgo = 0 # 

s4=kurtosis(difserie)#curtosis
t4=s4/sqrt(24/T)
pv=2*(1-pnorm(t4))
pv # h0: exceso kurtosis = 0 #

normalTest(difserie,method='jb') # h0: distribucion normal #


# Partimos serie, tomemos el 5% para la prueba
h <- round(length(difserie)*0.05, digits = 0 )
h

#dividimos en entrenamiento (95%) y prueba (5%)
trainArima <- difserie[1:(length(difserie) - h) ]
testArima<- difserie[(length(difserie) - h + 1):length(difserie) ]

#ACF y PACF de entrenamiento
acf(trainArima,main="")#rezago 5 y 6 significativos
pacf(trainArima,main="")#rezago 5 y 7 significativos

#----Ajuste del modelo
eacf(trainArima, 9,9)
m1auto<-auto.arima(trainArima)
m1auto

arima1<-arima(trainArima,c(1,0,1),method = "ML")
arima1

arima2<-arima(trainArima,c(1,0,5), method="ML")
arima2

arima3<-arima(trainArima,c(5,0,5), method="ML")
arima3

arima4<-arima(trainArima,c(5,0,6), method="ML")
arima4#este me dio menor AIC, se escoje estee

arima5<-arima(trainArima,c(1,0,6), method="ML")
arima5

arima6<-arima(trainArima, c(4,0,5), method = "ML")
arima6

arima7<-arima(trainArima,c(4,0,3), method="ML")
arima7

arima8<-arima(trainArima,c(3,0,7), method="ML")
arima8

arima9<-arima(trainArima,c(5,0,7), method="ML")
arima9

arima10<-arima(trainArima,c(6,0,7), method="ML")
arima10

#---Validación del modelo escogido

tsdiag(arima4)

#Estacionariedad de los residuos
plot(arima4$residuals, main="", sub="Figura 7: Residuos del modelo ARIMA(4,0,6)", xlab="Tiempo",ylab="Residuos")

#Autocorrelación de los residuos
acf(arima4$residuals, main="", sub="Figura 8: Autocorrelaciones de los residuos del modelo ARIMA(4,0,6)", xlab="Tiempo",ylab="Autocorrelación")
pacf(arima4$residuals, main="", sub="Figura 9: Autocorrelaciones Parciales de los residuos del modelo ARIMA(4,0,6)", xlab="Tiempo",ylab="Autocorrelación")

#No existe autocorrelación significativa en los residuos

#Normalidad de los residuos
qqnorm(arima4$residuals, main="", sub="Figura 10: Gráfico Q para evaluar normalidad");qqline(arima4$residuals)
shapiro.test(arima4$residuals)
#Existe normalidad en los residuos

#no hay patrÓn en los residuos.

#-----Pronóstico
ffArima<-predict(arima4,n.ahead = h)
autoplot(ts(difserie))+autolayer(ffArima$pred)

################################################# MODELOS MULTIVARIADOS ###################################

#Se carga la base y se selecciona la hoja que contiene la base agregada con todo las posibles variables explicativas
Datos2 <- na.omit(read_excel("BASE DATOS R.xlsx", 
                            sheet = "BASE R" , col_types = c("date", 
                                                             "text", "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric")))            
#Cointegración de variables sin diferenciación
johatestInicial=ca.jo(Datos2[,-c(1:2,7,9)], type = "trace", K=2, ecdet ="none", spec = "longrun") #K is the number of lags to use in the vector autoregressive model and is set this to the minimum, K=2.
summary(johatestInicial)
#hay un vector de cointegración, r=0 al 1% de significancia.

Datos2$Fecha<-NULL
str(Datos2)

#visualización de los datos
datos<-as.xts(Datos2,order.by= Datos2$DATE,frequency = 12)%>% na.omit()
datos$DATE<-NULL
dygraph(datos[,-c(5:8)]) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))


cacao<-Datos2$`Precio Cacao Nacional (COP/kilo)`
internacional<-Datos2$`Precio Cacao Internacional (COP/kilo)`
trm<-Datos2$`TRM (cop/usd)`
cafe<-Datos2$`Precio Café Internacional (COP/kilo)`
importación<-Datos2$`Importación Valor(miles COP/FOB)`
exportacion<-Datos2$`Exportación Valor(miles COP/FOB)`
nutresa<-Datos2$`Grupo Nutresa`

#se revisa si las series son estacionarias
adf.test(cacao)#no es estacionaria
adf.test(internacional)#no es estacionaria
adf.test(trm)#no es estacionaria
adf.test(cafe)#no es estacionaria
adf.test(importación)#estacionaria al 5% de significancia
adf.test(exportacion)#estacionaria al 10% de significancia
adf.test(nutresa)#no es estacionaria

################ MODELO VAR ##############

#Diferencio para que sean estacionarias las series
PrecioCacaod<-diff(Datos2$`Precio Cacao Nacional (COP/kilo)`)
PrecioInternacionald<-diff(Datos2$`Precio Cacao Internacional (COP/kilo)`)
TRMd<-diff(Datos2$`TRM (cop/usd)`)
precioCafed<-diff(Datos2$`Precio Café Internacional (COP/kilo)`)
importaciónd<-diff(Datos2$`Importación Valor(miles COP/FOB)`)
exportaciond<-diff(Datos2$`Exportación Valor(miles COP/FOB)`)
nutresad<-diff(Datos2$`Grupo Nutresa`, differences=2)

#compruebo estacionariedad
adf.test(PrecioCacaod)
adf.test(PrecioInternacionald)
adf.test(TRMd)
adf.test(precioCafed)
adf.test(importaciónd)
adf.test(exportaciond)
adf.test(nutresad)

fecha<-Datos2$DATE[3:104]

datos_union1<-data.frame(PrecioCacaod,PrecioInternacionald,TRMd,precioCafed,importaciónd,exportaciond)[-1,]

datos_union<-data.frame(datos_union1, nutresad)

datos_xts<-as.xts(datos_union,order.by= fecha,frequency = 12)


nivelk=VARselect(datos_xts,lag.max = 7)
nivelk$selection

##################
# VAR:

#pongo los datos en ts , serie de tiempo:
cacaoNacional=ts(datos_xts$PrecioCacaod, start = c(2013,09,01), frequency=12)
cacaoInternacional=ts(datos_xts$PrecioInternacionald, start = c(2013,09,01), frequency=12)
trm=ts(datos_xts$TRMd, start = c(2013,09,01), frequency=12)
cafe=ts(datos_xts$precioCafed, start = c(2013,09,01), frequency=12)
importación=ts(datos_xts$importaciónd, start = c(2013,09,01), frequency=12)
exportacion=ts(datos_xts$exportaciond,start = c(2013,09,01), frequency=12)
nutresa=ts(datos_xts$nutresad,start = c(2013,09,01), frequency=12)

#uno los activos
dataVAR2=ts.intersect(cacaoNacional, cacaoInternacional, trm,cafe,importación,exportacion,nutresa) %>% na.omit()
head(dataVAR2)

#función VAR
var_est2 <- VAR(y=dataVAR2, lag.max = 7)
summary(var_est2)
#el precio del cacao nacional e internacional es significativo

######Refino el modelo a solo 2 variables: precio nacional y precio internacional

#cargo datosy selecciono la hoja que tiene una periodicidad semanal del precio nacional e internacional
Datos3 <- na.omit(read_excel("BASE DATOS R.xlsx", sheet = 2))

precioNacional1<-Datos3$`Precio Cacao`
precioInternacional<-Datos3$`Pesos /kilo`

#miramos cointegración para complementar la prueba de johansen
po.test(cbind(precioNacional1, precioInternacional))#las series estan cointegradas

#unimos en un data frame
datos3<-data.frame(precioNacional1, precioInternacional)

nivelk=VARselect(datos3,lag.max = 7)
nivelk$selection


johatestSinDiferenciar=ca.jo(datos3, type = "trace", K=5, ecdet ="none", spec = "longrun") #K is the number of lags to use in the vector autoregressive model and is set this to the minimum, K=2.
summary(johatestSinDiferenciar)
#hay un vector de cointegración al 1% de significancia


#revisamos estacionariedad
adf.test(precioNacional1)
adf.test(precioInternacional)
#no son estacionarias entonces diferencio

precioNacional1d<-diff(precioNacional1)
adf.test(precioNacional1d)
precioInternacionald<-diff(precioInternacional)
adf.test(precioInternacionald)
#ya son estacionarias


datos33<-data.frame(precioNacional1d, precioInternacionald)
nivelk=VARselect(datos33,lag.max = 7)
nivelk$selection
johatestDiff=ca.jo(datos33, type = "trace", K=4, ecdet ="none", spec = "longrun") #K is the number of lags to use in the vector autoregressive model and is set this to the minimum, K=2.
summary(johatestDiff)

# Partimos serie, tomemos el 5% para la prueba
h1 <- round(length(precioNacional1d)*0.05, digits = 0 )
h1

#dividimos la base en entrenamiento y prueba
trainVAR <- datos33[1:(length(precioInternacionald) - h1), ]
testVAR<- datos33[(length(precioInternacionald) - h1 + 1):length(precioNacional1d), ]

##### VAR
var_est2 <- VAR(y=trainVAR, lag.max = 7)
model_sum=summary(var_est2)
model_sum

###calcular AIC
AIC(var_est2)

#ccf
ccf(trainVAR$precioInternacionald,trainVAR$precioNacional1d)

#prueba de granger-cause
gragtest=grangertest(trainVAR$precioNacional1d~ trainVAR$precioInternacionald, order=3)
gragtest#la serie de precios Internacionales es Gragner-Cause de la serie de precios Nacionales

#obtenemos el valor del pronóstico
ffVAR<-predict(var_est2,n.ahead = h1)
ffVAR

####Impulse response
chole = t(chol(model_sum$covres))
chole

# nos devolvemos en la transformación que hicimos para que fueran estacionarias las series
nhor=20
nr_lev <- nrow(datos3)
mr_lev= as.matrix(datos3)
str(mr_lev)
m.varf_lev_ft <- rbind(mr_lev, matrix(NA, nhor,2 ))
m.ft_df <- do.call(cbind,lapply(ffVAR$fcst, 
                                function(x) x[,"fcst"]))

# growth to level
for(h in (nr_lev+1):(nr_lev+nhor)) {
  hf <- h - nr_lev
  m.varf_lev_ft[h,] <- m.varf_lev_ft[h-1,] + m.ft_df[hf,]
}

# Draw Graph
str.main=c("Precio Nacional", "Precio Internacional")
par(mfrow=c(2,1), mar=c(2,2,2,2))

for(i in 1:2) {
  df <- m.varf_lev_ft[,i]
  matplot(df, type=c("l"), col = c("orange"), 
          main = str.main[i]) 
  abline(v=nr_lev, col="blue")
}

##forecast final recobrando el nivel de precio
ffVARsinDiff<-tail(m.varf_lev_ft, 20)



######### ---vECM-----###########33
#--------- Construcción modelo VECM

# Partimos serie, tomemos el 5% para la prueba
h2 <- round(length(precioNacional1)*0.05, digits = 0 )
h2
trainVECM <- datos3[1:(length(precioInternacional) - h2), ]
testVECM<- datos3[(length(precioInternacional) - h2 + 1):length(precioNacional1), ]

nivelk=VARselect(trainVECM, lag.max = 7, type = "const")
nivelk$selection

johatestVECM=ca.jo(trainVECM, type = "trace", K=3, ecdet ="none", spec = "longrun") #K is the number of lags to use in the vector autoregressive model and
summary(johatestVECM)

#modelo VECM
vecm1 = VECM(trainVECM, lag=2, r=1, estim = ("ML"))
summary(vecm1)

#AIC de modelo vec
AIC(vecm1)


#forecast
ffVECM<-predict(vecm1,n.ahead = h2)
plot(x=ffVECM[,2], y=ffVECM[,1], xlab="Precio Internacional del Cacao", ylab="Precio Nacional del Cacao", col = "orange")

plot(c(trainVECM$precioNacional1, ffVECM[,1]),type=c("l"), col = c("orange"),ylab="Precio Nacional del Cacao",xlab="Tiempo" )
abline(v=407-h2, col="blue")


# Diagnóstico: Las pruebas para ello funcionan con VAR, hay que transformar:
varmod1 = vec2var(johatestVECM, r=1)

#se revisan los residuales
varmod1$resid
checkresiduals(varmod1$resid[,1])
plot.ts(varmod1$resid, xlab="Tiempo",ylab="Residuos")

# correlación:
ade1 = serial.test(varmod1 , lags.pt = 5, type = "BG")
ade1   # H0: datos independientes, H1: datos dependientes. 
###residuos independientes

#heterocedasticidad
hete1 = arch.test(varmod1 , lags.multi = 15, multivariate.only = TRUE)
hete1  #efecto arch H0: no efecto arch, H1: hay efecto arch
#no efecto arch

#Normalidad
norm1 = normality.test(varmod1, multivariate.only = TRUE)
norm1
#no se distribuyen normal, esto no es un problema debido a que se utilizó el metodo de "ML"

# Análisis Impulso - Respuesta: Impulse response
m1irf = irf(varmod1,n.ahead = 30, boot = TRUE)
plot(m1irf)  #predicción eje Y: es la var dependiente, acorde el impulso X.


# Con OIR (orthogonal- Imp-Resp) Se descompone la matriz de vari-cov a una matriz triangular inferior con elementos positivos diagonales
suma = summary(vecm1)
# suma
choles = t(chol(suma$sigma))
choles  


############----Arima con regresores----------###
##Auto-arima:# Partimos serie, tomemos el 5% para la prueba
h3 <- round(length(precioNacional1)*0.05, digits = 0 )
h3
trainArimaE <- datos3[1:(length(precioNacional1) - h3), ]
testArimaE<- datos3[(length(precioNacional1) - h3 + 1):length(precioNacional1), ]

#se obtiene el modelo arima
modarima=auto.arima(trainArimaE$precioNacional1, xreg =trainArimaE$precioInternacional)
modarima 

#se revisan los residuales;
checkresiduals(modarima)#no hay error en los residuales
eacf(modarima$residuals)#ruido blanco

#pronóstico
fut_arima = forecast(modarima, xreg = prediccion[1:h3,])
autoplot(fut_arima, ylab = "Precio Nacional del Cacao")


################---Artificial Neural Networks
# Partimos serie, tomemos el 5% para la prueba
h4 <- round(length(datos3$precioNacional1)*0.05, digits = 0 )
h4
trainRN <- datos3[1:(length(datos3$precioNacional1) - h4), ]
testRN<- datos3[(length(datos3$precioNacional1) - h4 + 1):length(datos3$precioNacional1), ]

#se crea el objeto de red neuronal
(fit_nnr = nnetar(trainRN[,1], 
                  xreg = trainRN[,2], # external regressors, which must have the same number of rows
                  lambda="auto"))

#se halla el forecast
fct<-forecast(fit_nnr, xreg =prediccion[(length(prediccion)-h4+1):length(prediccion)], PI=TRUE)
fct

Gráfico
autoplot(fct) + xlab("Semanas") + ylab("Precio Nacional del cacao") 



######## Comparación de modelos

#comparación intermuestral
AICvar<-AIC(var_est2)
AICvecm<-AIC(vecm1)
AICarimaE<-AIC(modarima)

#en el caso del var comparamos contra datos sin diferenciar
test<-datos3[(length(precioInternacional) - h1 + 1):length(precioNacional1), ]

#méticas de precisión de forecast
RMSEvar<-rmse(test$precioNacional1, ffVARsinDiff[,1])
RMSEvecm<-rmse(testVECM$precioNacional1,ffVECM[,1])
RMSEarimaE<-rmse(testArimaE$precioNacional1,  fut_arima$mean)
RMSErn<-rmse(testRN$precioNacional1,  as.ts(fct$mean))

MAPEvar<-mape(test$precioNacional1, ffVARsinDiff[,1])
MAPEvecm<-mape(testVECM$precioNacional1,ffVECM[,1])
MAPEarimaE<-mape(testArimaE$precioNacional1,  fut_arima$mean)
MAPErn<-mape(testRN$precioNacional1,  as.ts(fct$mean))

#---se imprime los resultados en una tabla:
Modelo<-c("Arima con Regresores", "VAR", "VECM", "Redes Neuronales")
AIC<-c(AICarimaE,AICvar,AICvecm, "~")
RMSE<-c(RMSEarimaE, RMSEvar, RMSEvecm, RMSErn)
MAPE<-c(MAPEarimaE, MAPEvar, MAPEvecm, MAPErn)
res<-data.frame(Modelo,AIC,RMSE, MAPE)
print((res))




##########################---Modelo Univariado del precio Internacional del Cacao:
#se obtiene la serie de precios internacional del cacao
DateI<-as_date(Datos3$Date)
PriceI<-as.numeric(Datos3$`Pesos /kilo`)

#se agrega en una sola base
DatosI<-data.frame(DateI,PriceI)

#Gráfico de precios internacionales
plot(DatosI, main="", type = "l", col="orange")

#-Estadisticas básicas 
basicStats(PriceI) ## Resumen estadísticos


# Partimos serie, tomemos el 4% para la prueba
h <- round(length(PriceI)*0.04, digits = 0 )
h
train <- PriceI[1:(length(PriceI) - h) ]
test<- PriceI[(length(PriceI) - h + 1):length(PriceI) ]

#pasamoas a objeto ts
traints<-ts(train, start=c(07,07), frequency = 7)


#----Podemos descomponer la serie para ver la forma de lo componentes.
traints%>% mstl() %>%
  autoplot(traints) + xlab("Daily") +
  ggtitle("MSTL Decomposition of prices")

#Posibles enfoques de suavizamiento: 
#----Primer Modelo: Simple Exponential Smoothing, para modelar nivel sslamente.
fit1<-ses(traints, h=h )
summary(fit1)
ffit1<-forecast(fit1, h=h)
autoplot(fit1) +  autolayer(ffit1)

#----Segundo Modelo: Metodos con Tendencia. holt(): Tendencia Lineal con Holt,se puede incluir jorobas.
fit2 <- holt(traints,h=h, damped = TRUE)
summary(fit2)
ffit2<-forecast(fit2, h=h)
autoplot(fit2) +  autolayer(ffit2)

#------Tercer Modelo: Holt con correccisn a la deriva:
fit3<-HoltWinters(traints, alpha = NULL, beta=NULL, gamma = NULL)
fit3
ffit3<-forecast(fit3, h=h)
autoplot(traints)+autolayer(ffit3)

#------Cuarto Modelo: Probemos la aplicacisn de ets(), que deje determine el modelo
fit4<-ets(traints, model="AAA", damped=NULL, alpha=NULL, beta=NULL,
          gamma=NULL, phi=NULL, lambda="auto", biasadj=TRUE,
          additive.only=FALSE, allow.multiplicative.trend=FALSE)
summary(fit4)
ffit4<-forecast(fit4, h=h )
autoplot(forecast(fit4,h=h), include=50)

##Quinto modelo:
##

fc <- hw(subset(traints,end=length(traints)-h),
         damped = TRUE, seasonal="multiplicative", h=h)
autoplot(fc, include = 50) +
  autolayer(fc, series="HW multi damped", PI=FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))
summary(fc)
#-------Mitrica Desempeqo pronsstico:
RMSEses<-rmse(test, ffit1$mean)
RMSEholt<-rmse(test, ffit2$mean)
RMSE_HW<-rmse(test, ffit3$mean)
RMSEets<-rmse(test, ffit4$mean)
RMSEhw<-rmse(test, fc$mean)

MAPEses<-mape(test, ffit1$mean)
MAPEholt<-mape(test, ffit2$mean)
MAPE_HW<-mape(test, ffit3$mean)
MAPEets<-mape(test, ffit4$mean)
MAPEhw<-mape(test, fc$mean)
#---Imprimamos los resultados en una tabla:

Modelo<-c("ses", "holt", "HW", "ets","hw")
RMSE<-c(RMSEses, RMSEholt, RMSE_HW, RMSEets, RMSEhw)
MAPE<-c(MAPEses, MAPEholt, MAPE_HW, MAPEets, MAPEhw)
res<-data.frame(Modelo,RMSE, MAPE)
print((res))


## se elige el modelo 3 y se realiza predicción
prediccion<-predict(fit3, n.ahead=2*12)###24 semanas
prediccion<-as.matrix(prediccion[,1])

