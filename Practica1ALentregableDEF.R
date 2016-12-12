setwd("D:/AnalisisEstadistico")

################## CARGA DE DATOS #############################

CASAS=read.csv("house_train.csv")

head(CASAS)
str(CASAS)

############# FORMATEO DE LAS VARIABLES #################################

CASAS$zipcode=as.factor(CASAS$zipcode)
CASAS$bedrooms=as.factor(CASAS$bedrooms)
CASAS$bathrooms=as.factor(CASAS$bathrooms)
CASAS$waterfront=as.factor(CASAS$waterfront)
CASAS$view=as.factor(CASAS$view)
CASAS$floors=as.factor(CASAS$floors)
CASAS$grade=as.factor(CASAS$grade)
CASAS$condition=as.factor(CASAS$condition)

str(CASAS)
#summary(CASAS)


nrow(CASAS[,c(1,3:21)]) # compruebo si hay repetidos
nrow(unique(CASAS[,c(1,4:21)]))
nrow(CASAS[!duplicated(CASAS[,c(1,4:21)]),]) # quito repetidos

############# ANÁLISIS DEL PRECIO Y SUPERFICIE DE LAS VIVIENDAS #################################

library(ggplot2)

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=zipcode )) # observando la distribución de las variables por códogo postal veo que son casas de Seattle

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=price)) # precio en ciertas zonas en el centro es mayor

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=sqft_living)) # en el este las casas son mas grandes que en el oeste

# voy a introducir una variable ueva: precio por metro (pie) cuadrado
library(dplyr) # necesitaré paquete dplyr
CASAS3 <- mutate(CASAS, priceXsqft_living = price/sqft_living)

f <- ggplot(CASAS3, aes(long,lat))
f + geom_point(aes(color=priceXsqft_living)) #el precio por metro cuadrado es definitivamente mayor en el centro. Es algo que podía indicar una brecha estructural.


############# ANÁLISIS DEL CAMBIO ESTRUCTURAL #################################

summary(CASAS3) # que existen outliers en el data sest

# divido el data set en dos partes: casas en el centro y casas en afueras para ver si puedo obtener un resultado mejor

CASAS_CENTRO = CASAS3[CASAS3$long<= -122.125 & CASAS3$long>= -122.4 & CASAS3$lat>47.53 & CASAS3$lat<47.7 ,]
CASAS_AFUERAS = CASAS3[((CASAS3$long> -122.125 | CASAS3$long< -122.4 ) & (CASAS3$lat<47.53 | CASAS3$lat>47.7) )    | ((CASAS3$lat>=47.53 | CASAS3$lat<=47.7) &(CASAS3$long> -122.125 | CASAS3$long< -122.4 ))| ((CASAS3$lat<=47.53 | CASAS3$lat>=47.7) &(CASAS3$long< -122.125 | CASAS3$long> -122.4 )),]

nrow(CASAS3)
nrow(CASAS_CENTRO )+nrow(CASAS_AFUERAS)

f <- ggplot(CASAS_CENTRO, aes(long,lat))
f + geom_point(aes(color=priceXsqft_living)) 


f <- ggplot(CASAS_AFUERAS, aes(long,lat))
f + geom_point(aes(color=priceXsqft_living)) 

# compruebo si existe cambio de estructura de precios en función de la ubicación de las viviendas

modelotodo=lm(price~sqft_living,data=CASAS3)
modelocentro=lm(price~sqft_living,data=CASAS_CENTRO)
modeloafueras=lm(price~sqft_living,data=CASAS_AFUERAS)

plot(CASAS$sqft_living,CASAS$price)
abline(modelotodo,col="red",lty = "dashed")
abline(modelocentro,col="blue")
abline(modeloafueras,col="green") # encuentro que la pendiente de cambio de preio en finción de superficie es mayor para las casas en el centro que para las casas en afueras


# adicionalmente realizo chow test
if (!require("gap")){
  install.packages("gap") 
  library(gap)
}

chow.test(log(CASAS_CENTRO$price),CASAS_CENTRO$sqft_living,log(CASAS_AFUERAS$price),CASAS_AFUERAS$sqft_living)

chow.test(CASAS_CENTRO$price,CASAS_CENTRO$sqft_living,CASAS_AFUERAS$price,CASAS_AFUERAS$sqft_living) # el resultado da siginificativo

# compruebo la distribución de los dos data sets:
summary(CASAS_CENTRO)
summary(CASAS_AFUERAS)# aunque parece que sigue habiendo valores extremos las series están mas homogeneas que la serie principal



#################### constucción del modelo ########################################

# -------------------------- modelo básico ---------------------------------------

#casas del centro

modelocentrolog=lm(log(price)~sqft_living,data=CASAS_CENTRO)
summary(modelocentrolog)
qqnorm(modelocentrolog$residuals); qqline(modelocentrolog$residuals,col=2)

plot(modelocentrolog$residuals)
smoothScatter(modelocentrolog$residuals)
hist(modelocentrolog$residuals)
confint(modelocentrolog,level=0.95)

if (!require("MASS")){
  install.packages("MASS") 
  library(MASS)
}

if (!require("caTools")){
  install.packages("caTools") 
  library(caTools)
}
modelocentrologrobusto=rlm(log(price)~sqft_living,data=CASAS_CENTRO)
summary(modelocentrologrobusto)
qqnorm(modelocentrologrobusto$residuals); qqline(modelocentrolog$residuals,col=2)

plot(modelocentrologrobusto$residuals)
smoothScatter(modelocentrologrobusto$residuals)
hist(modelocentrologrobusto$residuals)
confint.default(modelocentrologrobusto,level=0.95)

AIC(modelocentrolog)
AIC(modelocentrologrobusto)
BIC(modelocentrolog)
BIC(modelocentrologrobusto)


#casas de las afueras

modeloafueras=lm(log(price)~sqft_living,data=CASAS_AFUERAS)
summary(modeloafueras)
qqnorm(modeloafueras$residuals); qqline(modeloafueras$residuals,col=2)
confint.default(modeloafueras,level=0.95)

modeloafuerasrobusto=rlm(log(price)~sqft_living,data=CASAS_AFUERAS)
summary(modeloafuerasrobusto)
qqnorm(modeloafuerasrobusto$residuals); qqline(modeloafueras$residuals,col=2)
confint.default(modeloafuerasrobusto,level=0.95)


plot(modeloafueras$residuals)
smoothScatter(modeloafueras$residuals)
hist(modeloafueras$residuals)
confint(modeloafueras,level=0.95)



AIC(modeloafueras)
AIC(modeloafuerasrobusto)
BIC(modeloafueras)
BIC(modeloafuerasrobusto)

# ------------------ analizamos si añadir variables--------------------------------

# variables adicionales

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=waterfront)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=view)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=yr_built )) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=yr_renovated)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=bedrooms)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=bathrooms)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=floors)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=grade)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=condition)) 

head(CASAS3)
head(CASAS3[,c(3:12,15:16,22)])
plot(CASAS3[,c(3:12,15:16,22)])


# modelo casas centro

#busco patrones en los residuos
plot(modelocentrolog$residuals,col=CASAS3$yr_renovated)
plot(modelocentrolog$residuals,col=CASAS3$sqft_lot)
plot(modelocentrolog$residuals,col=CASAS3$bedrooms)
plot(modelocentrolog$residuals,col=CASAS3$floors)
plot(modelocentrolog$residuals,col=CASAS3$grade)
plot(modelocentrolog$residuals,col=CASAS3$condition)
plot(modelocentrolog$residuals,col=CASAS3$waterfront)
plot(modelocentrolog$residuals,col=CASAS3$view)
plot(modelocentrolog$residuals,col=CASAS3$yr_built)
plot(modelocentrolog$residuals,col=CASAS3$sqft_living15)
plot(modelocentrolog$residuals,col=CASAS3$sqft_lot15)
plot(modelocentrolog$residuals,col=CASAS3$sqft_above)

modelocentrologgrade=lm(log(price)~sqft_living+grade,data=CASAS_CENTRO)
modelocentrologcond=lm(log(price)~sqft_living+grade+condition,data=CASAS_CENTRO)
modelocentrologwaterfront=lm(log(price)~sqft_living+grade+condition+waterfront,data=CASAS_CENTRO)
modelocentrologwiew=lm(log(price)~sqft_living+grade+condition+waterfront+view,data=CASAS_CENTRO)
modelocentrologyrbuild=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built,data=CASAS_CENTRO)
modelocentrologneighbours=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built+sqft_living15,data=CASAS_CENTRO)

AIC(modelocentrolog)
AIC(modelocentrologgrade)
AIC(modelocentrologcond)
AIC(modelocentrologwaterfront)
AIC(modelocentrologwiew)
AIC(modelocentrologyrbuild)
AIC(modelocentrologneighbours)

BIC(modelocentrolog)
BIC(modelocentrologgrade)
BIC(modelocentrologcond)
BIC(modelocentrologwaterfront)
BIC(modelocentrologwiew)
BIC(modelocentrologyrbuild)
BIC(modelocentrologneighbours)

summary(modelocentrologneighbours)
confint(modelocentrologneighbours,level=0.95)
qqnorm(modelocentrologneighbours$residuals); qqline(modelocentrologneighbours$residuals,col=2)
hist(modelocentrologneighbours$residuals)


# modelo casas afueras

#busco patrones en los residuos

plot(modeloafueras$residuals,col=CASAS3$yr_renovated)
plot(modeloafueras$residuals,col=CASAS3$bedrooms)
plot(modeloafueras$residuals,col=CASAS3$grade)
plot(modeloafueras$residuals,col=CASAS3$condition)
plot(modeloafueras$residuals,col=CASAS3$waterfront)
plot(modeloafueras$residuals,col=CASAS3$view)
plot(modeloafueras$residuals,col=CASAS3$yr_built)
plot(modeloafueras$residuals,col=CASAS3$sqft_living15)
plot(modeloafueras$residuals,col=CASAS3$sqft_lot15)
plot(modeloafueras$residuals,col=CASAS3$sqft_above)

modeloafueras=lm(log(price)~sqft_living,data=CASAS_AFUERAS)
modeloafuerasgrade=lm(log(price)~sqft_living+grade,data=CASAS_AFUERAS)
modeloafuerascondition=lm(log(price)~sqft_living+grade+condition,data=CASAS_AFUERAS)
modeloafueraswaterfront=lm(log(price)~sqft_living+grade+condition+waterfront,data=CASAS_AFUERAS)
modeloafuerasview=lm(log(price)~sqft_living+grade+condition+waterfront+view,data=CASAS_AFUERAS)
modeloafuerasyrbuild=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built,data=CASAS_AFUERAS)
modeloafuerasneihgbours=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built+sqft_living15,data=CASAS_AFUERAS)

AIC(modeloafueras)
AIC(modeloafuerasgrade)
AIC(modeloafuerascondition)
AIC(modeloafueraswaterfront)
AIC(modeloafuerasview)
AIC(modeloafuerasyrbuild)
AIC(modeloafuerasneihgbours)

BIC(modeloafueras)
BIC(modeloafuerasgrade)
BIC(modeloafuerascondition)
BIC(modeloafueraswaterfront)
BIC(modeloafuerasview)
BIC(modeloafuerasyrbuild)
BIC(modeloafuerasneihgbours)



summary(modeloafuerasneihgbours)
confint(modeloafuerasneihgbours,level=0.95)
qqnorm(modeloafuerasneihgbours$residuals); qqline(modeloafuerasneihgbours$residuals,col=2)
hist(modeloafuerasneihgbours$residuals)


# --------------------- modelo con dummy:centro=1 ----------------------------------

library(dplyr) # necesitaré paquete dplyr
CASAS_CENTRO <- mutate(CASAS_CENTRO, centro = 1)
NROW(CASAS_CENTRO)
CASAS_AFUERAS <- mutate(CASAS_AFUERAS, centro = 0)
NROW(CASAS_AFUERAS)
CASAS_TODAS <- rbind(CASAS_CENTRO,CASAS_AFUERAS) 
NROW(CASAS_TODAS)

CASAS_TODAS$centro=as.factor(CASAS_TODAS$centro)

modelotodas=lm(log(price)~sqft_living+centro,data=CASAS_TODAS)
modelotodasgrade=lm(log(price)~sqft_living+grade+centro,data=CASAS_TODAS)
modelotodascondition=lm(log(price)~sqft_living+grade+condition+centro,data=CASAS_TODAS)
modelotodaswaterfront=lm(log(price)~sqft_living+grade+condition+waterfront+centro,data=CASAS_TODAS)
modelotodasview=lm(log(price)~sqft_living+grade+condition+waterfront+view+centro,data=CASAS_TODAS)
modelotodasyrbuild=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built+centro,data=CASAS_TODAS)
modelotodasneihgbours=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built+sqft_living15+centro,data=CASAS_TODAS)


AIC(modelotodas)
AIC(modelotodasgrade)
AIC(modelotodascondition)
AIC(modelotodaswaterfront)
AIC(modelotodasview)
AIC(modelotodasyrbuild)
AIC(modelotodasneihgbours)

summary(modelotodasneihgbours)

confint(modelotodasneihgbours,level=0.95)
qqnorm(modelotodasneihgbours$residuals); qqline(modelotodasneihgbours$residuals,col=2)
hist(modelotodasneihgbours$residuals)
smoothScatter(modelotodasneihgbours$residuals)


################################ modelo predictivo ###############################################

# ------------------ compruebo las correlaciones para detectar multicolinearidad-----------------

summary(CASAS_TODAS)



cor(CASAS_TODAS[,c(3,6,7, 13:16,20:22)]) # matriz de correlaciones para variables numericas

#install.packages("car")
library(car)

# VIF para ver tambíen las variables categoricas
modelopruebacorr =lm(price~sqft_living+grade+condition+waterfront+view+yr_built+sqft_living15+centro,data=CASAS_TODAS)
vif(modelopruebacorr)

summary(modelopruebacorr)

# ----------------- divido la población en las muestras de entrenamiento y test-----------------------------------


if (!require("glmnet")){
  install.packages("glmnet") 
  library(glmnet)
}

if (!require("caTools")){
  install.packages("caTools") 
  library(caTools)
}

set.seed(12345) 
SAMPLE = sample.split(CASAS_TODAS$price, SplitRatio = 0.75)
Train = subset(CASAS_TODAS, SAMPLE == TRUE)
Test = subset(CASAS_TODAS, SAMPLE == FALSE)


# --------------------------- primer modelo predictivo ----------------------------------

modelolm =lm(price~sqft_living+grade+condition+waterfront+view+yr_built+sqft_living15+centro,data=Train)
summary(modelolm)

stepAIC(modelolm,direction="backward") # veo con backward selection si excluir alguna variable


Train$prediccion=predict(modelolm,type="response")
R2_Train=1-sum((Train$price-Train$prediccion)^2)/sum((Train$price-mean(Train$price))^2)

Test$prediccion=predict(modelolm,newdata=Test,type="response")
R2_Test=1-sum((Test$price-Test$prediccion)^2)/sum((Test$price-mean(Test$price))^2)

R2_Train
R2_Test


confint(modelolm,level=0.95)
qqnorm(modelolm$residuals); qqline(modelolm$residuals,col=2)
hist(modelolm$residuals)


# -------------------- prueba con modelo robusto------------------------------------------


if (!require("MASS")){
  install.packages("MASS") 
  library(MASS)
}

if (!require("caTools")){
  install.packages("caTools") 
  library(caTools)
}

modelorlm =rlm(price~sqft_living+grade+condition+waterfront+view+yr_built+sqft_living15+centro,data=Train)
summary(modelorlm)

Train$prediccion=predict(modelorlm,type="response")
R2rlm_Train=1-sum((Train$price-Train$prediccion)^2)/sum((Train$price-mean(Train$price))^2)

Test$prediccion=predict(modelorlm,newdata=Test,type="response")
R2rlm_Test=1-sum((Test$price-Test$prediccion)^2)/sum((Test$price-mean(Test$price))^2)

R2rlm_Train
R2rlm_Test


confint.default(modelorlm,level=0.95)
qqnorm(modelorlm$residuals); qqline(modelolm$residuals,col=2)
hist(modelorlm$residuals)

# -------comparo los modelos-------------------

AIC(modelolm)
AIC(modelorlm) # el modelo robusto no sale mejor



# ---------------------- cálculo deprecios para las casas no valoradas ---------------------------------


house_test=read.csv("house_test.csv")


house_test$zipcode=as.factor(house_test$zipcode)
house_test$bedrooms=as.factor(house_test$bedrooms)
house_test$bathrooms=as.factor(house_test$bathrooms)
house_test$waterfront=as.factor(house_test$waterfront)
house_test$view=as.factor(house_test$view)
house_test$floors=as.factor(house_test$floors)
house_test$grade=as.factor(house_test$grade)
house_test$condition=as.factor(house_test$condition)


summary(house_test)
nrow(house_test)
house_test_CENTRO = house_test[house_test$long<= -122.125 & house_test$long>= -122.4 & house_test$lat>47.53 & house_test$lat<47.7 ,]
house_test_AFUERAS = house_test[((house_test$long> -122.125 | house_test$long< -122.4 ) & (house_test$lat<47.53 | house_test$lat>47.7) )    | ((house_test$lat>=47.53 | house_test$lat<=47.7) &(house_test$long> -122.125 | house_test$long< -122.4 ))| ((house_test$lat<=47.53 | house_test$lat>=47.7) &(house_test$long< -122.125 | house_test$long> -122.4 )),]
nrow(house_test_CENTRO)+nrow(house_test_AFUERAS)


house_test_CENTRO <- mutate(house_test_CENTRO, centro = 1)

house_test_AFUERAS <- mutate(house_test_AFUERAS, centro = 0)

house_test_TODAS <- rbind(house_test_CENTRO,house_test_AFUERAS) 
NROW(house_test_TODAS)
house_test_TODAS$centro=as.factor(house_test_TODAS$centro)

house_test_TODAS$price=predict(modelolm,newdata=house_test_TODAS,type="response")
summary(house_test_TODAS)

library(ggplot2)

f <- ggplot(house_test_TODAS, aes(long,lat))
f + geom_point(aes(color=price ))

 write.csv(house_test_TODAS, "house_test_con_precios.csv")
























