# ================================================
# Analizando las Encuestas de Hogares 2019
# ================================================

# 1) Instalación de paquetes 
install.packages(c("tidyverse","psych","haven", "PerformanceAnalytics"))
library(tidyverse)
library(haven)
library(psych)
library(PerformanceAnalytics)

# 2) Importación de los datos (Encuesta de Hogares 2019 )
EH2019 <- read_sav("EH2019_Persona.sav")
attach(EH2019) # Permite trabajar con los nombres de la varibales dentro del data frame 
names(EH2019) # Nombres de la varibles dentro del data frame
EH2019
str(EH2019) # Resumen del tipo de objeto 
class(EH2019) # Revisando la clase o tipo de objeto 

# 3) Observando las variables y Estadistica descriptiva
EH2019$ylab
EH2019$s02a_02

Genero<-EH2019$s02a_02
mean(EH2019$ylab, na.rm=TRUE) # Promedio con observaciones faltantes 
mean(ylab, na.rm=TRUE)

describe(ylab)
summary(ylab)

table(s02a_02) # Se puede agrupar variables cualitativas
str(s02a_02)
Genero[s02a_02 == 2] <- 0
str(Genero)
table(Genero)

Salario<-ylab
Edu <- aestudio
Edad <- s02a_03
Horas<-tothrs
E_civil<-s02a_10



Data <- data.frame(Salario, Edu, Edad, Horas, Genero, E_civil)
DF <- na.omit(Data) 
attach(DF)
DF

Salario_df<-DF$Salario
Horas_df<-DF$Horas
Edad_df<-DF$Edad
edu_df<-DF$Edu
Genero_df<-DF$Genero
civil_df<-DF$E_civil

#algoritmo del salario
Ln_salario <-log(Salario_df)-log(Horas_df*4)
describe(Salario)
describe(Ln_salario)
hist(Ln_salario)
hist(Salario_df)

## "ggplot2 "ejemplo de histograma:
install.packages("ggplot2")
library(ggplot2)

ggplot(DF, aes(x = Salario_df)) +
  geom_histogram()

hist1 <- ggplot(DF, aes(x=Salario_df)) + geom_histogram() + 
  xlab("Salario Mensuales (Mes)") + 
  ylab("Frecuencia Absoluta") +
  ggtitle("Distribución del Salario, EH 2019")
hist1

# 4) Análisis de correlaciones

Exp <- Edad_df-edu_df-6
Exp2 <- Exp*Exp

plot(Ln_salario ~ Exp)
plot(Ln_salario ~ Edad_df)
cor.test(Ln_salario,Exp)

Data1 <- data.frame(Genero_df,civil_df,edu_df,Exp,Exp2)
DF1 <- na.omit(Data1) 
attach(DF1)
DF1


# 5)Generar el modelo lineal multiple
#RLM
m1 <- lm(DF1$Ln_salario ~ DF1$edu_df, DF1)
summary(m1)


MR<- lm(Ln_salario~Genero_df+civil_df+edu_df+Exp+Exp2,data=DF1)
summary(MR)

#matriz de correlación R
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

R1<-cor(DF1)
library(corrplot)
corrplot(R1)
#determinante de la matriz R
R2<-cor(DF1[,1:5])
det(R2)
#indice de Condición de la matriz R
autov<- eigen(R2)$`values`
round(sqrt(autov[1]/autov[5]),3)

#Correlaciones observadas entre pares de variables: 
round(cor(DF1),5)  

# 6)Análisis de componentes principales
library(stats)
PCA <- prcomp(DF1, scale = TRUE)
summary(PCA)

plot(PCA,type="l")
biplot(PCA,scale=0)

#Extraer dos componentes
#Tomamos las 2 primeras componentes
round(PCA$rotation[,1:2],4)

library(factoextra)
#Grafico de proporción de varianza explicada
fviz_screeplot(PCA, addlabels = TRUE, ylim = c(0, 60))


#Porcentaje de varianza total
barplot(summary(PCA)$importance[2, ])
#Realizar análisis regresión de la variable Ln_salario sobre PCA
z1<-PCA$x[,1]
z1
z2<-PCA$x[,2]
z2

regcp<-lm(Ln_salario~z1+z2,data=DF1)
regcp
summary(regcp)

#Factor de inflación de la varianza(VIF)
library(car)
vif(regcp)

#transformación para obtener los coeficientes de regresión originales
head((coefsCP<-regcp$coefficients[1:2] %*% t(PCA$rotation[,1:2])))

install.packages("Metrics")
library(Metrics) #error cuadratico
# se estima el error de predicción mediante el Error Cuadrático Medio:
ECMREG<-rmse(Ln_salario,predict(MR)) #error medio cuadrático para modelo MR
ECMPC<-rmse(Ln_salario,predict(regcp)) #error medio cuadrático para modelo regcp

ECM<- c(ECMREG,ECMPC)
ECM
plot(ECM,col=c("dark blue","dark red"),type="p",cex=1.5,
     lwd=2,xlim=c(0.5,4.5),ylim=c(0.5,4.5),
     xlab=" ",ylab="ECM")
legend("topright", inset=.02, title="Modelo de regresión",
       c("M\'ultiple","CP"), fill=c("dark blue","dark red"), horiz=TRUE,
       cex=0.9)


#Estimacion de la varianza de ambos modelos
varmod<-c(summary(MR)[[2]]^2, summary(regcp)[[2]]^2)
plot(varmod,col=c("dark blue","dark red"),type="p",cex=1.5,
     lwd=2, xlim=c(0.5,2.5), ylim=c(0.10,0.26),
     xlab=" ",ylab="Estimación de la varianza")
legend("bottomright", inset=.02, title="Modelo de regresión",
       c("M\'ultiple","PCA"), fill=c("dark blue","dark red"),
       horiz=TRUE, cex=0.9)


# Presentando los resulatados de la regresiones
install.packages("stargazer")
library(stargazer)

stargazer(MR, type="text")
stargazer(regcp, type="text")


stargazer(MR,regcp , type="text",
          title = "Resultados de las regresiones")

