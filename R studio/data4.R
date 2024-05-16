# Establecer el directorio de trabajo
setwd("C:/Users/KEVIN MARCA/Downloads/Proyecto Reto/Data set")

# Cargar los datos
datos <- read.csv("DatosVuelo.csv", header = TRUE, sep = ";", dec = ",")

# Visualizar los datos
View(datos)
dimnames(datos)
labels(datos)
##attach(datos)

# An??lisis descriptivo para la variable angulo
min(datos$angulo)
max(datos$angulo)
range(datos$angulo)
diff(range(datos$angulo))

table(datos$angulo)
cuartiles <- quantile(datos$angulo, probs = c(0.25, 0.5, 0.75))
cuantil <- quantile(datos$angulo, probs = c(0.27))
deciles <- quantile(datos$angulo, probs = seq(0.1, 0.9, by = 0.1))
IQR(datos$angulo)

datos$anguloCut <- cut(datos$angulo, breaks = quantile(datos$angulo, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE)
head(datos)
absoluta <- table(datos$anguloCut)
relativa <- round(prop.table(absoluta), 2)
summary(datos)
etiqueta <- rownames(absoluta)
porcentaje <- relativa * 100
#Histograma 
library(ggplot2)
# Crear el histograma con separaci??n entre columnas y velocidad en el eje y
ggplot(datos, aes(x = angulo)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histograma de la variable angulo",
       x = "??ngulo",
       y = "Velocidad")


# Crear el diagrama de caja
ggplot(datos, aes(y = angulo)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Diagrama de Caja de la variable angulo",
       y = "??ngulo")



#MEDIDAS DE DISPERSION 

media <- mean(datos$angulo)
varianza <- var(datos$angulo)
desviacion <- sd(datos$angulo)
plot(x  = datos$angulo, y = datos$velocidad0,
     main = "angulo -  velocidad ", xlab = "angulo", ylab = "velodicidad")
lines(datos$angulo,datos$velocidad0, type="l", col="red", pch= 16 )

ggplot(data=datos, aes(x= angulo))+
  geom_histogram(binwidth = 0.05, fill = "lightblue", color="black", alpha=0.8)+
  geom_vline(xintercept = media, color="red",linetype="dashed",size=1)+
  geom_vline(xintercept = media - desviacion, color ="blue", linetype="dashed", size=1)+
  geom_vline(xintercept = media + desviacion, color="blue",linetype="dashed", size=1)+
  xlim(0,02)+
  ylim(0,10)+
  labs(title="angulos", x = "velocidad",y ="Frecuencia ")


