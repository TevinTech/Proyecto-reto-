# Establecer el directorio de trabajo
setwd("C:/Users/KEVIN MARCA/Downloads/Proyecto Reto/Data set")

# Cargar los datos
datos <- read.csv("Data_2.csv", header = TRUE, sep = ";", dec = ",")

# Visualizar los datos
View(datos)
dimnames(datos)
labels(datos)
##attach(datos)

# An??lisis descriptivo para la variable angulo
min(datos$Velocidad)
max(datos$Velocidad)
range(datos$Velocidad)
diff(range(datos$Velocidad))

table(datos$Velocidad)
cuartiles <- quantile(datos$Velocidad, probs = c(0.25, 0.5, 0.75))
cuantil <- quantile(datos$Velocidad, probs = c(0.27))
deciles <- quantile(datos$Velocidad, probs = seq(0.1, 0.9, by = 0.1))
IQR(datos$Velocidad)

datos$veloicdadCut <- cut(datos$Velocidad,breaks = quantile(datos$Velocidad, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE)
head(datos)
absoluta <- table(datos$velocidadCut)
relativa <- round(prop.table(absoluta), 2)
summary(datos)
etiqueta <- rownames(absoluta)
porcentaje <- relativa * 100
library(ggplot2)

# Crear el histograma
ggplot(datos, aes(x = Velocidad)) +
  geom_histogram(binwidth = 0.8, fill = "skyblue", color = "black", alpha = 0.6) +
  labs(title = "Histograma de la variable Velocidad",
       x = "Velocidad",
       y = "Frecuencia")


# Crear el diagrama de caja
ggplot(datos, aes(y = Velocidad)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Diagrama de Caja: Velocidad",
       y = "Velocidad")




#MEDIDAS DE DISPERSION 

# Calcular la media, varianza y desviaci??n est??ndar de la velocidad
media <- mean(datos$Velocidad)
desviacion <- sd(datos$Velocidad)

# Crear el gr??fico de dispersi??n
plot(x = datos$Velocidad, y = datos$angulo,
     main = "Gr??fico de Dispersi??n: Velocidad vs. ??ngulo",
     xlab = "Velocidad",
     ylab = "??ngulo",
     pch = 16, col = "red")

# Crear el histograma
ggplot(data = datos, aes(x = Velocidad)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black", alpha = 0.8) +
  geom_vline(xintercept = media, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media - desviacion, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media + desviacion, color = "blue", linetype = "dashed", size = 1) +
  xlim(0,13) +
  ylim(0,11) +
  labs(title = "Histograma de la variable Velocidad",
       x = "Velocidad",
       y = "Frecuencia")
