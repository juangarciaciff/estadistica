############################ ANALISIS ESTADISTICO - Master BI y BD  ###############################

# Hacer uso del dataset "diamonds" que contendrá el precio (entre otras variables interesantes) de unos 54.000 diamantes.
#
# Objetivo : realizar distintos tipos de análisis estadístico de sus variables para intentar
# averiguar algún tipo de comportamiento oculto aparentemente en los datos. 
#
# Para ello os marco los siguientes pasos: tipos de variables, medidas de posición central, 
# medidas de dispersión, 
# distribución y relación entre ellas, más análisis de regresión
#
# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:
# price: Precio en dolares americanos
# carat: peso del diamante
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
# x: longitud en mm 
# y: ancho en  mm 
# z: profundidad en mm 
# depth: porcentaje total de profundidad 
# table: anchura de la parte superior de diamante con relacion al punto mas ancho 

# Responde cada bloque cubriendo al menos lo indicado:

# ========================================================================================================================
# Cargamos las librerías y paquetes necesarias
# install.packages("corrplot")
# ...
# ========================================================================================================================

library(ggplot2)  # diamonds, ...
library(e1071)    # skewness, kurtosis, ...
library(corrplot)
library(plyr)
library(car)
library(lmtest)   # partial.cor
library(GGally)   # ggpairs
library(memisc)   # mtable

# ========================================================================================================================
# Pasamos el conjunto de datos diamonds a un dataframe (df).
# Visualizamos las primeras observaciones (líneas) del dataframe.
# Visualizamos información básica y resumida del dataframe (10 variables, 53.940 filas, ...)
# Las variables x, y y z son las medidas 3D. Eliminamos las observaciones que contienen x, y o z con valor 0.
# ========================================================================================================================

df <- as.data.frame(diamonds)
head(df)
str(df)

# 'data.frame':	53940 obs. of  10 variables:
# $ carat  : num  0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
# $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
# $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
# $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
# $ depth  : num  61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
# $ table  : num  55 61 65 58 58 57 57 55 61 61 ...
# $ price  : int  326 326 327 334 335 336 336 337 337 338 ...
# $ x      : num  3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
# $ y      : num  3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
# $ z      : num  2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...

# Elimimanos observaciones con x, y o z a 0
df <- subset(df, !(x<=0 | y<=0 | z<=0))
str(df)
# Se han eliminado 20 observaciones => 53.920 obs.

# Ordenamos las variables de tipo factor color y clarity según la documentación (help(diamonds))
df$color <- ordered(df$color, levels=c('J', 'I', 'H', 'G', 'F' ,'E' , 'D')) 
df$clarity <- ordered(df$clarity, levels=c('I1', 'SI1', 'SI2', 'VS1', 'VS2' ,'VVS1' , 'VVS2', 'IF')) 
levels(df$color)
levels(df$clarity)

########################################################################################################################
########################################################################################################################
# 1 - Selecciona una muestra representativa para "cut"
########################################################################################################################
########################################################################################################################

### Obtendremos una muestra mediante MUESTREO ESTRATIFICADO.
#
# En el enlace http://www.psyma.com/company/news/message/como-determinar-el-tamano-de-una-muestra
# se explica la fórmula para el cálculo del tamaño de una muestra conociendo el tamaño de la población.
#
# En el siguiente enlace hay una calculadora de tamaños de muestras:
# http://med.unne.edu.ar/biblioteca/calculos/calculadora.htm
#
# Asignamos los parámetros:
# - Porcentaje de error...........: 5%
# - Nivel de confianza............: 99%
# - Tamaño de la población........: 54.000
# - Distribución de las respuestas: 50%
#
# Y obtenemos un tamaño de muestra de 656, que redondeamos a 700.
size <- 700

### Obtenemos los valores distintos de la variable cut y el número de filas de cada uno de ellos.
table(df$cut)
# Fair      Good Very Good   Premium     Ideal 
# 1610      4906     12082     13791     21551 

### Obtenemos las frecuencias de cada uno de los valores distintos de la variable cut.
calculoMuestras <- data.frame(prop.table(table(df$cut)))
calculoMuestras
#         cut       Freq
# 1      Fair 0.02984798
# 2      Good 0.09095291
# 3 Very Good 0.22398962
# 4   Premium 0.25567297
# 5     Ideal 0.39953652

### Obtenemos el número de muestras para cada uno de de los valores distintos de la variable cut.
calculoMuestras$NumMuestras <- round(calculoMuestras$Freq * size)
calculoMuestras
#         cut       Freq NumMuestras
# 1      Fair 0.02984798          12
# 2      Good 0.09095291          36
# 3 Very Good 0.22398962          90
# 4   Premium 0.25567297         102
# 5     Ideal 0.39953652         160

### Obtenemos las muestras para cada uno de de los valores distintos de la variable cut
dfx <- subset(df, cut=='Fair')
ids <- sample(1:nrow(dfx), size=calculoMuestras$NumMuestras[1], replace=FALSE)
df1 <- dfx[ids,]

dfx <- subset(df, df$cut=='Good')
ids <- sample(1:nrow(dfx), size=calculoMuestras$NumMuestras[2], replace=FALSE)
df2 <- dfx[ids,]

dfx <- subset(df, df$cut=='Very Good')
ids <- sample(1:nrow(dfx), size=calculoMuestras$NumMuestras[3], replace=FALSE)
df3 <- dfx[ids,]

dfx <- subset(df, df$cut=='Premium')
ids <- sample(1:nrow(dfx), size=calculoMuestras$NumMuestras[4], replace=FALSE)
df4 <- dfx[ids,]

dfx <- subset(df, df$cut=='Ideal')
ids <- sample(1:nrow(dfx), size=calculoMuestras$NumMuestras[5], replace=FALSE)
df5 <- dfx[ids,]

### Obtenemos un nuevo dataframe con las muestras (dfm)
dfm <- rbind(df1, df2, df3, df4, df5)
head(dfm)

########################################################################################################################
########################################################################################################################
# 2 - Análisis de las variables
# Análisis descriptivo de las variables: Tipo de variable, distribución y representación
# Detección de casos atípicos y su tratamiento
########################################################################################################################
########################################################################################################################

### Visualizamos información básica y resumida del dataframe de la muestra
str(dfm)
# 10 variables, 400 filas, ...
# Las variables son de los tipos siguientes:
# - cualitativa: cut, color, clarity
# - cuantitativa continua: carat, depth, table, price, x, y, z

### Para detectar valores atípicos de una variable en un dataframe utilizaremos el diagrama de caja (box plot).
# Es un tipo de gráfico que utiliza los cuartiles para representar un conjunto de datos. 
# Permite observar de un vistazo la distribución de los datos y sus principales características: 
# centralidad, dispersión, simetría y tamaño de las colas.

### Para eliminar los valores atípicos se comprueba en el box plot si hay valores fuera de un rango que llamarenos mínimo a máximo.
# Para ello, obtenemos los valores máximo y mínimo y eliminamos aquellos valores tales que valor > máximo o valor < mínimo.

### Para determinar la asimetría utilizamos la función skewness:
# > 0 => (media > mediana) => la muestra asimétrica a la derecha (la cola larga a la derecha)
# < 0 => (media < mediana) => la muestra asimétrica a la izquierda (la cola larga a la izquierda)
# Hay que tener en cuenta que en distribuciones no unimodales, esta criterio no puede aplicarse de manera estricta.

### Para determinar el el grado de concentración de los valores alrededor de la zona central de la distribución utilizamos el Coeficiente de Cursotis:
# < 0 => los datos están muy dispersos (Distribución Platicúrtica)
# = 0 => la distribución es Normal (Distribución Mesocúrtica )
# > 0 => los datos están muy concentrados (Distribución Leptocúrtica) 

### Para comprobar la normalidad de una variable utilizamos el Test de Shapiro-Wilk, uno de las más utilizados y eficientes, aunque para tamaños de muestra inferiores a 5000:
# p > 0.05 => no podemos rechazar que la distribución sea normal.
# p < 0.05 => podemos rechazar que la distribución sea de tipo normal.
# NOTA: Utilizaremos este tes, anunque se utiliza en muestras de hasta 50 elementos. 
# NOTA: Otros test válios para tamaños de muestra mayores son el de Kolmogorov-Smirnov o su corrección Lilliefors.

### Para graficar las variables utilizaremos las funciones:
# boxplot........ -> diagrama de caja
# hist........... -> histograma
# density........ -> densidad
# qqnorm y qqline -> comparativa entre los cuantiles de nuestros datos y los de la distribución normal estándar N(0,1)


### ----------------------------------------------------------------------------------------------------
### Función para graficar una variable cuantitativa determinada
### ----------------------------------------------------------------------------------------------------
graficarVariable <- function(variable) {
  
  # Establecemos la distribución de gráficos en el lienzo
  par(mfrow = c(2,2))
  
  # Mostramos el diagrama de caja
  boxplot(variable)
  
  # Mostramos el histograma
  hist(variable)
  
  # Determinamos y mostramos la densidad
  plot(density(variable))
  
  # Mostramos la comparativa entre los cuantiles de nuestros datos y los de la distribución normal estándar N(0,1)
  qqnorm(variable)
  qqline(variable, col=2)
  
  # Restituímos la distribución normal de gráficos en el lienzo
  par(mfrow = c(1,1))
  
}


### ----------------------------------------------------------------------------------------------------
### Analizamos la variable carat
### ----------------------------------------------------------------------------------------------------

# Mostramos varias gráficas y el resumen estadístico:
graficarVariable(dfm$carat)
summary(dfm$carat)

# Buscamos y eliminamos los casos atípicos: según vemos en el box plot, solo hay que hacerlo para los valores por arriba:
maxVal <- quantile(dfm$carat, .75) + 1.5 * IQR(dfm$carat)
dfm <- dfm[dfm$carat < maxVal,]

# Graficamos el nuevo dataset:
graficarVariable(dfm$carat)

# Determinamos la asimetría con skewness:
skewness(dfm$carat)
# [1] 0.7378274
# La distribución es asimétrica hacia la derecha.

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(dfm$carat)
# [1] -0.3097709
# Los datos están algo dispersos (distribución es platicúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(dfm$carat)
# W = 0.91245, p-value = 2.828e-14
# Podemos rechazar que la distribución sea de tipo normal.

### ----------------------------------------------------------------------------------------------------
### Analizamos la variable depth
### ----------------------------------------------------------------------------------------------------

# Mostramos varias gráficas y el resumen estadístico:
graficarVariable(dfm$depth)
summary(dfm$depth)

# Buscamos y eliminamos los casos atípicos: según vemos en el box plot, solo hay que hacerlo para los valores por arriba y por abajo:
maxVal <- quantile(dfm$depth, .75) + 1.5 * IQR(dfm$depth)
dfm <- dfm[dfm$depth < maxVal,]
minVal <- quantile(dfm$depth, .25) - 1.5 * IQR(dfm$depth)
dfm <- dfm[dfm$depth > minVal,]

# Graficamos el nuevo dataset:
graficarVariable(dfm$depth)

# Determinamos la asimetría con skewness:
skewness(dfm$depth)
# [1] -0.1180623
# La distribución es asimétrica hacia la izquierda

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(dfm$depth)
# [1] -0.1537011
# Los datos están dispersos (distribución platicúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(dfm$depth)
# W = 0.99314, p-value = 0.09002
# No podemos rechazar que la distribución sea de tipo normal.

### ----------------------------------------------------------------------------------------------------
### Analizamos la variable table
### ----------------------------------------------------------------------------------------------------

# Mostramos varias gráficas y el resumen estadístico:
graficarVariable(dfm$table)
summary(dfm$table)

# Buscamos y eliminamos los casos atípicos: según vemos en el box plot, solo hay que hacerlo para los valores por arriba:
maxVal <- quantile(dfm$table, .75) + 1.5 * IQR(dfm$table)
dfm <- dfm[dfm$table < maxVal,]

# Graficamos el nuevo dataset:
graficarVariable(dfm$table)

# Determinamos la asimetría con skewness:
skewness(dfm$table)
# [1] 0.4322443
# La distribución es asimétrica hacia la derecha.

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(dfm$table)
# [1] -0.1660508
# Los datos están dispersos (distribución platicúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(dfm$table)
# W = 0.96179, p-value = 3.974e-08
# Podemos rechazar que la distribución sea de tipo normal.


### ----------------------------------------------------------------------------------------------------
### Analizamos la variable price
### ----------------------------------------------------------------------------------------------------

# Mostramos varias gráficas y el resumen estadístico:
graficarVariable(dfm$price)
summary(dfm$price)

# Buscamos y eliminamos los casos atípicos: según vemos en el box plot, solo hay que hacerlo para los valores por arriba:
# Lo repetinos varias veces, hasta eliminar todos los casos atípicos.
maxVal <- quantile(dfm$price, .75) + 1.5 * IQR(dfm$price)
dfm <- dfm[dfm$price < maxVal,]

# Graficamos el nuevo dataset:
graficarVariable(dfm$price)

# Determinamos la asimetría con skewness:
skewness(dfm$price)
# [1] 1.169368
# La distribución es asimétrica hacia la derecha.

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(dfm$price)
# [1] 0.5200813
# Los datos están concentrados (distribución leptocúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(dfm$price)
# W = 0.84145, p-value < 2.2e-16
# Podemos rechazar que la distribución sea de tipo normal.


### ----------------------------------------------------------------------------------------------------
### Analizamos la variable x
### ----------------------------------------------------------------------------------------------------

# Mostramos varias gráficas y el resumen estadístico:
graficarVariable(dfm$x)
summary(dfm$x)

# Buscamos y eliminamos los casos atípicos: según vemos en el box plot, no parece que los haya.

# Determinamos la asimetría con skewness:
skewness(dfm$x)
# [1] 0.4124855
# La distribución es asimétrica hacia la derecha

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(dfm$x)
# [1] -1.031263
# Los datos están dispersos (distribución platicúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(dfm$x)
# W = 0.93147, p-value = 4.438e-11
# Podemos rechazar que la distribución sea de tipo normal.


### ----------------------------------------------------------------------------------------------------
### Analizamos la variable y
### ----------------------------------------------------------------------------------------------------

# Mostramos varias gráficas y el resumen estadístico:
graficarVariable(dfm$y)
summary(dfm$y)

# Buscamos y eliminamos los casos atípicos: según vemos en el box plot, no parece que los haya.

# Determinamos la asimetría con skewness:
skewness(dfm$y)
# [1] 0.4058016
# La distribución es asimétrica hacia la derecha

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(dfm$y)
# [1] -1.05981
# Los datos están dispersos (distribución platicúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(dfm$y)
# W = 0.92997, p-value = 3.16e-11
# Podemos rechazar que la distribución sea de tipo normal.


### ----------------------------------------------------------------------------------------------------
### Analizamos la variable z
### ----------------------------------------------------------------------------------------------------

# Mostramos varias gráficas y el resumen estadístico:
graficarVariable(dfm$z)
summary(dfm$z)

# Buscamos y eliminamos los casos atípicos: según vemos en el box plot, no parece que los haya.

# Determinamos la asimetría con skewness:
skewness(dfm$z)
# [1] 0.3828598
# La distribución es asimétrica hacia la derecha

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(dfm$z)
# [1] -1.075595
# Los datos están dispersos (distribución platicúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(dfm$z)
# W = 0.9323, p-value = 5.366e-11
# Podemos rechazar que la distribución sea de tipo normal.


### ----------------------------------------------------------------------------------------------------
### Analizamos la variable cut
### ----------------------------------------------------------------------------------------------------

# La variable cut es cualitativa, por lo que hay que convertirla a valores cuantitativos para poder trabajar con ella.
table(dfm$cut)
numCut <- ifelse(dfm$cut == "Fair", 1, 
                   ifelse(dfm$cut == "Good", 2, 
                          ifelse(dfm$cut == "Very Good", 3,
                                 ifelse(dfm$cut == "Premium", 4, 
                                        5)
                                 )
                          )
                   )
table(numCut)

# Mostramos varias gráficas:
graficarVariable(numCut)

# Determinamos la asimetría con skewness:
skewness(numCut)
# [1] -0.6040109
# La distribución es asimétrica hacia la izquierda.

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(numCut)
# [1] -0.873376
# Los datos están dispersos (distribución platicúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(numCut)
# W = 0.81047, p-value < 2.2e-16
# Podemos rechazar que la distribución sea de tipo normal.


### ----------------------------------------------------------------------------------------------------
### Analizamos la variable color
### ----------------------------------------------------------------------------------------------------

# La variable color es cualitativa, por lo que hay que convertirla a valores cuantitativos para poder trabajar con ella:
table(dfm$color)
numColor <- ifelse(dfm$color == "D", 7, 
                   ifelse(dfm$color == "E", 6,
                            ifelse(dfm$color == "F", 5,
                                   ifelse(dfm$color == "G", 4,
                                          ifelse(dfm$color == "H", 3,
                                                 ifelse(dfm$color == "I", 2, 
                                                        1)
                                                 )
                                          )
                                   )
                            )
                     )
table(numColor)

# Mostramos varias gráficas:
graficarVariable(numColor)

# Determinamos la asimetría con skewness:
skewness(numColor)
# [1] -0.1017736
# La distribución es asimétrica hacia la izquierda.

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(numColor)
# [1] -0.6172897
# Los datos están dispersos (distribución platicúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(numColor)
# W = 0.94518, p-value = 4.053e-10
# Podemos rechazar que la distribución sea de tipo normal.


### ----------------------------------------------------------------------------------------------------
### Analizamos la variable clarity
### ----------------------------------------------------------------------------------------------------

# La variable clarity es cualitativa, por lo que hay que convertirla a valores cuantitativos para poder trabajar con ella:
table(dfm$clarity)
numClarity <- ifelse(dfm$clarity == "I1", 9, 
                   ifelse(dfm$clarity == "IF", 8,
                          ifelse(dfm$clarity == "VVS2", 7,
                                 ifelse(dfm$clarity == "VVS1", 6,
                                        ifelse(dfm$clarity == "VS2", 5,
                                               ifelse(dfm$clarity == "VS1", 4,
                                                      ifelse(dfm$clarity == "SI2", 3,
                                                             ifelse(dfm$clarity == "SI1", 2, 
                                                                    1)
                                                      )
                                               )
                                        )
                                 )
                          )
                   )
)
table(numClarity)

# Mostramos varias gráficas:
graficarVariable(numClarity)

# Determinamos la asimetría con skewness:
skewness(numClarity)
# [1] -0.9504786
# La distribución es asimétrica hacia la izquierda.

# Determinamos el grado de concentración de los valores alrededor de la zona central de la distribución con el Coeficiente de Cursotis:
kurtosis(numClarity)
# [1] -0.2834505
# Los datos están dispersos (distribución platicúrtica).

# Determinamos la normalidad de la variable con el Test de Shapiro-Wilk:
shapiro.test(numClarity)
# W = 0.83209, p-value < 2.2e-16
# Podemos rechazar que la distribución sea de tipo normal.

### ----------------------------------------------------------------------------------------------------
# En el aparatdo 4 del ejercicio se intentará coNstruir un modelo para predecir la variable precio, 
# y para ello se tendrá en cuenta el coeficiente de variación de Pearson:
CVP <- function(x) {
  (sd(x) / abs(mean(x)))
}
CVP(dfm$table)
CVP(dfm$depth)
CVP(dfm$carat)
CVP(dfm$price)
CVP(dfm$x)
CVP(dfm$y)
CVP(dfm$z)
# Podemos ver que las variables con mayor variabilidad (valor más alto del coeficiente de variacion de Pearson) son carat y price.

########################################################################################################################
########################################################################################################################
# 3 - Inferencia
# Calcula un intervalo de confianza para la media de "carat" y "depth"
# Formula un test de hipotesis
########################################################################################################################
########################################################################################################################

### Para calcular el intervarlo de confianza utilizamos la función t.test (0,95 por defecto)

### --------------------------------------------------------------------------------------------------------------------
###  Calculamos el intervalo de confianza (95%) para carat (peso del diamante) y mostramos una gráfica.
### --------------------------------------------------------------------------------------------------------------------

t.test(dfm$carat)
intervalo <- t.test(dfm$carat)$conf.int[1:2]
valinf <- intervalo[1]
valsup <- intervalo[2]
cat ("Intervalo de confianza para la media de carat (", mean(dfm$carat), "): [", valinf, ",", valsup , "]")
par(mfrow=c(1,1))
plot(density(dfm$carat))
abline(v=mean(dfm$carat), col="blue", lty=2, lwd=1)
abline(v=valinf, col="red", lty=2, lwd=1)
abline(v=valsup, col="red", lty=2, lwd=1)

### --------------------------------------------------------------------------------------------------------------------
###  Calculamos el intervalo de confianza (95%) para depth (porcentaje de profundidad) y mostramos una gráfica.
### --------------------------------------------------------------------------------------------------------------------

t.test(dfm$depth)
intervalo <- t.test(dfm$depth)$conf.int[1:2]
valinf <- intervalo[1]
valsup <- intervalo[2]
cat ("Intervalo de confianza para la media de depth(", mean(dfm$depth), "): [", valinf, ",", valsup , "]")
par(mfrow=c(1,1))
plot(density(dfm$depth))
abline(v=mean(dfm$depth), col="blue",lty=2, lwd=1)
abline(v=valinf, col="red", lty=2, lwd=1)
abline(v=valsup, col="red", lty=2, lwd=1)

### --------------------------------------------------------------------------------------------------------------------
### Formulamos la siguiente hipótesis:
### --------------------------------------------------------------------------------------------------------------------
# La hipotesis nula (la que vamos a contrastar) es que la media de las dos muestras son iguales:
# la media de los precios (price) de los diamentes de tipo Ideal, es igual a la media de los precios de los de tipo Premium, 
# con un nivel de confianza de 95%.

### --------------------------------------------------------------------------------------------------------------------
### Test de hipotesis utilizando la formula de contraste de media para dos muestras que asumimos independientes.
### --------------------------------------------------------------------------------------------------------------------

# Obtenemos los precios de los diamantes de tipo Ideal y Premium y los mostramos en una gráfica box plot para compararlos
pricePremium <- dfm[dfm$cut=="Premium", ]
priceIdeal <- dfm[dfm$cut=="Ideal", ]
boxplot(pricePremium, priceIdeal)
t.test(pricePremium$price, priceIdeal$price)

# Como hemos considerado un nivel de confianza de 95%, para poder rechazar la hipótesis nula (H0) el p-value no puede ser superior a 0.05 (5%).
# El resultado del test p-value = 0.002689, por lo que rechazamos H0 => las medias de los diamentes de tipo Ideal y Premium no son iguales.

########################################################################################################################
########################################################################################################################
# 4 - Relaciones entre las variables
# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlacion)
########################################################################################################################
########################################################################################################################

### --------------------------------------------------------------------------------------------------------------------
### DEPENDENCIA y CORRELACIÓN
### --------------------------------------------------------------------------------------------------------------------

# Funcion para hacer el test de correlacion entre variables del dataset:
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1 : (n - 1)) {
    for (j in (i + 1) : n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# En primer lugar reemplazamos los valores categóricos por valores numéricos.

dfm$cut <- ifelse(dfm$cut == "Fair", 1, 
                 ifelse(dfm$cut == "Good", 2, 
                        ifelse(dfm$cut == "Very Good", 3,
                               ifelse(dfm$cut == "Premium", 4, 
                                      5)
                        )
                 )
)

dfm$color <- ifelse(dfm$color == "D", 7, 
                   ifelse(dfm$color == "E", 6,
                          ifelse(dfm$color == "F", 5,
                                 ifelse(dfm$color == "G", 4,
                                        ifelse(dfm$color == "H", 3,
                                               ifelse(dfm$color == "I", 2, 
                                                      1)
                                        )
                                 )
                          )
                   )
)

dfm$clarity <- ifelse(dfm$clarity == "I1", 9, 
                     ifelse(dfm$clarity == "IF", 8,
                            ifelse(dfm$clarity == "VVS2", 7,
                                   ifelse(dfm$clarity == "VVS1", 6,
                                          ifelse(dfm$clarity == "VS2", 5,
                                                 ifelse(dfm$clarity == "VS1", 4,
                                                        ifelse(dfm$clarity == "SI2", 3,
                                                               ifelse(dfm$clarity == "SI1", 2, 
                                                                      1)
                                                        )
                                                 )
                                          )
                                   )
                            )
                     )
)

# Mostramos gráficas de correlación de variables
par(mfrow = c(2,2))
corrplot(cor(dfm), method="number")
corrplot(cor(dfm), type="upper")
corrplot(cor(dfm), type="upper", order="hclust",  p.mat = p.mat, sig.level = 0.01)
p.mat <- cor.mtest(dfm)
cor.test(dfm$cut,dfm$table)
# En las gráficas podemos observar que las variables peso (carat), precio (price) y tamaño (x, y z) están relacionadas.

### Otra gráfica que aporta información interesante es la siguiente (donde hemos quitado las variables categóricas):s
ggpairs(dfm[,c(-2, -3, -4)])


### --------------------------------------------------------------------------------------------------------------------
### ANOVA
### --------------------------------------------------------------------------------------------------------------------

# Anova requiere que las poblaciones sean normales, que las muestras sean independientes, y que tengan igual varianza.
# Aunque no se cumplen las premisas, haremos un ejercicio sobre price y carat.

anova_price_carat <- aov(dfm$price ~ dfm$carat)
summary(anova_price_carat)

# Df    Sum Sq   Mean Sq F value Pr(>F)    
# dfm$carat     1 4.009e+09 4.009e+09    3652 <2e-16 ***
#   Residuals   619 6.796e+08 1.098e+06                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Vemos que hay diferencias significativas entre las medias de price y carat


####################################################################################
# 5 - Analisis de regresion
# Formular un modelo de regresion y analiza los resultados
# Muestra los residuos y analiza los resultados
# Aplica una transformacion a la regresion y analiza los resultados
# Interpreta los coeficientes estandarizados de la regresion
####################################################################################

# Vamos a intentar explicar cómo se puede determinar el precio del diamante en función de sus características.
# Nuestra variabe dependiente (variable a explicar) es el precio (price)
# Las variables monetarias, como precio, son altamente asimétricas,
# por lo que es conveniente aplicar una transformación que conduzca a una distribución normal.
# Para este tipo de variable se utilizan métodos de transformación logarítmica.

ggplot(aes(x=log(price)), data=dfm) + ggtitle("Log(price) distribution") + geom_histogram(fill='green', color='gray')

# Vemos que tras la transformación logarítmica la distribución pasa a bimodal.

# Vimos anteriormente (con el Coeficiente de Peason) que las variables com mayor variabilidad son price y carat.
# Por ello, haremos la primera regresión lineal con estas ds varables.
# Luego iremos incorporando otras variables (cut, color y clarity)

m1 <-lm(formula = log10(price) ~ carat, data = dfm)
m2 <-lm(formula = log10(price) ~ carat + cut, data = dfm)
m3 <-lm(formula = log10(price) ~ carat + cut + color, data = dfm)
m4 <-lm(formula = log10(price) ~ carat + cut + color + clarity, data = dfm)
mtable(m1, m2, m3, m4)
# ...
# adj. R-squared      0.9        0.9        0.9        0.9  
# ...
# Como podemos ver, los adj. R-squared son muy altos, por lo que los modelos son capaces de explicar gran parte de la variabilidad en 'price'.

# Para analizar los residuos obtenemos la tabla de análisis de la varianza de los errores
anova(m4)
residuos <-rstandard(m4)
valores_ajustados <-fitted(m4)
plot(valores_ajustados, residuos)
# En la gráfica no se observa ningún patrón especial, por lo que tanto la homocedasticidad como la linealidad resultan hipótesis razonables.
# No obstante, la hipótesis de normalidad se suele comprobar mediante un QQ plot de los residuos y mediante un histograma.

qqnorm(residuos)
qqline(residuos)
hist(residuos)
# Como vemos, se puede asumir la independencia.

