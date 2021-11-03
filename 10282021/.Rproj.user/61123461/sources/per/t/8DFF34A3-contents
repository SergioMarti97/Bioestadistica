# Apuntes 28/10/2021

# Distribución F(m,n)
x <- seq(0, 300, length=4000)
plot(x, df(x, df1=5, df2=2), xlim=c(0, 5), xlab="x", ylab="Densidad", main="Distribución F(5, 2)", type="l")
plot(x, df(x, df1=5, df2=2), ylim=c(0, 1), xlab="x", ylab="Probabilidad", main="Distribución F(5, 2)", type="l")

# Bloque AEDM

# Análisis de componentes principales: ACP o PCA (significan lo mismo)
# Esto consiste en reducir el número de variables


#######################
# Caso práctico: Iris #
#######################

# Por ejemplo: lo que quieres ver es que el perfil de
# S. setosa es distinta a S. virginica. Como tenemos 4 variables, 
# vamos a reducir el número de variables para que de forma gráfica
# podamos representar estás nuevas variables. De esta forma, se puede ver
# con menos variables si los grupos se superponen, si se oponen, si son iguales...


# Invocamos los datos de iris, con la función data.
# Esto se puede hacer porque iris está en el core de R
data(iris)
# Así se pueden ver los nombres de las variables de iris
names(iris)

# Se pueden cambiar el nombre de las variables de esta forma
# NOTA: los arrays en R comienzan en 1
names(iris)[c(1, 2, 3, 4)] <- c("sLength", "sWidth", "pLength", "pWidth")
# Comprobamos que se han cambiado
names(iris)

# Primero hacemos un test unidimensional de las variables,
# así comprobamos: la media, los cuartiles, si tiene valores fuera de los cuartiles...
# 
# tapply es muy util porque permite hacer un summary de los grupos,
# en este caso, de especies.
# 
# Además, se puede llamar desde dentro de un bucle for
tapply(iris[,1], iris$Species, summary)

# Ahora usamos un bucle for para hacer 4 boxplots con una linea
par(mfrow=c(1,4))
for (i in 1:4) boxplot(iris[,i]~iris$Species)

# Representación de las 4 variables sin distinguir las especies
par(mfrow=c(1,1))
boxplot(iris[,1:4], col="bisque", outcol=3, outpch=15)

# Representación en 3 dimensiones, que sería lo máximo que podemos representar...
library(scatterplot3d)
scatterplot3d(iris[,1], iris[,2], iris[,3], color=as.numeric(iris[,5]), pch=as.numeric(iris[,5]))

# Además también se puede hacer la representación del QQ plot para ver la normalidad de las variables
par(mfrow=c(1,4))
for (i in 1:4) {qqnorm(iris[,i]); qqline(iris[,i], col=2)}

# Aquí ya empezamos con la covarianza entre todas las variables
pairs(iris[,1:4],pch=as.numeric(iris[,5]), col=iris[,5])

# Aquí podemos ver que hay una correlación entre pLength y pWidth... la nube de puntos
# se puede aproximar a una linea. Eso quiere decir, que quizá estas dos variables
# se puedan fusionar en la misma componente principal o variable sintética

# Donde más claro se ve es comprobando la matriz de varianza y covarianza
#
# Las funciones son cov() y cor(). Pero también utilizamos la función round(num, decimales)
# para asegurarnos de que en la matriz podamos ver los datos bien, con el mismo número 
# de decimales
round(cov(iris[,1:4]), 2) # -> Matriz de covarianza
round(cor(iris[,1:4]), 2) # -> Matriz del coef. de correlación lineal. Estos datos están estandarizados pero no tienen dimensión

# ¡Aquí podemos comprobar que sLength y sWidth serán las componentes principales!

# Representar las correlaciones
library(corrplot)
par(mfrow=c(1, 1))
corrplot.mixed(cor(iris[,1:4]), lower.col=1)

# La componente principal que obtenemos fusionando variables tendrá más información
# que la componente principal que obtenemos porque nos damos cuenta que no está relacionada
# con nadie.
#
# Todo esto ha sido la antesala a la reducción de variables

# Idoneidad del PCA o ACP: ¿El determinante de la matriz de correlacion "R" es igual a 1?
# H0: |R| = 1
# H1: |R| != 1
library("psych") # <- contiene las funciones de bartlett y KMO

# hay dos paquetes que puedes instalarte para hacer estas cosas
# REdaS

cortest.bartlett(cor(iris[,1:4]), n=150)
# El p-valor es muy pequeño así que rechazamos la hipotesis núla (H0) y aceptamos H1
# Este test tiene algunos problemas así que se complementa con el test KMO
KMO(cor(iris[,1:4])) # <- Nos da un valor relativamente mediocre (0.54), y las medidas de adecuación individuales para cada variable

# NOTA: ¡Solo hacer un análisis de componentes principales o de reducción de variables si el valor
# del estadístico general que devuelve KMO es mayor a 0.5!

library("FactoMineR") # Muy buena libreria para hacer el análisis de componentes principales
iris.pca = PCA(iris[,1:4], scale.unit = T, ncp = 4, graph = T)

# scale.unit = T sirve para estandarizar los valores. En este caso podríamos 
# haber puesto F (false) porque los datos de iris están estandarizados

# Devuelve una lista, con toda la información
iris.pca
# Valores propios de cada componente
iris.pca$eig
# Los dos primeros componentes retenien el 95.81% de la información, el resto podemos despreciarlo

# Y un resumen de lo que hemos hecho
summary.PCA(iris.pca)

# La información de los individuos no es muy relevante
# Aparecen cosas como la contribución de cada individuo a las variables
# La contribución es similar a la correlación
# En este caso en particular no aparece la 4 dimensión por la consola, pero si que está
#
# Si retengo dos variables, la comunalidad ("cos2") me ayuda a explicar: ¿Que proporción de la 
# información se queda representada/explicada con esas variables que me quedo?

# también los nombres
names(iris.pca)

# Los valores propios (lambdas en los apuntes) me dan la informarción de 
# la varianza de cada autovector, y el autovector es la forma de calcular 
# las variables sintéticas
round(iris.pca$svd$V, 2)
# Estos son los coeficientes de los autovectores para calcular los componentes principales
# 
# Ej:
# 
# Y_1 = 0.52 * sLength - 0.27 * sWidth + 0.58 * pLength + 0.56 * pWidth
# 
# El que ha programado el paquete ha cambiado las filas por las columnas 
# respecto a conforme está explicado en los apuntes

# Coordenadas de las variables de las componentes principales
head(iris.pca$var$coord) # coincide con las correlaciones de los individuos

# Todas las comunalidades, calidad de la representación
iris.pca$var$cos2
rowSums(iris.pca$var$cos2)
colSums(iris.pca$var$contrib)

# Otro grafiquito que combina los dos graficos de antes del PCA
par(mfrow=c(1,1))
biplot(iris.pca$ind$coord[,1:2], iris.pca$var$coord[,1:2], xlim=c(-4,4))

# Otra comprobación
dimdesc(iris.pca, axes=c(1,2))

# Forma alternativa de hacer lo mismo
iris.pca = PCA(iris, scale.unit = T, ncp = 5, graph = T, quali.sup=5)

# El grafico con las dimensiones reducidas
plot.PCA(iris.pca, axes=c(1, 2), choix="ind", habillage = 5)

# Aquí ya estoy reventado y no se que signfica este grafico
library("nFactors")
ap <- parallel(subject = nrow(iris[,1:4]), var = ncol(iris[,1:4]), rep = 100, cent = 0.05)
nS <- nScree(ev$values, ap$eigen$qevpea)
nS
plotnScree(nS)


############
# LEUKEMIA #
############
round(cov(leukemia[1:10]), 2) # -> Matriz de covarianza
round(cor(leukemia[,1:10]), 2)