# Sesion 1 FAEDE curso 2021-22 Sesion 6/10/2021

# 1 Revusión de R

# Para actualizar la version de R
#updateR()

# Personalizar el directorio de trabajo
getwd() # directorio de trabajo por defecto
#setwd("C:/Users/fenix/Desktop/bioestadistica") # directorio de trabajo elegido por el usuario
load("ambiente.sav") # cargar algún archivo
view() # ver algún archivo

# R-Objetos
# 1 Vectores
c(1, 2, 3, 4, 5) # Un vector númerico
c(F, T, F, T, T) # Un vector lógico
c("Juan", "Sergio", "Pablo", "Maria") # Vector
x=c(1, 2, 3, 4, 5) # Crear un objeto 'x' de tipo array
x # Ver el contenido del objeto
y=1:50;y # Ejecutar dos sentencias a la vez, separar con ';'

seq(from=0, to=10, 0.5) # Función para construir un vector de 0 a 10 incremento de 0.5
seq(0, 1, length.out=14) # Función para construir un vector, de 0 a 1, pero con 11 valores (él solo calcula el incremento)
seq(1, 9, by=2) # Vector del 1 al 9 pero solo los impares

# En R, los índices no empiezan en 0 como en python, comienzancon 1

# 2 Factores: variables cualitativas
factor(letters[1:20])

# 3 Listas: variables cualitativas + cuantitativas
milista <- list(nombre="Pedro", mujer="Maria", casados=T, n.hijos=3, edad.hijos=c(1, 2, 4));milista

# 4 Matrices y arrays
matrix(1:12)
matrix(1:12, nrow=3)
n = matrix(1:12, nrow=3, byrow=T)
din(n) # dimension de la matriz
length(n) # número de elementos de la raiz

n[1,] # Primera fila, todas las columnas
n[1:5, 1:4] # Primeras 5 filas, primers 4 columnas
n[, 2:6] # todas las filas de las columnas de la 2 a la 6

# Cualitativo/Discreto: variables que definen cualidades. Ej: sexo, color del pelo, fumador-no fumador, grupos. Rango discreto de datos: Si/No, H/M, 1/2
# Cuantitativo/Continuo: variables que definen cantidades. EJ: altura, peso, distancia, pulso cardiaco. Rango de valores continuo (Ej: 120.5; 98.3; 12.0)
# Las variables cuantitativas se pueden agrupar en grupos formando variables cualtiativas. Por ejemplo: la edad es continua (16, 17, 20, 25 años...), pero puedes
# hacer grupos de edad: niños (1 - 4 años), jovenes (10 - 25 años)

# 5 Data frames
ElPulso$Peso.kg <- with(ElPulso, round(ElPulso$Weight * 0.454, 1)) 
ElPulso$Altura.cm <- with(ElPulso, round(ElPulso$Height * 2.54, 1)) 
# Añadir una columna/factor al data frame, calculada a partir de otra columna
# Primer argumento: archivo con el que trabajas. Segundo: la transformación o los datos que quiere
View(ElPulso)

# Factores
ElPulso$Actividad <- factor(ElPulso$Activity, levels=1:3, labels = c("Sauve", "Moderada", "Alta"))
ElPulso$Fumar <- factor(ElPulso$Smokes, levels=1:2, labels = c("Fuma", "No Fuma"))
summary(ElPulso)

# Convertir variables cuantitativas en cualitativas
library("car") # Cargar la libreria car para usar la función recode
library(carData)
ElPulso$Peso.int <- recode(ElPulso$Peso.kg, '40:60="N"; 60.1:80="S";80.1:100="M"', as.factor=TRUE)
View(ElPulso)

# attach -> Carga en el environment las variables del data frame
# detach -> Descarga de la memoria de las variables del data frame

data(iris) # Cargar el archivo de datos iris, por defecto en el paquete base de R

o <- par(mfrow=c(1,2)) # Permite en un mismo gráfico, dos gráficas distintas. Ej: un boxplot y una regresión
hist(iris$Sepal.Length) # Histograma
boxplot(iris$Sepal.Length~iris$Species, col=c(2, 3, 4)) # boxplot
par(o) # Desaparece el formato de gráficos dobles

boxplot(ElPulso$Pulse1~ElPulso$Peso.int, col=c(2, 3, 4)) # boxplot
boxplot(ElPulso$Pulse2~ElPulso$Peso.int, col=c(2, 3, 4)) # boxplot

boxplot(ElPulso$Pulse1~ElPulso$Actividad, col=c(2, 3, 4), ylim=c(50, 140)) # boxplot
boxplot(ElPulso$Pulse2~ElPulso$Actividad, col=c(2, 3, 4), ylim=c(50, 140)) # boxplot

# Calcular la probabilidad de que en el caso que ha explicado la profesora, la pareja tengan 1 hijo de ojos claros
dbinom(1, 3, 0.25) # 'd' = probabilidad puntal  (1) Num hijo con ojos azules (2) Num total hijos (3) Probabilidad puntual de tener un hijo con ojos azules
pbinom(1, 3, 0.25) # 'p' = probabilidad acumulada, es decir, como máximo un hijo con ojos azules (P(x<=1))
# Representar una distribución binomial (los posibles valores de probabilidad)
x <- 0:3
plot(x, dbinom(x, 3, 0.25), type="h")
