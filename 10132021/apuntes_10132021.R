# Variables Aleatorias DISCRETAS: Modelo Binomial #

o = par(mfrow=c(2,1))
x = 0:3
plot(x, dbinom(x, size=3, prob=0.25), xlab="n. éxitos", ylab="probabilidad", xlim=c(0,3), main="Func. Probabilidad")
points(x, dbinom(x, size=3, prob=0.25), pch=16, col="green")
abline(h=0, col="blue", lty=3)

# lower.tail = True --> P(X <= 5), es la opción por defecto
# lower.tail = False --> P(X > 5), se debe de especificar

pbinom(1, size = 3, prob = 0.25, lower.tail = F)

rm(x) # Limpiar el valor de la X para evitar solapamiento

# POISON
# X=nº de niños con ojos azules nacidos en una hora ~ Poison (lambda = 10)
x = 0:20 # Nace 50 niños
dpois(x, lambda = 10) # Probabilidades, lambda = el número de 
sum(dpois(x, lambda = 10))
ppois(x, lambda = 10)

o = par(mfrow=c(2,1))
curve(dpois(x, lambda = 10), from=0, to=20, n=21, xlab="n. éxitos", ylab="probabilidad", xlim=c(0,50), main="Func. Probabilidad Poisson", type="h")
points(x, dpois(x, lambda = 10), pch=16, col="green")
abline(h=0, col="blue", lty=3)

plot(x, ppois(x, lambda=10), xlab="n. éxitos", ylab="probabilidad acumulada",main="Func. Distribución Poisson acumulada", type="s")
par(o)

# Prefijos
# d = función de densidad/probabilidad, para que una variable sigua siga una distribución 
# p = función de distribución. Ej: F(2) = P(x < 2) = 0.75
# q = cuantilica, es la inversa de la función de distribución. Ej: F(0.75) = 2
# r = genera una muestra aleatoria que sigue una distribución binomial b(n, p)

# X= nº de hijos con ojos marrones antes de tener 1 hijo con ojos azules ~ G(p=0.25)
# Probabilidad de que la familia acabe teniendo 3 o más hijos
pgeom(2, 0.25, lower.tail = F) # ¿Número de fracasos hasta conseguir un éxito?

# La distribución de los valores de expresión del gen Zyxin en paciente ALL sigue una N(mu=1.5, sigma=0.5)
# 1. Representar la función de densidad y la función de distribución
rm(x)
x=seq(-0.5, 3.5, length = 10000)

par(mfrow=c(2,1))
plot(x, dnorm(x, mean=1.5, sd=0.5), xlab="x", ylab="densidad", main="Func. Densidad", type="l")
abline(v=1.5, col="magenta", lty=3, lwd=5) # v = --> vertical || h = --> horizontal. lty = 3 --> Discontinuo. lwd = 5 grosor de la linea

plot(x, pnorm(x, mean=1.5, sd=0.5), xlab="x", ylab="probabilidad", main="Func. Probabilidad", type="l")
par(par(mfrow=c(2,1)))

# 2. Cual es la probabilidad de que los valores de expresion sean menores que 1.2?
pnorm(1.2, mean=1.5, sd=0.5)

# 3. Cual es la probabilidad de que los valores de expresion sean mayores que 1.2?
1 - pnorm(1.2, mean=1.5, sd=0.5)
# o bien, con las opciones de la función
pnorm(1.2, mean=1.5, sd=0.5, lower.tail = F)

# 4. Cual es la probabilidad de que los valores de expresion esten entre 0.8 y 2.4?
pnorm(2.4, mean=1.5, sd=0.5) - pnorm(0.8, mean=1.5, sd=0.5)

# 5. Genera una muestra aleatoria de tamaño 100 de la población dada,
# esto es, que se distribua segun una normal de parametros con media = 1.5 y desviación tipica = 0.5
set.seed(12345);rnorm(1000, 1.5, 0.5)


# Intentando cargar librerias
#library(spikeslab)
#load("DataMaster_2021.RData")
#read.csv("DataMaster_2021.RData")

# Se ha cargado desde la interfaz de R studio la base de datos leukemia.csv
# También el csv de ElPulso
# Factores
ElPulso$Actividad <- factor(ElPulso$Activity, levels=1:3, labels = c("Sauve", "Moderada", "Alta"))
ElPulso$Fumar <- factor(ElPulso$Smokes, levels=1:2, labels = c("Fuma", "No Fuma"))
ElPulso$Sexo <- factor(ElPulso$Sex, levels=1:2, labels = c("Hombre", "Mujer"))
summary(ElPulso)

# Inferencia paramétrica: se refiere a la media, la desviación... parametros de la muestra
# Pruebas de conformidad: comprobar si un valor es igual a lo que esperamos o no
# Utilizaremos las pruebas T de media, porque siguen una distribución de t-student

# Hacemos un test de conformidad para comprobar si la media del campo "Pulse1" 
# es igual a 50 (H0) o distinto a 50 (H1)
t.test(ElPulso$Pulse1, mu=50)

t.test(ElPulso$Pulse1, mu=50, conf.level = 0.99)

t.test(ElPulso$Pulse2, mu=75)

# Solo las mujeres
t.test(ElPulso$Pulse2[ElPulso$Sexo=="Mujer"], mu=75)

# Comprobar si las medias de los pulsos son distintas entre dos grupos, por ejemplo: mujeres y hombres:
# Antes de realizar un test de medias entre dos poblaciones hay que probar que las varianzas de las dos
# muestras sean iguales. Es decir, se debe de hacer un test de varianzas.
var.test(ElPulso$Pulse2~ElPulso$Sexo)
# Antes, ha salido falso, es decir que las varianzas de las dos poblaciones son distintas.
# La opción por defecto es que las varianzas son FALSE, así que no hay que hacer nada. Si fuera TRUE, 
# hay que utilizar el parametro "var.equal" (var.equal = T). Estadísticamente esto se llama "La aproximación de Wells"
t.test(ElPulso$Pulse2~ElPulso$Sexo)

# Esto es para hacer una ANOVA = ANalisis Of VAriance
aov(ElPulso$Pulse1~ElPulso$Actividad)

# Contraste para: la media de pulso 1 entre fumadores y no fumadores.
# H0: las medias son iguales. H1: las medias son distintas
var.test(ElPulso$Pulse1~ElPulso$Fumar)
t.test(ElPulso$Pulse1~ElPulso$Fumar)
