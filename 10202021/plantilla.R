#Importar el fichero leukemia.xlsx

#Visualizar el fichero importado

View(leukemia)

#Convertir la variable Y en factor con etiquetas: 0="ALL" y 1="AML"

leukemia$Y <- factor(leukemia$Y, levels=0:1, labels = c("ALL", "AML"))

######################################################################
#Test t de Student para una muestra: Prueba de conformidad de la media
######################################################################
#Contrastar si la media de expresion genica del gen Gdf5 (x.3395) de pacientes ALL es 0

t.test(leukemia$x.3395[leukemia$Y == 'ALL'], mu=0) # mu no es necesario ponerlo xq por defecto mu es 0
# No rechazamos H0, lo que significa que para este gen, los pacientes con esta enfermedad (ALL), no se está expresando

#Contrastar si la media de expresion genica del gen Gdf5 (x.3395) de pacientes AML es 0

t.test(leukemia$x.3395[leukemia$Y == 'AML'], mu=0) # 
# Rechazamos H0, la media de expresión del gen Gdf5 no es 0, es otra media

# Graficos
# --- variables utiles para representar los graficos --
limForX = c(-1.5, 1.5)
limForY = c(0, 25)
xLabel = "Niveles de expresión"
yLabel = "Frecuencia"
title = "Frecuencia de expresion del gen Gdf5 para pacientes con"

# --- dos histogramas, uno encima del otro, comparados --
o <- par(mfrow=c(2,1))
hist(leukemia$x.3395[leukemia$Y == 'ALL'], ylim=limForY, xlim=limForX, xlab=xLabel, ylab=yLabel, main=paste(title, 'ALL'))
abline(v=mean(leukemia$x.3395[leukemia$Y == 'ALL']), col='blue', lty=3)
hist(leukemia$x.3395[leukemia$Y == 'AML'], ylim=limForY, xlim=limForX, xlab=xLabel, ylab=yLabel, main=paste(title, 'AML'))
abline(v=mean(leukemia$x.3395[leukemia$Y == 'AML']), col='blue', lty=3)
rm(limForY)
rm(xLabel)
rm(yLabel)
par(o)

# -- un boxplot, para cada enfermedad ---
boxplot(leukemia$x.3395 ~ leukemia$Y, ylim=limForX, ylab='Expresión', xlab='Enfermedades', main='Expresion del gen Gdf5')
means <- aggregate(leukemia$x.3395, list(leukemia$Y), mean)
points(x=1:nrow(means), y=means$x, col='blue', pch=16)
text(
  x=1:nrow(means), 
  y=means$x - 0.25, 
  labels = paste('Media: ', round(means$x, 3)), 
  col='black')
rm(limForX)
rm(title)

#Asumiendo que los valores de expresion de CCND3 Cyclin D3 (x.1040) para pacientes ALL sigue una distribuci?n normal, contrastaremos si su media es mayor o igual que 0

t.test(leukemia$x.1040[leukemia$Y == 'ALL'], mu=0) # igual
t.test(leukemia$x.1040[leukemia$Y == 'ALL'], alternative='greater', mu=0) # mayor??

####################################################
#Test t de Student para dos muestras independientes
####################################################
#Golub et al. (1999) identifica genes expresion diferenciales entre pacientes AML y ALL, siendo uno de ellos el gen CCND3 Cyclin D3. 
#Representar gr?ficamente y comprobar que existen diferencias significativas.

boxplot(leukemia$x.1040 ~ leukemia$Y, ylab='Expresión', xlab='Enfermedades', main='Expresion del gen CCMD3 Cyclin D3')
means <- aggregate(leukemia$x.1040, list(leukemia$Y), mean)
points(x=1:nrow(means), y=means$x, col='blue', pch=16)
text(
  x=1:nrow(means), 
  y=means$x - 0.25, 
  labels = paste('Media: ', round(means$x, 3)), 
  col='black')

#Ejemplo 3.2 Analogamente testar la hipotesis nula de igualdad de varianzas y la hipotesis nula de igualdad de medias entre los pacientes ALL y AML puede ser testadas.

var.test(leukemia$x.1040 ~ leukemia$Y)

####################################################
#Test t de Student para dos muestras dependientes
###################################################

#Ejemplo. Contrastar si existen diferencias de medias entre pulso antes y pulso despues en fumadores, con alfa = 0.99.

View(ElPulso)

# --- Extraemos los datos de las dos poblaciones ---
pulsoFumAntes = ElPulso$Pulse1[ElPulso$Fumar=='Fuma']
pulsoFumDespu = ElPulso$Pulse2[ElPulso$Fumar=='Fuma']

# --- (1) Realizamos el test de varianzas ---
var.test(pulsoFumAntes, pulsoFumDespu)
# Las varianzas son iguales, p-valor: 0.492

# --- (2) Realizamos el test de medias ---
t.test(pulsoFumAntes, pulsoFumDespu, paired=T)
# Las medias son distintas, p-valor: 0.007215

# --- (3) Podemos hacer un gráfico para verlo ---
boxplot(pulsoFumAntes, pulsoFumDespu, ylim=c(40, 120), ylab='Pulsaciones / min', names=c('Antes', 'Después'), main='Comparación entre el puslo antes y después de la actividad fisica en fumadores')
means <- c(mean(pulsoFumAntes), mean(pulsoFumDespu))
points(x=1:2, y=means, col='blue', pch=16)
text(
  x=1:2, 
  y=means + 5, 
  labels = paste('Media: ', round(means, 3)), 
  col='black')


# Inferencia no paramétrica: test de bondad de ajuste y QQ-plot

qqnorm(leukemia$x.3395[leukemia$Y=='ALL']);qqline(leukemia$x.3395, col=2)
ks.test(leukemia$x.3395[leukemia$Y=='ALL'], 'norm', mean(leukemia$x.3395[leukemia$Y=='ALL']), sd(leukemia$x.3395[leukemia$Y=='ALL']))

# --- 
require(car)
qqPlot(leukemia$x.3395[leukemia$Y=='ALL'], "norm", pch=20, ylab="expresión génica", main="QQPlot")


