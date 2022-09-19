library(readr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(fda.usc)
library(fda)
library(ftsa)
library(tictoc) # medir el tiempo de ejecucion
library(fields) # para el grafico de calor (covarianza)

## Lectura de la base de datos y datos 2021, guardados
data <- as.data.frame(read_csv("02Datos/EMBI2021.csv"))
embi2021 <- data[,c('MesDia','2021')]
embi <- as.data.frame(read_csv("02Datos/EMBI.csv")) # sin NA's

rownames(embi) <- embi[,1]
embi <- embi[,-1]
embi <- as.data.frame(embi)

## Transformar a objeto fdata (functional data)
embit <- t(embi) # transponemos 
argvals <- 1:365
embif <- fdata(embit, argvals = argvals, 
               names = list(main='EMBI',
                            xlab='Mes-Dia',ylab='Anio'))

is.fdata(embif)
dim(embif)
rangeval(embif)

## Buscar K y lambda, adecuados para el SUAVIZAMIENTO

l <- c(0, 2^seq(-2, 9, length.out = 30))
length(l)
nb <- seq(7, 41, by = 2)
length(nb)
opt <- optim.basis(embif, lambda = l, numbasis = nb, 
                   type.CV = GCV.S, type.basis = 'bspline')

min(opt$gcv)
K <- opt$numbasis.opt
lambda <- opt$lambda.opt

## De los pares de (K,l) se observa el menor GCV
gcv <- as.data.frame(opt$gcv)
colnames(gcv) <- round(as.numeric(colnames(gcv)), 2)

gcv$nb <- as.numeric(rownames(gcv)) ## creo nueva columna
ind1 <- melt(gcv, id.vars = 'nb') 

ind1  %>% ggplot(aes(x = nb, y = value,  col = variable, group = variable)) +
  labs(x = "Numero Bases", y = 'GCV')  +
  geom_point()

######################
## Ajuste de los datos
######################

## 1era forma: utilizando lo dado por optim.basis
dataf <- embif
dataf$data <- embif$data%*%opt$S.opt  # producto X*S
plot(dataf, main=" Suavizamiento con B-spline")

## 2da forma: utilizando la funcion S.basis
rango <- c(1,365)
bs <- create.bspline.basis(rangeval = rango, 
                           norder = 4,
                           nbasis = K)

S  <-  S.basis(embif$argvals, bs, lambda = lambda)
dataf <- embif
dataf$data <- embif$data%*%S  # producto X*S
color <- c(1:8, 10:22)
plot(dataf, main="Datos Funcionales EMBI", col = color , xlim = c(0,395),xaxt="n")
axis(1, at = seq(0, 365, by = 25), las=2)
legend(370,5700, c('2000', '2001', '2002', '2003', '2004', '2005',
                   '2006', '2007', '2008', '2009', '2010', '2011',
                   '2012', '2013', '2014', '2015', '2016', '2017',
                   '2018', '2019', '2020'),
       col = c(1:8, 10:22), cex = 0.47,  lwd = 1)
text(80, 5400, '2020', cex=0.45)
text(320, 4800, '2008', cex=0.45)
text(0, 5000, '2009', cex=0.45)
text(0, 3100, '2000', cex=0.45)

## Objeto fd, para ver cada curva y coeficientes de la combinacion lineal
embifd <- fdata2fd(embif, type.basis = 'bspline',
                   nbasis = K,
                   lambda = lambda)

plotfit.fd(t(embit)[,'2000'], 1:365, embifd['2000'])
plotfit.fd(t(embit)[,'2001'], 1:365, embifd['2001'])
plotfit.fd(t(embit)[,'2002'], 1:365, embifd['2002'])
plotfit.fd(t(embit)[,'2003'], 1:365, embifd['2003'])
plotfit.fd(t(embit)[,'2004'], 1:365, embifd['2004'])
plotfit.fd(t(embit)[,'2005'], 1:365, embifd['2005'])
plotfit.fd(t(embit)[,'2006'], 1:365, embifd['2006'])
plotfit.fd(t(embit)[,'2007'], 1:365, embifd['2007'])
plotfit.fd(t(embit)[,'2008'], 1:365, embifd['2008'])
plotfit.fd(t(embit)[,'2009'], 1:365, embifd['2009'])
plotfit.fd(t(embit)[,'2010'], 1:365, embifd['2010'])
plotfit.fd(t(embit)[,'2011'], 1:365, embifd['2011'])
plotfit.fd(t(embit)[,'2012'], 1:365, embifd['2012'])
plotfit.fd(t(embit)[,'2013'], 1:365, embifd['2013'])
plotfit.fd(t(embit)[,'2014'], 1:365, embifd['2014'])
plotfit.fd(t(embit)[,'2015'], 1:365, embifd['2015'])
plotfit.fd(t(embit)[,'2016'], 1:365, embifd['2016'])
plotfit.fd(t(embit)[,'2017'], 1:365, embifd['2017'])
plotfit.fd(t(embit)[,'2018'], 1:365, embifd['2018'])
plotfit.fd(t(embit)[,'2019'], 1:365, embifd['2019'])
plotfit.fd(t(embit)[,'2020'], 1:365, embifd['2020'])

View(embifd$coefs)

## Curvas estimadas por fd
t <- 1:365
phi <- eval.basis(t, bs)
View(phi)  #365*37
View(embifd$coefs) #37*21

Xest <- t(phi%*%embifd$coefs)
# comparacion fdata y fd
View(Xest)
View(opt$fdata.est$data)

######################
## Analisis Exploratorio
######################

### Calculo del area de las curvas: Regla de Simpson
Area <- function(a=1, b=365){
  x <- vector()
  h <- 1
  for(j in 1:21){
    Int <- 0
    x0 <- a
    for(i in 1:364){
      x1 <- x0 + h
      x.med <- (x1 + x0)/2
      Int <- Int + (h/6)*(eval.fd(x0, embifd)[j] +
                            4*eval.fd(x.med, embifd)[j] +
                            eval.fd(x1, embifd)[j])
      x0 <- x1
    }
    x <- c(x, Int)
  }
  return(x)
}

A <- Area()
A
area <- data.frame('Xi' = 2000:2020, 'Area' = A)
area %>% ggplot(aes(x = as.character(Xi), y = Area)) +
  labs(x = 'X_i(t)',  y= 'Area') +
  geom_point(col = 'blue') +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) 

### Derivadas
# con nderiv = 0, se tienen las curvas ajustadas originales
der1 <- fdata.deriv(embif, nderiv = 1, nbasis = K,
                    lambda = lambda, method='bspline')
plot(der1, main= "EMBI' ", col = color, xaxt="n")
axis(1, at = seq(0, 365, by = 25), las=2)

der2 <- fdata.deriv(embif, nderiv = 2, nbasis = K,
                    lambda = lambda, method='bspline')
plot(der2, main= "EMBI'' ", col = color, xaxt="n")
axis(1, at = seq(0, 365, by = 25), las=2)

### Deteccion de Datos Funcionales Atipicos
### Fraiman y Muniz
FM <- depth.FM(dataf, trim = 0.15)
FM  # tambien se imprimen los Fn para cada t
FM$dep 
FM$lmed # mediana
FM$ltrim # son las curvas que utilizan para sacar la media recortada
plot(FM, ylim = c(0,5900))

### Modal
MD <- depth.mode(dataf, trim = 0.15)
MD$dep
plot(MD, ylim = c(0,6100))
MD$lmed 
MD$ltrim 

### Algoritmo Bootstrap (ambos metodos)

tic("sleeping")
outFM <- outliers.depth.trim(dataf, dfunc = depth.FM, smo = 0.1,
                             nb = 200, trim = 0.15)
toc()
plot(dataf,col="grey", main = 'Outliers - FM')
lines(dataf[outFM$outliers], type = 'l', lwd = 2, lty = 2, col = 'red')

tic("sleeping")
outMD <- outliers.depth.trim(dataf, dfunc = depth.mode, smo = 0.1,
                             nb = 200,  trim = 0.15)
toc()
plot(dataf,col="grey", main = 'Outliers - MD')
lines(dataf[outMD$outliers], type = 'l', lty = 2, lwd = 1.2, col = 'red')

### Medidas Estadisticas
embiF <- dataf[-c(1,9,10,21)] # retiro atipicos

## Media
medf <- func.mean(embiF)
plot(embiF, xlim = c(0,400), col = 1:17, main = 'Funcion Media', xlab = NA, ylab = '')
legend(370,2000, c('2001', '2002', '2003', '2004', '2005',
                   '2006', '2007', '2010', '2011', '2012', 
                   '2013', '2014', '2015', '2016', '2017',
                   '2018', '2019'),
       col = 1:17, cex = 0.47,  lwd = 1)
plot(medf, col ='red', lwd = 4, add = T)
View(medf$data)
max(medf)
min(medf)

## SD
sdf <- func.var(embiF)^0.5
plot(embiF, ylim= c(200, 2100),  col = 1:17, main = 'Funcion Desviacion Estandar', 
     xlab = NA, ylab = NA)
plot(sdf, col ='red', lwd = 4, add = T)
min(sdf)
max(sdf)

## Var-Cov
embiFD <- fdata2fd(embiF, nbasis = K, lambda = lambda)
embiCOV <- var.fd(embiFD)
teval <- eval.bifd(1:365, 1:365, embiCOV)
dev.new()
image.plot(1:365, 1:365, teval, xlab = 'Mes-Dia',
           ylab = 'Mes-Dia', main  = 'Covarianza',
           cex.axis = 1.5, cex.lab = 1.5)

## Correlacion
embiCORR <- cor.fd(1:365, embiFD)
dev.new()
image.plot(1:365, 1:365, embiCORR, xlab = 'Mes-Dia',
           ylab = 'Mes-Dia', main = 'Correlacion',
           cex.axis = 1.5, cex.lab = 1.5)

############################
###### Aplicacion FPCA
############################
embifd1 <- embifd[-c(1,9,10,21)] # reserva

### para comparar y usar despues el f1
embif1 <- fdata(embifd1)
dataf <- embif1
S1  <-  S.basis(embif1$argvals, bs, lambda = lambda)
dataf$data <- embif1$data%*%S1  # producto X*S
plot(dataf, main="Suavizamiento con B-splines")#, ylim = c(0,3500),lty=2)

plotfit.fd(t(embit)[,'2019'], 1:365, embifd1['2019']) # para comparar
###

embiPCA <- pca.fd(embifd1, nharm = 4)
embiPCA$varprop
varPC <- embiPCA$values  ## Varianza total por componente
embiPCA$harmonics$coefs # coefcientes de la combinacion lineal de las base 

# Grafica de valores propios = varianza
ggplot(data = data.frame('nb' = 1:8, 'lambda'=embiPCA$values[1:8]),
       aes(x = nb, y = lambda)) +
  labs(x = 'Componentes',  y= 'Varianza') +
  geom_point(col = 'blue') + geom_line(col = 'blue')

# Grafica del porcentaje acumulado explicado de la variacion total
propVAR <- cumsum(embiPCA$values[1:8])/sum(embiPCA$values)
ggplot(data = data.frame('nb' = 1:8, 'Var'= propVAR),
       aes(x = nb, y = Var)) +
  geom_hline(yintercept=0.98, color='red') +
  labs(x = 'Componentes',  y= 'Porcentaje Varianza Acumulada') +
  geom_point(col = 'blue') + geom_line(col = 'blue')

# Grafica de cada componente principal
FPC <- embiPCA$harmonics
View(FPC$basis)
FPCvals <- eval.fd(1:365, FPC)

plot(1:365, FPCvals[,1], xlab = 'Dia', ylab = 'FPC1',
     lwd = 2, lty = 1,  type = 'l')
plot(1:365, FPCvals[,2], xlab = 'Dia', ylab = 'FPC2',
     lwd = 2, lty = 1, col = 2, type = 'l')
plot(1:365, -FPCvals[,3], xlab = 'Dia', ylab = 'FPC3',
     lwd = 2, lty = 1, col = 3, type = 'l')
plot(1:365, FPCvals[,4], xlab = 'Dia', ylab = 'FPC4',
     lwd = 2, lty = 1, col = 4, type = 'l')

# Todas las FPC
matplot(1:365, FPCvals, xlab = 'Dias', ylab = 'FPCs',
        lwd = 2, lty = 1, type = 'l')
legend(0, -0.05, c('PC1', 'PC2', 'PC3', 'PC4'),
       col = 1:4, lty = 1, lwd = 2, cex = 0.5)

## Perturbaciones en la media
plot.pca.fd(embiPCA, xlab = 'Mes-DIa')

## Proporcion de los scores de las FPC
scores <- embiPCA$scores
rownames(scores) <- rownames(embif1$data)
scores <- sapply(1:4, function(i){scores[,i]^2/(17*varPC[i])})
scores <- as.data.frame(scores)

scores %>% ggplot(aes(x=rownames(scores), y = V1)) + 
  geom_bar(stat="identity", fill = '#BB8FCE', position="stack") + 
  labs(x = 'X_i(t)',  y= '% f(1,i)') +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) 

scores %>% ggplot(aes(x=rownames(scores), y = V2)) + 
  geom_bar(stat="identity", fill = '#85C1E9', position="stack") + 
  labs(x = 'X_i(t)',  y= '% f(2,i)') +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) 

scores %>% ggplot(aes(x=rownames(scores), y = V3)) + 
  geom_bar(stat="identity",  fill = '#F8C471',position="stack") + 
  labs(x = 'X_i(t)',  y= '% f(3,i)') +
  theme(axis.text.x = element_text(angle = 45,  vjust=1, hjust=1))

scores %>% ggplot(aes(x=rownames(scores), y = V4)) + 
  geom_bar(stat="identity", fill = '#A3E4D7', position="stack") + 
  labs(x = 'X_i(t)',  y= '% f(4,i)') +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))


############################
###### Modelo
############################

x <- outMD$Dep[-c(1,9,10,21)]
x  <- x  %>% as.matrix() %>% rownames() %>% as.numeric()
matriz <- t(Xest)[,x]  # tomo la matriz de las curvas ajustadas (m x n)
colnames(matriz1) <- 1:17  ## renombro porque en ftsm debe considerarse un orden cronologico.

embifts <- fts(1:365, matriz, xname = "Mes-Dia", 
                yname = "Embi anual") # objeto fts
plot(embifts)  # grafico de las curvas (lo antes obtenido)

## Modelo por componentes principales
modelo <- ftsm(embifts, order=4, method = 'classical')
modelo$coeff # puntuaciones de los datos en las componentes (lo antes obtenido)
modelo$y$y      # lo original (ajustado por curvas)
modelo$fitted$y # curvas estimadas por el modelo

  ### SUPUESTOS DEL AJUSTE DEL MODELO
err <- modelo$residuals$y

### Independencia
  ## Rachas
ind.err <- vector()
library(tseries)
for(i in 1:nrow(err)){ 
  test <- tseries::runs.test(as.factor(err[i,] > mean(err[i,])))$p.value
  ind.err <- c(ind.err, test) 
}
ind.err < 0.05

ggplot(data = data.frame('t' = 1:365, 'p.valor'= ind.err),
       aes(x = t, y = p.valor)) + 
  geom_point(col = 'blue') +
  geom_hline(yintercept=0.05, color='red', lwd = 0.5)


  ## Ljung-Box
ind.err <- vector()
for(i in 1:nrow(err)){ 
  test <- Box.test(err[i,], lag=3, type="Ljung-Box")$p.value
  ind.err <- c(ind.err, test) 
}
ind.err < 0.05

ggplot(data = data.frame('t' = 1:365, 'p.valor'= ind.err),
       aes(x = t, y = p.valor)) + 
  geom_point(col = 'blue') +
  geom_hline(yintercept=0.05, color='red', lwd = 0.5)

###### Media
med.err <- vector()
for(i in 1:nrow(err)){ med.err <- c(med.err, mean(err[i,])) }

ggplot(data = data.frame('t' = 1:365, 'media.error'= med.err),
       aes(x = t, y = media.error)) + ylim(-0.5,0.5) + 
  geom_point(col = 'blue') +
  geom_hline(yintercept=0, color='red', lwd = 1)


library(caret)
y <- as.vector(modelo$fitted$y)
y_ <-as.vector(modelo$y$y)
R2(y , y_)
summary(modelo)
mean((y - y_)^2)
-2*log(mean((y - y_)^2)/(17*365)) + 2*4
-2*log(mean((y - y_)^2)/(17*365)) + 4*log(17*365)

##### MEJOR CASO

## Prediccion de la funcion 2021+
deps <- outMD$Dep[-c(1,9,10,21)] # tomo las medidas de profundidad (sin considerar los ya atipicos)
orden.asc <- deps %>% sort()  %>% as.matrix() %>% rownames() %>% as.numeric()
# Ascendente: las ultimas tienen mas profundidad,
# por tanto son las curvas con comportamiento mas normal.

matriz1 <- t(Xest)[,orden.asc]  
colnames(matriz1) <- 1:17  

embifts1 <- fts(1:365, matriz1, xname = "Mes-Dia", 
                yname = "Embi anual") # objeto fts
modelo1 <- ftsm(embifts1, order=4, method = 'classical')


a <- 0.6 # para el suavizado exponencial
pred1 <- forecast(modelo1, h=1, method = 'ets', alpha = a, model = 'ANN',
                  jumpchoice = 'actual', level = 0)

plot(pred1, 'components', xlab2 = 'Datos Funcionales',
     ylab2 = 'puntuaciones') # serie de las puntuaciones
  # dibuja la curva estimada, las FPC y las series

# Datos funcionales
plot(embifts1, col = gray(0.8), xlab = "Dia", ylab = "Puntuaciones EMBI")
# Prediccion de la funcion
plot(pred1,  lwd = 3, col = 'blue',  add = TRUE)
legend("topleft", "2021", col = "blue",  cex = 0.7,  lwd = 1)

min(pred1$mean$y)
max(pred1$mean$y)

# Comparacion con datos orginales 2021
datos2021 <- embi2021[1:217,2] # 217: 05 de agosto
data.frame('Dias' = 1:217, 'EMBI_2021' = datos2021) %>% 
  ggplot(aes(Dias, EMBI_2021)) + 
  geom_line(col = 'blue')

data.frame(EMBI_2021 = datos2021, Prediccion_EMBI_2021 = pred1$mean$y[1:217]) %>% 
  ggplot(aes(EMBI_2021, Prediccion_EMBI_2021)) + 
  geom_point()

### Precision
mejor <- data.frame(embi = datos2021,  pred =  pred1$mean$y[1:217])
# tomo los valores para los cuales se observan
# mejores predicciones
mejorVal <- mejor %>% dplyr::filter(embi < 1000)

View(mejorVal)
mejorVal %>% ggplot(aes(embi, pred)) + geom_point()

obs <- nrow(mejorVal)
MAPE1 <- (100/obs)*sum(abs((mejorVal[,1])- mejorVal[,2])/abs(mejorVal[,1]))

###### Algoritmo para Bandas de confianza mediante bootstrap suavizado

# atp: vector de las ubicaciones de los datos sin atipicos
# X: la matriz de las funciones estimadas (21x365), sin retirar atipicos
# outD: lo calculado por el algoritmo de outliers segun la medida de profundidad usada. 
# orderD: tipo de orden para las profundidades de los datos
  # si FALSE: orden ascendente; si TRUE: orden descendente
# nb: numero de muestras bootstrap
# smo: parametro de bootstrap suavizado
# alp: para el suavizado exponencial

N <- 200
IC.bootstrap <- function(atp = c(2:8,11:20), X, outD, orderD=F, 
                         nb = N, smo = 0.02, alp = a){
  n <- length(atp)  # longitud para una muestra
  muestras <- list()
  for(j in (1:nb)){
    muestras[[j]] <- sample(atp, size=n, replace=T)
  }  # guardo en una lista cada muestra sin atipicos.
  
  preds <- list()
  basef <- fdata(X, argvals = 1:365)
  varX <- var(basef[atp]$data)*smo 
  
  for(i in (1:nb)){
    prof <- outD$Dep[muestras[[i]]]  # no se consideran los atipicos
    # obtengo las profundidades de los datos de la remuestra
    orden.D <- prof %>% sort(decreasing = orderD) %>% as.matrix() %>% 
      rownames() %>% as.numeric()
    # se ordenan las profundidades y obtengo sus ubicaciones
    
    dataB <- basef[orden.D] # para suavizar las muestras bootstrap
    nr <- nrow(dataB)
    nc <- ncol(dataB)
    if(smo > 0){
      dataB$data <- dataB$data + mvrnorm(n = nr, rep(0, nc), varX)
    }
    
    matriz.boot <- t(dataB$data)
    # obtengo la matriz de la muestra, ordenada segun las profundidades
    colnames(matriz.boot) <- 1:n 
    
    # Reformulo el modelo
    embifts.boot <- fts(1:365, matriz.boot)
    
    modelo.boot <- ftsm(embifts.boot, order=3, method = 'classical')
    
    pred.boot <- forecast(modelo.boot, h=1, method = 'ets', alpha = alp, model = 'ANN',
                          jumpchoice = 'actual')
    
    preds[[i]] <- pred.boot ## cada elemento de la lista contiene el modelo
  }
  return(preds)
}

boot1 <- IC.bootstrap(X = Xest, outD = outMD, orderD= F) # ascendente -> mejor caso

# Calculo la distancia de la curva pronosticada con lo estimado por las remuestras 
est.muestras1 <- matrix(nrow = N, ncol = 365)
for(i in 1:N){ est.muestras1[i, ] <- boot1[[i]]$mean$y }

estimacion1 <- fdata(t(pred1$mean$y) , argvals = 1:365) # prediccion del modelo original
B1 <- fdata(est.muestras1 , argvals = 1:365) # prediccion con las remuestras
distboot1 <- as.vector(metric.lp(estimacion1, B1))

# Nivel de confianza del 90%
q1 <- quantile(distboot1, 0.90)
nivel.confianza1 <- which(distboot1 <  q1)

# Grafico de la Banda de confianza
plot(pred1, ylim=c(400,1100), lwd = 5, col = 'white')
for(i in nivel.confianza1){
  plot(boot1[[i]], add=T, col = 'grey', lwd = 10)
}
plot(pred1, lwd = 3, col = 'blue', add=T)


##### PEOR CASO

orden.desc <- deps %>% sort(decreasing = T) %>% as.matrix() %>% rownames() %>% as.numeric()
# Descendente: las ultimas tienen menos profundidad,
# por tanto, las que tienen puntuaciones EMBI altas.

matriz2 <- t(Xest)[,orden.desc]
colnames(matriz2) <- 1:17 

embifts2 <- fts(1:365, matriz2, xname = "Mes-Dia", yname = "Embi anual")
modelo2 <- ftsm(embifts2, order=4, method = 'classical')
pred2 <- forecast(modelo2, h=1, method = 'ets', alpha = a, model = 'ANN',
                  jumpchoice = 'actual', level=0)

plot(pred2, 'components', xlab2 = 'Datos Funcionales', ylab2 = 'puntuaciones')

plot(embifts2, col = gray(0.8), xlab = "Dia", ylab = "Puntuaciones EMBI")
# Prediccion de la funcion
plot(pred2, lwd = 3, col = 'blue',  add = TRUE)
legend("topleft", "2021", col = "blue",  cex = 0.6,  lwd = 1)

min(pred2$mean$y)
max(pred2$mean$y)

data.frame(EMBI_2021 = datos2021, Prediccion_EMBI_2021 = pred2$mean$y[1:217]) %>% 
  ggplot(aes(EMBI_2021, Prediccion_EMBI_2021)) + 
  geom_point()

# MSE
mejor <- data.frame(embi = datos2021,  pred =  pred2$mean$y[1:217])
# tomo los valores para los cuales se observan
# mejores predicciones
mejorVal <- mejor %>% dplyr::filter(embi > 1000)
mejorVal %>% ggplot(aes(embi, pred)) + geom_point()

obs <- nrow(mejorVal)
MAPE2 <- (100/obs)*sum(abs((mejorVal[,1])- mejorVal[,2])/abs(mejorVal[,1]))

# Banda de confianza
boot2 <- IC.bootstrap(X = Xest, outD = outMD, orderD= T) # descendente, peor caso

est.muestras2 <- matrix(nrow = N, ncol = 365)
for(i in 1:N){ est.muestras2[i, ] <- boot2[[i]]$mean$y }

estimacion2 <- fdata(t(pred2$mean$y) , argvals = 1:365) 
B2 <- fdata(est.muestras2 , argvals = 1:365) 
distboot2 <- as.vector(metric.lp(estimacion2, B2))
q2 <- quantile(distboot2, 0.90)
nivel.confianza2 <- which(distboot2 <  q2)

plot(pred2, ylim=c(800,2100), lwd = 5, col = 'white')
for(i in nivel.confianza2){
  plot(boot2[[i]], add=T, col = 'grey', lwd = 10)
}
plot(pred2, lwd = 3, col = 'blue', add=T)
