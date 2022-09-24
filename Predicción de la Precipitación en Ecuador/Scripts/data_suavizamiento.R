#######################################################################################################################################
# Librerias
######################################################################################################################################
library(fda.usc)
library(fda)
library(dplyr)
library(plotly)
library(ggplot2)
library(plyr)
library(reshape2)
#######################################################################################################################################
# Data
#####################################################################################################################################
load("Data/DatosRegresion/DatosRegresion")
#####################################################################################################################################
# Variables
######################################################################################################################################
temp=x1
pt=y1
viento=x2
#######################################################################################################################################
# Gráficos para Temperatura
#--------------------------------------

plot(temp[,5], col="red",
     xlab = "Tiempo", ylab = "Temperatura",
     main = "Datos de Temperatura diaria",
     ylim = c(25, 26.3), 
     cex.main=1.2, 
     #lty = 1,lwd=2,
     cex.lab=1)
i=3
for( i in 3:5){
  points(temp[,i], col=topo.colors(i))
}
#--------------------------------------
# Gráficos para Temperatura
#--------------------------------------
plot(viento[,5], col="red",
     xlab = "Tiempo", ylab = "Velocidad del Viento",
     main = "Datos de la Velocidad diaria del Viento ",
     ylim = c(2, 5.3), 
     cex.main=1.2, 
     #lty = 1,lwd=2,
     cex.lab=1)
i=3
for( i in 3:5){
  points(viento[,i], col=topo.colors(i))
}
#####################################################################################################################################
# Método de Suvizamiento; Haciendo un fd object usando a Fourier basis para Temperatura
######################################################################################################################################
nbasisT<-5
nb <- seq(5, 15, by = 5)

nT <-  dim(temp)[1] #Observaciones(filas)
sT <-  dim(temp)[2] #variables(columnas)

argvalsT<-seq(1,nT, by=1) #sequencia
rangeT  <- c(1,nT)
periodT <- nT

basisT <- create.fourier.basis(rangeT,12, periodT)
plot(basisT)
title("65 Bases de Fourier")
par(mfrow=c(1,1))

fdTemp <- Data2fd(temp,argvalsT,basisT)
plot(fdTemp)
fdTemp$coefs
summary(fdTemp)
#######################################################################################################################################
# Método de Suvizamiento; Haciendo un fd object usando a Fourier basis para el Viento
######################################################################################################################################
nT <-  dim(viento)[1] #Observaciones(filas)
sT <-  dim(viento)[2] #variables(columnas)

argvalsT<-seq(1,nT, by=1) #sequencia
rangeT  <- c(1,nT)
periodT <- nT

basisV <- create.fourier.basis(rangeT,11, periodT)
fdViento <- Data2fd(viento,argvalsT,basisV)

plot(fdViento)
#######################################################################################################################################
# Componentes Funcionales para la Temperatura
#######################################################################################################################################
# Componentes FPC
#--------------------------------------
par(mfrow=c(1,1))
pc.svd<-fdata2pc(fdata(fdTemp),ncomp=5)
norm.fdata(pc.svd$rotation)
plot(pc.svd$rotation,main="",lwd=2)
summary(pt,pc.svd,biplot=T,corplot=TRUE)
summary(pt,pc.svd,corplot=TRUE)

cp = pca.fd(fdTemp , nharm = 3)
plot(cp)
cp$scores
c=summary(pt,pc.svd$mean,biplot=FALSE,corplot=TRUE) 
plot(pc.svd$rotation,main="",lwd=2)
#--------------------------------------
#Componentes FPLS 
#--------------------------------------
pls1<-fdata2pls(fdataobj = fdata(fdTemp),pt,ncomp=5)
norm.fdata(pls1$rotation[1,])
summary(pt,pls1)
summary(pls1,biplot=FALSE)
plot(pls1$rotation,main="",lwd=2)


