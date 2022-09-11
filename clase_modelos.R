library(fda.usc)
library(fda)
library(dplyr)
library(ggplot2)
library(plyr)
library(reshape2)
library(openxlsx)

#######################################################################################################################################
# Data
#####################################################################################################################################
temp = CanadianWeather$dailyAv[,,"Temperature.C"]
plot(temp[,35], type = "l")

# Gráfico de las tres columnas
matplot(temp, type = "l", lty = 1,
        ylab = "temp",
        ylim = c(min(temp), max(temp))) # Límites del eje Y

##################################
prectmp <- CanadianWeather$dailyAv[,,"Precipitation.mm"]
vientomp <-read.csv(file = "Mat_Resp/Viento.csv") 

for (j in 1:35) {
  index <- prectmp[,j] == 0
  prectmp[index,j] <- 0.05
}

temp=temp[,1:25]

pt=apply(prectmp,2,mean)

pt=log(pt)
plot(pt)

pt=pt[1:25]
viento=as.matrix(vientomp)
View(pt)
#######################################################################################################################################
# Making a fd object by using a Fourier basis para Temperatura
######################################################################################################################################
nbasisT <- 5
nT <-  dim(temp)[1] #Observaciones(filas)
sT <-  dim(temp)[2] #variables(columnas)

argvalsT<-seq(1,nT, by=1) #sequencia
rangeT  <- c(1,nT)
periodT <- nT

basisT <- create.fourier.basis(rangeT, 21, periodT)
plot(basisT)
######################################################################################################################################
fdTemp <- Data2fd(temp,argvalsT,basisT)
plot(fdTemp)
plot(temp)

#########3
nb<-floor(seq(5,30,len=5))
a=optim.basis(fdataobj = fdTemp,type.CV = GCV.S,numbasis = nb)
######
summary(fdTemp)

regresion=fregre.basis.cv(fdTemp,pt,type.CV=GCV.S,basis.b = 21,type.basis = "fourier")
regresion$gcv.opt
regresion$gcv
regresion$fregre.basis




######################################
fdViento <- Data2fd(viento,argvalsT,basisT)
plot(fdViento)


#Medias 
P.mean=mean.fd(fdTemp)
plot(P.mean, ylab="Temperatura", xlab="día")
title("Promedio Temperatura Funcional")
lines(P.mean, lty=1, lwd=3, col="red")

#Varianza
P.std=std.fd(fdTemp)
plot(P.std^2, ylab="Viento", xlab="día")
title("Varianza Temperatura Funcional")
lines(P.std, lty=1, lwd=3, col="red")

#Covarianza
plot(var.fd(fdTemp))
plot(cca.fd(fdTemp))

library(fda)
#Componentes PC
par(mfrow=c(1,1))
pc<-fdata2pc(fdata(fdTemp),ncomp=3)
pc$rotation

#Componentes PLS
pls1<-fdata2pls(fdata(fdTemp),pt,ncomp=3)
pls1$rotation
summary(pls1$x)
plot(pls1$rotation,main="",lwd=2)
#######################################################################################################################################
# Functional Regression with response scalar
######################################################################################################################################
#FD=list("TF"=fdTemp,"RSF"=fdRS,"p"=p)
#FD=list("TF"=fdTemp,"R"=pt)
#names(FD)

#TF=t(FD$TF$coefs)
#dim(TF)

#RSF=FD$RSF$coefs
#dim(RSF)
#y1=FD$R
#r=data.frame(y1)
#dim(r)
#######################################################################################################################################
#Functional Regression with scalar response using basis representation
######################################################################################################################################
res1 = fregre.basis(fdataobj = fdTemp,y=pt,basis.b = 'Fourier')
res1$gvc

plot(res1$residuals, type="line")
lines(res1$fitted.values)
lines(res1$coefficients)


a=summary(res1)
a$Influence

plot(inf1$DCE,type="line")

library(corrplot)
library(PerformanceAnalytics)

inf1=influence(res1)
plot(inf1$DCE,col="blue",lwd=3, type="line", main="Medidad de Cook para la Estimación ")

plot(inf1$DCP,type="line",lwd=3,ylim=c(0,40),main="Medidas de Predicción de Cook y Peña")
lines(inf1$DP,col="red",lwd=3)
legend( -2.5, 9.5, cex=0.8, col = c("black", "red"),
        pch =1:3, lty=1:3, title="Tratamiento", horiz = TRUE)


#res2=fregre.basis.cv(fdataobj = fdTemp,pt)
#b=summary(res2)

######################################################################################################################################
res3=fregre.pc(fdataobj = fdTemp,pt)
res3$beta.est
c=summary(res3)

inf3=influence(res3)
plot(inf3$betas,lwd=3)

plot(inf3$DCP,type="line",lwd=3,ylim = c(0,500))
lines(inf3$DCE,col="blue",lwd=3)
lines(inf3$DP,col="red",lwd=3)

plot(inf3$DCE,type="line")



#res4=fregre.pc.cv(fdataobj = fdata(fdTemp),pt)
#d=summary(res4)
######################################################################################################################################
res5=fregre.pls(fdataobj = fdata(fdTemp),pt)
class(res1$beta.est)

View(pt)
e=summary(res5)
e$Influence
class(res5$beta.est)

inf5=influence(fd(res5$beta.est))


res6=fregre.pls.cv(fdata(fdTemp),pt)
f=summary(res6)
######################################################################################################################################
xlist=list("df"=data.frame(pt),
           "fdTemp"=fdata(fdTemp),"fdViento"=fdata(fdViento))

f<-pt ~ fdTemp + fdViento

res=fregre.lm(f,data=xlist,control=list(maxit=1,trace=FALSE))
res 
summary(res)
