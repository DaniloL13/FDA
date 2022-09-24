#######################################################################################################################################
# Medidas de Centralidad para la Temperatura
######################################################################################################################################
# Media 
#--------------------------------------
P.mean=mean.fd(fdTemp)
plot(P.mean, ylab="Temperatura", xlab="día")
title("Promedio Temperatura Funcional")
lines(P.mean, lty=1, lwd=3, col="red")
#--------------------------------------
par(mfrow=c(1,3))
#--------------------------------------
plot(func.mean(fdata(fdTemp)), main = "M. Centralidad (media recortada 15%)",
     ylim = c(23.1, 24.3), cex.main=1.2, col=1, lty = 1,lwd=2,
     xlab = "Tiempo (días)",ylab = "Temperatura (°C)", cex.lab=1)
lines(func.trim.FM(fdata(fdTemp), trim = 0.15), col = 2,lwd=2)
lines(func.trim.mode(fdata(fdTemp), trim = 0.15), col = 3, lty = 3,lwd=2)
lines(func.trim.RP(fdata(fdTemp), trim = 0.15), col = 4, lty = 4,lwd=2)
lines(func.trim.RT(fdata(fdTemp), trim = 0.15), col = 7, lty = 4,lwd=2)
legend(x =200, y=24.35,
       legend = c("mean","trim.FM","trim.mode","trim.RP","trim.RT"), 
       bty = "n", 
       fill = c(1,2,3,4,7),
       xpd = F,
       cex = 0.8)
#--------------------------------------
# Mediana
#--------------------------------------
plot(func.mean(fdata(fdTemp)), main = "M. Centralidad (mediana)",
     ylim = c(23.1, 26.2),cex.main=1.2, col=1, lty = 1,lwd=2,
     xlab = "Tiempo (días)",ylab = "Temperatura (°C)", cex.lab=1)
lines(func.med.FM(fdata(fdTemp)), col = 2,lwd=2)
lines(func.med.mode(fdata(fdTemp)), col = 3, lty = 3,lwd=2)
lines(func.med.RP(fdata(fdTemp)), col = 4, lty = 4,lwd=2)
lines(func.med.RT(fdata(fdTemp)), col = 7, lty = 4,lwd=2)
legend(x =200, y=26.35,
       legend = c("mean","med.FM","med.mode","med.RP","med.RT"), 
       bty = "n", 
       fill = c(1,2,3,4,7),
       xpd = F,
       cex = 0.8)
#######################################################################################################################################
# Medida de Varariablidad para la Temperatura
######################################################################################################################################
# Varianza
#--------------------------------------
P.std=std.fd(fdTemp)
plot(P.std, ylab="Viento", xlab="día")
title("Varianza Temperatura Funcional")
lines(P.std, lty=1, lwd=3, col="red")
######################## 
plot(func.var(fdata(fdTemp)), main = "M. Dispersión (varianza recortada 15%)", 
     ylim = c(3,6.1),cex.main=1.2, col=1, lty = 1,lwd=2,
     xlab = "Tiempo (días)",ylab = "Temperatura (°C)", cex.lab=1)
lines(func.trimvar.FM(fdata(fdTemp), trim = 0.15), col = 2,lwd=2)
lines(func.trimvar.mode(fdata(fdTemp), trim = 0.15), col = 3, lty = 3,lwd=2)
lines(func.trimvar.RP(fdata(fdTemp), trim = 0.15), col = 4, lty = 4,lwd=2) 
lines(func.trimvar.RT(fdata(fdTemp), trim = 0.15), col = 7, lty = 4,lwd=2)
legend(x =200, y=6.25,
       legend = c("mean","trimvar.FM","trimvar.mode","trimvar.RP","trimvar.RT"), 
       bty = "n", 
       fill = c(1,2,3,4,7),
       xpd = F,
       cex = 0.8)
#--------------------------------------
#Covarianza
#--------------------------------------
plot(var.fd(fdTemp))
plot(cca.fd(fdTemp))
#######################################################################################################################################
# Medidas de Centralidad para el Viento
######################################################################################################################################
# Media 
#--------------------------------------
par(mfrow=c(1,1))
########
plot(func.mean(fdata(fdViento)), main = "M. Centralidad (media recortada 15%)",
     ylim = c(2.2, 3.3), cex.main=1.2, col=1, lty = 1,lwd=2,
     xlab = "Tiempo (días)",ylab = "Viento (m/s)",
     cex.lab=1)
lines(func.trim.FM(fdata(fdViento), trim = 0.15), col = 2,lwd=2)
lines(func.trim.mode(fdata(fdViento), trim = 0.15), col = 3, lty = 3,lwd=2)
lines(func.trim.RP(fdata(fdViento), trim = 0.15), col = 4, lty = 4,lwd=2)
lines(func.trim.RT(fdata(fdViento), trim = 0.15), col = 7, lty = 4,lwd=2)
legend(x =200, y=2.5,
       legend = c("mean","trim.FM","trim.mode","trim.RP","trim.RT"), 
       bty = "n", 
       fill = c(1,2,3,4,7),
       xpd = F,
       cex = 0.8)
#--------------------------------------
# Mediana
#--------------------------------------
plot(func.mean(fdata(fdViento)), main = "M. Centralidad (mediana)",
     ylim = c(2.25, 4),cex.main=1.2, col=1, lty = 1,lwd=2,
     xlab = "Tiempo (días)",ylab = "Viento (m/s)",
     cex.lab=1)
lines(func.med.FM(fdata(fdViento)), col = 2,lwd=2)
lines(func.med.mode(fdata(fdViento)), col = 3, lty = 3,lwd=2)
lines(func.med.RP(fdata(fdViento)), col = 4, lty = 4,lwd=2)
lines(func.med.RT(fdata(fdViento)), col = 7, lty = 4,lwd=2)
legend(x =200, y=2.75,
       legend = c("mean","med.FM","med.mode","med.RP","med.RT"), 
       bty = "n", 
       fill = c(1,2,3,4,7),
       xpd = F,
       cex = 0.8)
#######################################################################################################################################
# Medida de Varariablidad para el Viento
######################################################################################################################################
# Varianza
#--------------------------------------
plot(func.var(fdata(fdViento)), main = "M. Dispersión (varianza recortada 15%)", 
     ylim = c(0.2,1.5),cex.main=1.2, col=1, lty = 1,lwd=2,
     xlab = "Tiempo (días)",ylab = "Viento (m/s)", cex.lab=1)
lines(func.trimvar.FM(fdata(fdViento), trim = 0.15), col = 2,lwd=2)
lines(func.trimvar.mode(fdata(fdViento), trim = 0.15), col = 3, lty = 3,lwd=2)
lines(func.trimvar.RP(fdata(fdViento), trim = 0.15), col = 4, lty = 4,lwd=2) 
lines(func.trimvar.RT(fdata(fdViento), trim = 0.15), col = 7, lty = 4,lwd=2) 
legend(x =200, y=0.55,
       legend = c("mean","trimvar.FM","trimvar.mode","trimvar.RP","trimvar.RT"), 
       bty = "n", 
       fill = c(1,2,3,4,7),
       xpd = F,
       cex = 0.8)
#--------------------------------------
#Covarianza
#--------------------------------------
plot(var.fd(fdViento))
plot(cca.fd(fdViento))