#######################################################################################################################################
# Medidas de Produndidad para la Temperatura
#######################################################################################################################################
par(mfrow=c(1,4))
#--------------------------------------
# FM
#--------------------------------------
out.FM=depth.FM(fdTemp,trim=0.25,draw=F)

plot(out.FM$fdataobj, main = "Profundidad FM",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo (días)", ylab = "Temperatura (°C)",cex.lab=1)
lines(out.FM$median,col="red",lty=1,lwd=2)
lines(out.FM$mtrim, col="green",lty=1,lwd=2)
legend(x =0, y=20,
       legend = c("FM.Median","FM.tr25%"), 
       bty = "n", 
       fill = c("red","green"),
       xpd = F,
       cex = 1)
#--------------------------------------
# Modal
#--------------------------------------
out.mode=depth.mode(fdata(fdTemp),trim=0.25,draw=F)
plot(out.mode$fdataobj, main = "Profundidad Modal",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo (días)", ylab = "Temperatura (°C)",cex.lab=1)
lines(out.mode$median,col="red",lty=1,lwd=2)
lines(out.mode$mtrim, col="green",lty=1,lwd=2)
legend(x =0, y=20,
       legend = c("mode.Median","mode.tr25%"), 
       bty = "n", 
       fill = c("red","green"),
       xpd = F,
       cex = 1)
#--------------------------------------
# RP
#--------------------------------------  
out.RP=depth.RP(fdTemp,trim=0.25,draw=F)
plot(out.RP$fdataobj, main = "Profundidad RP",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo (días)", ylab = "Temperatura (°C)",cex.lab=1)
lines(out.RP$median,col="red",lty=1,lwd=2)
lines(out.RP$mtrim, col="green",lty=1,lwd=2)
legend(x =0, y=20,
       legend = c("RP.Median","RP.tr25%"), 
       bty = "n", 
       fill = c("red","green"),
       xpd = F,
       cex = 1)
#--------------------------------------
# RT
#--------------------------------------  
out.RT=depth.RT(fdTemp,trim=0.25,draw=F)
plot(out.RT$fdataobj, main = "Profundidad RT",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo (días)", ylab = "Temperatura (°C)",cex.lab=1)
lines(out.RT$median,col="red",lty=1,lwd=2)
lines(out.RT$mtrim, col="green",lty=1,lwd=2)
legend(x =0, y=20,
       legend = c("RT.Median","RT.tr25%"), 
       bty = "n", 
       fill = c("red","green"),
       xpd = F,
       cex = 1)
##############################  
out.FSD=depth.FSD(fdata(fdTemp),trim=0.15,draw=TRUE)
out.KFSD=depth.KFSD(fdata(fdTemp),trim=0.15,draw=TRUE)
#######################################################################################################################################
# Medidas de Produndidad para el Viento
#######################################################################################################################################
par(mfrow=c(1,4))
#--------------------------------------
# FM
#--------------------------------------
out.FM2=depth.FM(fdViento,trim=0.25,draw=F)

plot(out.FM2$fdataobj, main = "Profundidad FM",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo", ylab = "Velocidad del Viento",cex.lab=1)
lines(out.FM2$median,col="red",lty=1,lwd=2)
lines(out.FM2$mtrim, col="green",lty=1,lwd=2)
legend(x =0, y=5,
       legend = c("FM.Median","FM.tr25%"), 
       bty = "n", 
       fill = c("red","green"),
       xpd = F,
       cex = 1)
#--------------------------------------
# Modal
#--------------------------------------
out.mode2=depth.mode(fdata(fdViento),trim=0.25,draw=F)

plot(out.mode2$fdataobj, main = "Profundidad Modal",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo", ylab = "Velocidad del Viento",cex.lab=1)
lines(out.mode2$median,col="red",lty=1,lwd=2)
lines(out.mode2$mtrim, col="green",lty=1,lwd=2)
legend(x =0, y=5,
       legend = c("mode.Median","mode.tr25%"), 
       bty = "n", 
       fill = c("red","green"),
       xpd = F,
       cex = 1)
#--------------------------------------
# RP
#--------------------------------------
out.RP2=depth.RP(fdViento,trim=0.25,draw=F)

plot(out.RP2$fdataobj, main = "Profundidad RP",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo", ylab = "Velocidad del Viento",cex.lab=1)
lines(out.RP2$median,col="red",lty=1,lwd=2)
lines(out.RP2$mtrim, col="green",lty=1,lwd=2)
legend(x =0, y=5,
       legend = c("RP.Median","RP.tr25%"), 
       bty = "n", 
       fill = c("red","green"),
       xpd = F,
       cex = 1)

#--------------------------------------
# RT
#--------------------------------------
out.RT2=depth.RT(fdViento,trim=0.25,draw=F)

plot(out.RT2$fdataobj, main = "Profundidad RT",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo", ylab = "Velocidad del Viento",cex.lab=1)
lines(out.RT2$median,col="red",lty=1,lwd=2)
lines(out.RT2$mtrim, col="green",lty=1,lwd=2)
legend(x =0, y=5,
       legend = c("RT.Median","RT.tr25%"), 
       bty = "n", 
       fill = c("red","green"),
       xpd = F,
       cex = 1)
##############################  
out.FSD=depth.FSD(fdata(fdViento),trim=0.15,draw=TRUE)
out.KFSD=depth.KFSD(fdata(fdViento),trim=0.15,draw=TRUE)
######################################