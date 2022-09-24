#######################################################################################################################################
# Bootstrap para la Temperatura
#######################################################################################################################################
control=list("col"=c("grey","blue","cyan"),"lty"=c(2,1,1),
             "lwd"=c(1,3,1))

par(mfrow=c(1,2))
########################################
# Bandas de Confianza con media
#--------------------------------------
out.boot1=fdata.bootstrap(fdata(fdTemp),
                          statistic=func.mean,nb=1000,draw=F,
                          draw.control=control,
                          alpha = 0.5)
plot(out.boot1$fdataobj, main = "Bandas de Confianza (media)",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo (días)", ylab = "Temperatura (°C)",cex.lab=1)
lines(out.boot1$resample,col="cyan",lty=1,lwd=1)
lines(out.boot1$statistic, col="blue",lty=1,lwd=2)
legend(x =180, y=21,
       legend = c("original curves","bootstrap curves IN","mean"), 
       bty = "n", 
       fill = c("grey","cyan","blue"),
       xpd = F,
       cex = 0.8)
#--------------------------------------
# Bandas de Confianza con media recortada 
#--------------------------------------
out.boot2=fdata.bootstrap(fdata(fdTemp),
                          statistic=func.trim.FM,nb=1000,draw=F,
                          draw.control=control,
                          alpha = 0.5)
plot(out.boot2$fdataobj, main = "Bandas de Confianza (media recortada 25%)",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo (días)", ylab = "Temperatura (°C)",cex.lab=1)
lines(out.boot2$resample,col="cyan",lty=1,lwd=1)
lines(out.boot2$statistic, col="blue",lty=1,lwd=2)
legend(x =180, y=21,
       legend = c("original curves","bootstrap curves IN","FM trim25%"), 
       bty = "n", 
       fill = c("grey","cyan","blue"),
       xpd = F,
       cex = 0.8)
#--------------------------------------
# Bandas de Confianza 
#--------------------------------------
out.boot3=fdata.bootstrap(fdata(fdTemp),
                          statistic=func.trim.mode,nb=1000,draw=TRUE,
                          draw.control=control,
                          alpha = 0.5)
#######################################################################################################################################
# Bootstrap para el Viento
#######################################################################################################################################
par(mfrow=c(1,2))
########################################
# Bandas de Confianza con media
#--------------------------------------
out.boot4=fdata.bootstrap(fdata(fdViento),
                          statistic=func.mean,nb=1000,draw=F,
                          draw.control=control,
                          alpha = 0.5)

plot(out.boot4$fdataobj, main = "Bandas de Confianza (media)",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo (días)", ylab = "Velocidad del Viento (m/s)",cex.lab=1)
lines(out.boot4$resample,col="cyan",lty=1,lwd=1)
lines(out.boot4$statistic, col="blue",lty=1,lwd=2)
legend(x =1, y=5.5,
       legend = c("original curves","bootstrap curves IN","mean"), 
       bty = "n", 
       fill = c("grey","cyan","blue"),
       xpd = F,
       cex = 0.8)
#--------------------------------------
# Bandas de Confianza con media recortada 
#--------------------------------------
out.boot5=fdata.bootstrap(fdata(fdViento),
                          statistic=func.trim.FM,nb=1000,draw=F,
                          draw.control=control,
                          alpha = 0.5)
plot(out.boot5$fdataobj, main = "Bandas de Confianza (media recortada 25%)",
     col="grey",lty=2,lwd=1,cex.main=1,
     xlab = "Tiempo (días)", ylab = "Velocidad del Viento (m/s)",cex.lab=1)
lines(out.boot5$resample,col="cyan",lty=1,lwd=1)
lines(out.boot5$statistic, col="blue",lty=1,lwd=2)
legend(x =1, y=5.5,
       legend = c("original curves","bootstrap curves IN","FM trim25%"), 
       bty = "n", 
       fill = c("grey","cyan","blue"),
       xpd = F,
       cex = 0.8)
