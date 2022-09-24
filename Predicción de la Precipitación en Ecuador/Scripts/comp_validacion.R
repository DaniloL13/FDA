#######################################################################################################################################
# Medidas de Influencia
#######################################################################################################################################
library(corrplot)
library(PerformanceAnalytics)
library(MASS)
library(Rcmdr)
library(lmtest)
#----------------------------------------------------------------------
# Temperatura
#----------------------------------------------------------------------

a=influence(res_t)
plot(a$DCP)
plot(a$DCE,type="line")
plot(a$DP,type="line")
#----------------------------------------------------------------------
# Viento
#----------------------------------------------------------------------
inf1=influence(resV)
plot(inf1$DCE,col="blue",lwd=2, type="line", main="Medidad de Cook para la Estimación ")
lines(inf1$DCP,type="line",lwd=2,ylim=c(0,40),main="Medidas de Predicción de Cook y Peña")
lines(inf1$DP,col="red",lwd=2)


m1=data.frame(inf1$DCP,inf1$DCE,inf1$DP)
correlacion1<-round(cor(m1), 1)
pairs(correlacion1)

corrplot(correlacion1, method="number", type="upper")
chart.Correlation(correlacion1, histogram = F, pch = 19)

scatterplot3d::scatterplot3d(inf1$DCP,inf1$DCE,inf1$DP)

#----------------------------------------------------------------------
inf3=influence(resV_FPC)
plot(inf3$betas,lwd=3)

plot(inf3$DCP,type="line",lwd=3,ylim = c(0,4))
lines(inf3$DP,col="red",lwd=3)
plot(inf3$DCE,type="line")
###########
m3=data.frame(inf3$DCP,inf3$DCE,inf3$DP)
correlacion3<-round(cor(m3), 1)

pairs(correlacion3)
corrplot(correlacion3, method="number", type="upper")
chart.Correlation(correlacion3, histogram = T, pch = 19)
#######################################################################################################################################
# Medidas de Compración
#######################################################################################################################################
#----------------------------------------------------------------------
#AKAIKE
#----------------------------------------------------------------------
AIC=ncol(fdTemp$coefs)*(log(2*pi)+1+log((sum(res_t$residuals^2)/ncol(fdTemp$coefs))))+((length(res_t$coefficients)+1)*2)
AIC
#----------------------------------------------------------------------
#AKAIKE CORREGIDO
#----------------------------------------------------------------------
AICC=AIC+((2*(length(res_t$coefficients)+1))*((length(res_t$coefficients)+1)+1))/(ncol(fdTemp$coefs)-(length(res_t$coefficients)+1)-1)
AICC
#----------------------------------------------------------------------
#BAYES
#----------------------------------------------------------------------
BIC=ncol(fdTemp$coefs)*(log(2*pi)+1+log((sum(res_t$residuals^2)/ncol(fdTemp$coefs))))+((length(res_t$coefficients)+1)*log(ncol(fdTemp$coefs)))
BIC
#######################################################################################################################################
# Validación del Modelo
#######################################################################################################################################
#----------------------------------------------------------------------
# Estabilidad de los parametros
#----------------------------------------------------------------------
resettest(res)
#----------------------------------------------------------------------
# Normalidad
#----------------------------------------------------------------------
jarque.test(as.vector(res$residuals))
ad.test(res$residuals)
shapiro.test(res$residuals)
#----------------------------------------------------------------------
# Autocorrelación
#----------------------------------------------------------------------
a=res$residuals
dwtest ()
bgtest(res,order=1)
#----------------------------------------------------------------------
# Heterocedasticidad
#----------------------------------------------------------------------
qplot(x=res$fitted.values,
      y=(res$residuals))+
  geom_point()

bptest(res)
gqtest(res)
summary(res)
#----------------------------------------------------------------------
# Multicolinealidad
#----------------------------------------------------------------------
cor(fdTemp$coefs[,1],fdViento$coefs[,1],use="complete.obs")
#pairs(res[3:6])
#ggpairs(reg[3:6])
vif(res)

