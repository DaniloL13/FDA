#######################################################################################################################################
# Detección de Outlier 
#######################################################################################################################################
par(mfrow=c(1,2))
#--------------------------------------
# Temperatura
#--------------------------------------

a = outliers.depth.trim(fdataobj = fdTemp, dfunc = depth.FM, nb=100,
                        smo = 0.1, trim = 0.25)  

plot(a$Dep, 
     main = "Atípicos con profundidad FM",cex.main=1,
     col="blue", xlab="Cantones",ylab = "depth.trim FM.tr25%", cex.lab=1)


b = outliers.depth.pond(fdataobj = fdTemp, dfunc = depth.FM, nb=100,
                        smo = 0.1, trim = 0.25)

plot(b$Dep, main = "Atípicos con profundidad FM ponderado",cex.main=1,
     col="red", xlab="Cantones",ylab = "depth.trim FM.tr25%", cex.lab=1)

par(mfrow=c(1,2))
#--------------------------------------
# Viento
#--------------------------------------
a2=outliers.depth.trim(fdataobj = fdViento, dfunc = depth.FM, nb=100,
                       smo = 0.1, trim = 0.25)  
plot(a2$Dep, 
     main = "Atípicos con profundidad FM",cex.main=1,
     col="blue", xlab="Cantones",ylab = "depth.trim FM.tr25%", cex.lab=1)

b2=outliers.depth.pond(fdataobj = fdViento[1:23], dfunc = depth.FM, nb=100,
                       smo = 0.1, trim = 0.25)

plot(b2$Dep, main = "Atípicos con profundidad FM ponderado",cex.main=1,
     col="red", xlab="Cantones",ylab = "depth.trim FM.tr25%", cex.lab=1)


