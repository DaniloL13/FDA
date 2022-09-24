#######################################################################################################################################
# Validación Cruzada Generalizada
######################################################################################################################################
# Bases para la Temperatura
#--------------------------------------
reg = fregre.basis.cv(fdTemp,pt,type.CV=GCV.S)
reg$gcv.opt
reg$gcv
reg$fregre.basis
#############################
nb<-floor(seq(5,30,len=5))
#############################
out <- optim.basis(fdTemp,nb,type.basis="fourier")
matplot(t(out$gcv),type="l",main="GCV with fourier basis")

out1<-optim.basis(fdTemp,type.CV = CV.S,lambda=l,numbasis=nb)
matplot(t(out1$gcv),type="l",main="GCV with fourier basis")

summary(out1$gcv)
#############################
bb<-seq(5,65,by=5)
#############################
bsp.cv=fregre.basis.cv(fdataobj = fdTemp,pt,basis.x=65,basis.b=bb,type.basis="bspline")
fou.cv=fregre.basis.cv(fdataobj = fdTemp,pt,basis.x=65,basis.b=bb,type.basis="fourier")
plot(bb,bsp.cv$gcv,type="l",lwd=2,col=2,xlab="number of basis",ylab="GCV")
lines(bb,fou.cv$gcv,type="l",col=3,lwd=2)

bsp.cv$basis.b.opt
fou.cv$basis.b.opt
#--------------------------------------
# Bases para el Viento
#--------------------------------------
regV = fregre.basis.cv(fdViento,pt,type.CV=GCV.S)
regV$gcv.opt
regV$gcv
regV$fregre.basis
#############################
bb<-seq(5,65,by=5)
#############################
bsp.cv=fregre.basis.cv(fdataobj = fdViento,pt,basis.x=65,basis.b=bb,type.basis="bspline")
fou.cv=fregre.basis.cv(fdataobj = fdViento,pt,basis.x=65,basis.b=bb,type.basis="fourier")
plot(bb,bsp.cv$gcv,type="l",lwd=2,col=2,xlab="number of basis",ylab="GCV")
lines(bb,fou.cv$gcv,type="l",col=3,lwd=2)

bsp.cv$basis.x.opt
fou.cv$basis.b.opt
