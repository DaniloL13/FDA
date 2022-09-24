#######################################################################################################################################
# Modelos FLR con respuesta escalar para la Temperatura
#######################################################################################################################################
#----------------------------------------------------------------------
# Functional Regression with scalar response using basis representation
#----------------------------------------------------------------------
res1=fregre.basis.cv(fdataobj = fdTemp, y = pt)
summary(res1)

resT=fregre.basis(fdataobj = fdTemp, y = pt)
summary(resT)

#----------------------------------------------------------------------
# Functional Regression with scalar response using basis FPC
#----------------------------------------------------------------------
res3=fregre.pc.cv(fdataobj = fdata(fdTemp), y = pt)
summary(res3)

resT_FPC=fregre.pc(fdataobj = fdTemp, y = pt, l=c(1:5))
resT_FPC$Vp
plot(resT_FPC$beta.est)
summary(resT_FPC)


#----------------------------------------------------------------------
# Functional Regression with scalar response using basis FPLS
#----------------------------------------------------------------------
res5=fregre.pls.cv(fdataobj = fdata(fdTemp), y = pt, kmax = 5)
summary(res5)

resT_FPLS=fregre.pls(fdataobj = fdata(fdTemp), y = pt, l=c(1:5))
resT_FPLS$Vp
plot(resT_FPLS$beta.est)
summary(resT_FPLS)
resT_FPLS$H

#######################################################################################################################################
# Modelos FLR con respuesta escalar para el Viento
#######################################################################################################################################
#----------------------------------------------------------------------
# Functional Regression with scalar response using basis representation
#----------------------------------------------------------------------
res2=fregre.basis.cv(fdataobj = fdViento, y = pt)
summary(res2)

res_v=fregre.basis(fdataobj = fdViento, y = pt)
summary(res_v)

#----------------------------------------------------------------------
# Functional Regression with scalar response using basis FPC
#----------------------------------------------------------------------
res4=fregre.pc.cv(fdataobj = fdata(fdViento), y = pt)
summary(res4)


resV_FPC=fregre.pc(fdataobj = fdViento, y = pt, l=c(1:5))
resV_FPC$Vp
resV_FPC$beta.est
d=summary(resV_FPC)


#----------------------------------------------------------------------
# Functional Regression with scalar response using basis FPLS
#----------------------------------------------------------------------
res6=fregre.pls.cv(fdataobj = fdata(fdViento), y = pt, kmax = 5)
summary(res6)

resV_FPLS=fregre.pls(fdataobj = fdata(fdViento), y = pt, l=c(1:5))
resV_FPLS$Vp
plot(resV_FPLS$beta.est)
summary(resV_FPLS)
resV_FPLS$H
#######################################################################################################################################
# Modelos FLR con respuesta escalar con Bases Funcionales para la Temperatura y Viento
#######################################################################################################################################
#----------------------------------------------------------------------
# Functional Regression with scalar response using two basis representation
#----------------------------------------------------------------------
xlist=list("df"=data.frame(pt),
           "fdTemp"=fdata(fdTemp),
           "fdViento"=fdata(fdViento))

funcional <- pt ~ fdTemp + fdViento

res=fregre.lm(funcional, data=xlist, control = list(maxit=1,trace=FALSE))
res$fdataobj
res$beta.l
summary(res)

#########################
res_glm=fregre.glm(funcional,data=xlist,control=list(maxit=1,trace=FALSE))
res_glm 
##############################
