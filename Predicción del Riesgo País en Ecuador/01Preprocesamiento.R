##############################################
#### ANALISIS DE DATOS FUNCIONALES (FDA) #####
##############################################

#############################################
## Lectura y Preprocesamiento de los datos
#############################################

library(readr)      #lectura del archivo
library(lubridate)  #formato de fechas
library(tidyr)      #para spread
library(naniar)     #graficos NA
library(openxlsx)   #para guardar bases finales
library(reshape2)   #para grafico de evolucion
library(ggplot2)
library(tidyverse)

### datos 2000-2018
embi <- as.data.frame(read_csv("01Preprocesamiento/RiesgoPais2000.csv"))
str(embi)

embi[,1] <- as.Date(substring(embi[,1], 1, 10)) # para que no aparezca la hora
embi1 <- as.data.frame(cbind('Anio' = year(embi[,1]), 'Mes' = month(embi[,1]), 
                             'Dia' = day(embi[,1]), 'RiesgoPais' = embi[,2]))

embi2 <- spread(data = embi1, key = Anio, value = RiesgoPais)  ## anios como columnas.

embi2 <- unite(embi2, 'MesDia', 1:2,  sep = "-", remove = TRUE)  # en una sola columna el mes y dia

data1 <- embi2

data1 <- data1[,-20]  # data del 2000 al 2017, incluida fecha 2-29

#### datos 2004-2021
embi <- as.data.frame(read_csv("01Preprocesamiento/RiesgoPais2004.csv"))

embi[,1] <- as.Date(substring(embi[,1], 1, 10))
embi <- embi[-c(1:4904),]  # Considero desde 2018
embi1 <- as.data.frame(cbind('Anio' = year(embi[,1]), 'Mes' = month(embi[,1]), 
                             'Dia' = day(embi[,1]), 'RiesgoPais' = embi[,2]))

embi2 <- spread(data = embi1, key = Anio, value = RiesgoPais)  ## anios como columnas.

embi2 <- unite(embi2, 'MesDia', 1:2,  sep = "-", remove = TRUE)

data2 <- embi2

library(dplyr)
base <- left_join(data1, data2, by = 'MesDia')
detach(package:dplyr)

embi2021 <- base[,c('MesDia','2021')] # guardo datos 2021
embi <- base[,-23] # retiro 2021
row.names(embi) <- embi[,'MesDia']
embi <- embi[,-1] # retiro columna MesDia

vis_miss(embi) # grafico valores perdidos

### Ubicacion de los valores perdidos e imputacion
NA2018 <- which(is.na(embi[,'2018'])==TRUE) 
rownames(embi)[NA2018] 
NA2018 <- NA2018[-1]  ### quito 2/29
embi[NA2018,'2018'] <- embi[NA2018[1]-1,'2018']

NA2019 <- which(is.na(embi[,'2019'])==TRUE)
rownames(embi)[NA2019]
NA2019 <- NA2019[-1]  ### quito 2/29
for(i in seq(1, length(NA2019), by = 2)){
  embi[NA2019[i:(i+1)],'2019'] <- embi[NA2019[i]-1,'2019']
}

NA2020 <- which(is.na(embi[,'2020'])==TRUE)
rownames(embi)[NA2020]
for(i in seq(1, length(NA2020), by = 2)){
  embi[NA2020[i:(i+1)],'2020'] <- embi[NA2020[i]-1,'2020']
}

## Retiro 29 de febrero
embi <- embi[-60,]

### Guardo la base de datos final y del 2021
write.csv(embi, '02Datos/EMBI.csv')
embi2021 <- embi2021[-60,]
write.csv(embi2021, '02Datos/EMBI2021.csv')

ind <- embi
ind$MesDia <- 1:365
ind <- melt(ind, id.vars = 'MesDia') 

##### Grafico evolucion por anio
ind %>% ggplot(aes(x = MesDia, y = value, col = variable, group = variable, group = 1)) +
  labs(x = "Dia", y = 'Indicador EMBI') +
  theme(axis.text.x = element_blank()) +
  geom_line() +
  facet_wrap(~variable) +
  theme(legend.position="none")

####### Grafico: Evolucion anual
ind  %>% ggplot(aes(x = MesDia, y = value,  col = variable, group = variable)) +
  labs(x = "Mes_Dia", y = 'Riesgo Pais (EMBI)', title = "Evolucion") +
  theme(axis.text.x = element_blank()) +
  geom_line()