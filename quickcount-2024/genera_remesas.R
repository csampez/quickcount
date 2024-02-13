
## Febrero 2024 ##
## Este script genera cierto numero de muestras -- remesas a partir de un tamano de muestra ##

library(tidyverse)
library(survival)

setwd('~/Desktop/QuickCount/ConteoRapido2024/Senadores/')

casillas<-read.csv2('SenaduriaMR2018Casilla_conDto2023.txt', sep = "|")

#####

n_casillas<-dim(casillas)[1]

casi_df <- casillas 

## EL MUESTREO PARA SENADORES SERÁ POR ESTADO 
#summary(casi_df)

## 32 DISTRITOS
## Se obtiene el tamano de muestra 
args<-commandArgs(trailingOnly = TRUE)

#args<-c("3200","20")

n_muestra <- as.numeric(args[1])
#n_muestra <- 9000
ne <- 32 ## numero de estratos
nc <- n_muestra/ne

### id de la muestra...
file_arg <- as.numeric(args[2]) 
#file_arg <- 1

#print(file_arg)

## 320(10) 640(20) 1280(40), 1920 (60), 2560 (80), 3200 (100), 3840 (120)
#nc <- 30 ## 9000 casillas
#nc <- 25 ## 7500 casillas
#nc <- 20 ## 6000 casillas


get_casillas<-function(id_estrato){
  ## nc numero de casillas a muestrear
  ## id_estrato
  #print(sprintf('muestra estrato %s',id_estrato))
  sample_n(casi_df %>% filter(ID_ESTADO == id_estrato), nc)
}

get_muestra<-function(file_id){

muestra<-casi_df %>% 
          select(ID_ESTADO) %>%
          unique() %>%
          pull() %>%
          map(get_casillas) %>%
          reduce(rbind)

filename<-sprintf('~/Desktop/QuickCount/ConteoRapido2024/Senadores/remesas/remesa_%s_%s.txt', n_muestra, file_id)
print(sprintf("Grabando %s", filename))
write.table(x = muestra, file = filename, sep="|")
}

get_muestra(file_arg)

#####

