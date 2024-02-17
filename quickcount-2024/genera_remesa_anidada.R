
## Febrero 2024 ##
## Este script genera cierto numero de muestras -- remesas a partir de un tamano de muestra ademas se basa en un listado determinado para el numero de casillas para cada distrito dentro de cada estado#

suppressPackageStartupMessages(library(tidyverse))
library(survival)

setwd('~/Desktop/quickcount/quickcount-2024/Senadores/')

casillas<-read.csv2('SenaduriaMR2018Casilla_conDto2023.txt', sep = "|") %>% filter(ID_DISTRITO!=0)


rel_muestra<-read.csv('Tam_muestra.csv') %>% rename(n_casillas = n_dto) %>% select(ID_ESTADO, n_casillas) 
	
##### MARCO MUESTRAL ##### 

#n_casillas<-dim(casillas)[1]

casi_df <- casillas %>% 
	mutate(ID_ESTRATO=strata(ID_ESTADO,ID_DISTRITO)) %>% 
	left_join(rel_muestra, by="ID_ESTADO") 

#casi_df 
## EL MUESTREO PARA SENADORES SERÁ POR ESTADO AUNQUE OPERATIVAMENTE VIENE POR DISTRITO  
#summary(casi_df)

## 32 DISTRITOS
## Se obtiene el tamano de muestra 
args<-commandArgs(trailingOnly = TRUE)

#args<-c("1927","20")

## En este caso el numero de la muestra queda fijo
n_muestra <- "ANIDADA" # as.numeric(args[1])
#n_muestra <- 9000
#ne <- 32 ## numero de estratos
#nc <- n_muestra/ne

### id de la muestra...
file_arg <- as.numeric(args[2]) 
#file_arg <- 1

#print(file_arg)

get_casillas<-function(id_estrato){
  ## nc numero de casillas a muestrear
  ## id_estrato
  strata_df <- casi_df %>% filter(ID_ESTRATO == id_estrato)
  nc <- strata_df %>% select(n_casillas) %>% unique() %>% pull()
  #print(sprintf('muestra estrato %s',id_estrato))
  tm_e<-dim(strata_df)[1]
  #print(sprintf('tamano estrato %s casillas sugeridas %s',tm_e, nc))
  sample_n(strata_df, nc)
}

get_muestra<-function(file_id){

muestra<-casi_df %>% 
          select(ID_ESTRATO) %>%
          unique() %>%
          pull() %>%
          map(get_casillas) %>%
          reduce(rbind)

filename<-sprintf('~/Desktop/quickcount/quickcount-2024/Senadores/remesas_anidadas/remesa_%s_%s.txt', n_muestra, file_id)
print(sprintf("Grabando %s", filename))
write.table(x = muestra, file = filename, sep="|")
}

get_muestra(file_arg)

#####

