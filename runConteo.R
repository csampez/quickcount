### PAQUETERIAS ###
library(stringr)
library(purrr)
library(tidyverse)

setwd("~/GitHub/quickcount/")

get_ids<-function(ruta, patron, ini, fin){
  ## si hay archivos
  if(length(dir(ruta))>0){
    str_subset(pattern=patron, 
               dir(ruta)) %>%
      map(function(x){substr(x,ini,fin)}) %>%
      reduce(c)
  }else{
    c("")
  }
}

source("./ConteoBayesianoPostCamara2021_Correccion.R")
source("./compulsados.R")
#####RUTAS####

##Muestra
indir<-'z:\\unicom/cortes/pef/'
##Estimaciones
outdir_dip<-'z:\\nieto/diputaciones_pef/'
outdir_pef<-'z:\\nieto/pef/'
##Compulsados
buzon<-'z:\\buzon4/'
outdir_cdip<-'z:\\nieto/compulsado_dip_pef/'
outdir_cpef<-'z:\\nieto/compulsado_pef/'

pause<-100

# Nos movemos al CR
#setwd("~/ConteoRapido")
repeat { 
  
  #Revisamos estimaciones
  remesas<-get_ids(indir,patron='REMESA',ini=8, fin=17) %>% substr(.,3,10)
  pefs<-get_ids(outdir_pef, patron='nieto', ini=6, fin=13)
  #pefs<-c()
  #dips<-get_ids(outdir_dip, patron='nieto', ini=9, fin=16)
  Z<-setdiff(remesas, pefs)
  for (id_rem in Z){
    #if (if_rem[nchar(if_rem)]){
      get_estimates2(paste("04",id_rem,sep=''))
  }#else{}
  
  comps<-get_ids(outdir_cpef, patron='compulsado', ini=11, fin=18) 
  comps<-c()# touts<-dir(outputfolder2)
  Z<-setdiff(remesas, comps)
  for (id_rem in Z){
    get_compulsado(paste("04",id_rem,sep=''))
  }
  Sys.sleep(pause)
}

