##################################
##################################
########## COMPULSADOS  ##########
##########  Diputados   ########## 
##################################
##################################


#040005  1440


library(tidyverse)

#path<-"~/Desktop/quickcount/quickcount-2024/primer_simulacro/simba/"

compulsator<-function(time){

  #time<-"1440"
  pps<-c("PAN","PRI","PRD","PT","PVEM","MC","MORENA","IND")
  #path<-"~/Desktop/quickcount/quickcount-2024/primer_simulacro/simba/"
  
  #setwd(path)
  
  #bzn_cmp_dip<-"buzon_diputados"
  #bzn_dip_nieto <-"buzon_diputados_nieto"
  #bzn_dip_rdz <-"buzon_diputados_rdz"
  #remesa_dir<-"diputados"
  #equipo_dir<-"equipo4compartida"
  
  #get_comp<-function(time){
  #time<-'1700'
  id<-sprintf("040002%s",time)
  
  info<-read.csv("informacion/Info_distritos_2024.csv")
  #lnest<-info$LISTA_NOMINAL
  #LNN<-sum(lnest)
  #c1<-scan(paste(dir,"/REMESAS",id,".txt",sep=""),n=1)
  table_remesa<-sprintf("Q://REMESAS%s.txt",as.character(id))
  
  remesa<-read.table(table_remesa,header=TRUE,skip=1,sep="|")
  c0<-nrow(remesa)
  est <- remesa %>%
    left_join(info %>%
                rename(ID_DISTRITO_FEDERAL = ID_DISTRITO),
              by=c('ID_DISTRITO_FEDERAL', 'ID_ESTADO')) %>% 
    group_by(ESTRATO) %>%
    summarise(conteo=n()) %>%
    filter(conteo>0) %>%
    select(ESTRATO) %>%
    unique() %>% pull() %>% length()
  
  #tryCatch(
  #{
    # rdz.p <- read.csv(sprintf("%s/buzon_diputados/compulsado_diputados/rodriguez4025%s.csv",path,time)) 
    # rdz.d <- read.csv(sprintf("%s/buzon_diputados_compulsado/rodriguezdip4025%s.csv",path,time))
    # 
    # cot.p <- read.csv(sprintf("%s/buzon_diputados_compulsado/nieto4025%s.csv",path,time)) 
    # cot.d <- read.csv(sprintf("%s/buzon_diputados_compulsado/nietodip4025%s.csv",path,time))
  
    #time<-"1310"
      
    A<-file.exists(sprintf("K://rodriguez4002%s.csv",time))
    B<-file.exists(sprintf("L://rodriguezdip4002%s.csv",time))
    C<-file.exists(sprintf("W://nieto4002%s.csv",time))
    D<-file.exists(sprintf("X://nietodip4002%s.csv",time))
    
    if(A&B&C&D){
      rdz.p <- read.csv(sprintf("K://rodriguez4002%s.csv",time)) 
      rdz.d <- read.csv(sprintf("L://rodriguezdip4002%s.csv",time))
      
      cot.p <- read.csv(sprintf("W://nieto4002%s.csv",time)) 
      cot.d <- read.csv(sprintf("X://nietodip4002%s.csv",time))
      
      if(file.exists(sprintf("M://equipo24002%s.csv",time))){
        eq.p <- read.csv(sprintf("M://equipo24002%s.csv",time)) #%>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,IND,LMU)
        precomp<-rbind(cot.p , rdz.p, eq.p) %>%
          select(-EQ,-R, -EN)
      }else{
        print(sprintf("Archivos insuficientes para %s", time))
        precomp<-rbind(cot.p, rdz.p) %>%
          select(-EQ,-R, -EN)
      }
      
      
      #precomp<-rbind(cot.p , rdz.p, eq.p) %>% select(-EQ,-R, -EN)
      
      comp <- rbind(
        precomp %>% group_by(LMU) %>% summarise_at(c(pps,'PART'), funs(min)) %>% filter(LMU==0),
        precomp %>% group_by(LMU) %>% summarise_at(c(pps,'PART'), funs(mean)) %>% filter(LMU==1),
        precomp %>% group_by(LMU) %>% summarise_at(c(pps,'PART'), funs(max)) %>% filter(LMU==2)
      ) %>% mutate(
        EQ='compulsado',
        EN="40",
        R = substr(id,5,10))
      
      NMUESTRAL<-4620
      
      comp$ESTRATOS=c(300,'','') ## Total de estratos utilizados para la estimación del compulsado
      comp$EST_REC=c(est,'','') ## Estratos recibidos para la estimación
      comp$TOT_CAS=c(NMUESTRAL,'','') ## Total de casillas de la muestra para la entidad
      comp$CAS_REC=c(c0,'','') ## Total de casillas de la muestra recibidas para la entidad
      comp$PORCENTAJE=c(round(c0/NMUESTRAL*100,2),'','') ## Total de casillas recibidas respecto del Total de la muestra (decimal 2 digitos)
      
      comp %>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,IND,
                      PART,LMU,ESTRATOS,EST_REC,TOT_CAS,CAS_REC,PORCENTAJE) %>%
        write.csv(sprintf('Z://compulsado40%s.csv', substr(id,5,10)), row.names = F)
      
      
      print("Escribimos compulsado proporciones diputados")
      ### COMPULSADO DIPUTACIONES
      
      
      if(file.exists(sprintf("N://equipo2dip4002%s.csv",time))){
        
        eq.d <- read.csv(sprintf("N://equipo2dip4002%s.csv",time))
        
        precomp<-rbind(cot.d, rdz.d, eq.d) %>%
          select(-EQ,-R, -EN)
      }else{
        print(sprintf("Archivos insuficientes para %s", time))
        precomp<-rbind(cot.d, rdz.d) %>%
          select(-EQ,-R, -EN)
      }
      
      #rdz.d <- read.csv(sprintf('z:\\buzon4/rodriguezdip%s.csv',substr(id,3,10)))
      #precomp<-rbind(cot.d, rdz.d, eq.d) %>% select(-EQ,-R, -EN)
      
      comp <- rbind(
        precomp %>% group_by(LMU) %>% summarise_at(pps, funs(min)) %>% filter(LMU==0),
        precomp %>% group_by(LMU) %>% summarise_at(pps, funs(round(mean(.)))) %>% filter(LMU==1),
        precomp %>% group_by(LMU) %>% summarise_at(pps, funs(max)) %>% filter(LMU==2)
      ) %>% mutate(
        EQ='compulsado',
        EN="40",
        R = substr(id,5,10))
      
      comp %>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,IND,LMU) %>%
        write.csv(sprintf('Y://compulsadodip40%s.csv', substr(id,5,10)), row.names = F)
      
      print("Escribimos compulsado diputados")
    }else{
      print(sprintf("Falta estimacion para %s", time))
      
    }
    # rdz.p <- read.csv(sprintf("K://rodriguez4002%s.csv",time)) 
    # rdz.d <- read.csv(sprintf("L://rodriguezdip4002%s.csv",time))
    # 
    # cot.p <- read.csv(sprintf("W://nieto4002%s.csv",time)) 
    # cot.d <- read.csv(sprintf("X://nietodip4002%s.csv",time))
    
    # rdz.p <- read.csv(sprintf("%s/buzon_diputados/rodriguez/rodriguez4025%s.csv",path,time)) 
    # rdz.d <- read.csv(sprintf("%s/buzon_diputados_escano/rodriguezdip/rodriguezdip4025%s.csv",path,time))
    # 
    # cot.p <- read.csv(sprintf("%s/buzon_diputados/nieto/nieto4025%s.csv",path,time)) 
    # cot.d <- read.csv(sprintf("%s/buzon_diputados/nietodip/nietodip4025%s.csv",path,time))
  
    #eq.p <- read.csv(sprintf("M://equipo24002%s.csv",time))
    #eq.d <- read.csv(sprintf("N://equipo2dip4002%s.csv",time))
  
  
  #},
  #error = function(cond) {
  #            message(paste("file does not seem to exist:", time))},
  #finally = message(paste("file does not seem to exist:", time))
  #)
  
  }

dir("W://") %>% substr(10,13) %>% map(compulsator)
#times<-dir(sprintf("%s/diputados/", path)) %>% substr(14,17) %>% sort(decreasing=TRUE) 


while(TRUE){
  
t1s<-dir("W://") %>% substr(10,13) 
t2s<-dir("Y://") %>% substr(17,20) 

# for (t in times){
#    compulsator(t)
# }
setdiff(t1s,t2s) %>% map(compulsator)
t1s %>% sort(decreasing=TRUE) %>% map(compulsator)
Sys.sleep(10)
}


# times<-c('1406','1415','1420','1710','1715','1420','1425', )
# compulsator(times[1])
