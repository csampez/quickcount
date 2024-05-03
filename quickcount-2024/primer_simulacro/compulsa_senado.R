##################################
##################################
########## COMPULSADOS  ##########
##########  SENADORES   ########## 
##################################
##################################
times<-c('1406','1415','1420','1425')

compulsator<-function(time){
  
  pps<-c("PAN","PRI","PRD","PT","PVEM","MC","MORENA")
  
  path<-"~/Desktop/quickcount/quickcount-2024/primer_simulacro/"
  
  setwd(path)
  
  bzn_cmp_sen<-"buzon_senadores_compulsado"
  equipo_dir<-"equipo4compartida"
  
  id<-sprintf("050025%s",time)
  
  info<-read.csv("../informacion_2024/Info_estados_2024.csv")
  table_remesa<-paste("simba/senadores","/REMESAS",as.character(id),".txt",sep="")
  
  remesa<-read.table(table_remesa,header=TRUE,skip=1,sep="|")
  c0<-nrow(remesa)
  est <- remesa %>%
    left_join(info,
              by=c('ID_ESTADO')) %>% 
    group_by(ID_ESTADO) %>%
    summarise(conteo=n()) %>%
    filter(conteo>0) %>%
    select(ID_ESTADO) %>%
    unique() %>% pull() %>% length()
  
  rdz.p <- read.csv(paste("simba/buzon_senadores_compulsado/rodriguez5025",time,".csv",sep = "")) 
  rdz.d <- read.csv(paste("./simba/buzon_senadores_compulsado/rodriguezsen5025",time,".csv",sep = ""))
  
  cot.p <- read.csv(paste("simba/buzon_senadores_compulsado/nieto5025",time,".csv",sep = "")) 
  cot.d <- read.csv(paste("./simba/buzon_senadores_compulsado/nietosen5025",time,".csv",sep = ""))
  #eq.dip <- read.csv(paste("./simba/buzon_senadores_compulsado/nietosen5025",times[3],".csv",sep = ""))
  
  precomp<-rbind(cot.p , rdz.p) %>%
    select(-EQ,-R, -EN)
  
  comp <- rbind(
    precomp %>% group_by(LMU) %>% summarise_at(c(pps,'PART'), funs(min)) %>% filter(LMU==0),
    precomp %>% group_by(LMU) %>% summarise_at(c(pps,'PART'), funs(mean)) %>% filter(LMU==1),
    precomp %>% group_by(LMU) %>% summarise_at(c(pps,'PART'), funs(max)) %>% filter(LMU==2)
  ) %>% mutate(
    EQ='compulsado',
    EN="50",
    R = substr(id,5,10))
  
  NMUESTRAL<-1620
  
  comp$ESTRATOS=c(32,'','') ## Total de estratos utilizados para la estimación del compulsado
  comp$EST_REC=c(est,'','') ## Estratos recibidos para la estimación
  comp$TOT_CAS=c(NMUESTRAL,'','') ## Total de casillas de la muestra para la entidad
  comp$CAS_REC=c(c0,'','') ## Total de casillas de la muestra recibidas para la entidad
  comp$PORCENTAJE=c(round(c0/NMUESTRAL*100,2),'','') ## Total de casillas recibidas respecto del Total de la muestra (decimal 2 digitos)
  
  comp %>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,
                  PART,LMU,ESTRATOS,EST_REC,TOT_CAS,CAS_REC,PORCENTAJE) %>%
    write.csv(sprintf('./simba/buzon_senadores_compulsado/compulsado50%s.csv', substr(id,5,10)), row.names = F)
  
  ### COMPULSADO SENADURIAS
  
  precomp<-rbind(cot.d, rdz.d) %>% select(-EQ,-R, -EN)
  
  comp <- rbind(
    precomp %>% group_by(LMU) %>% summarise_at(pps, funs(min)) %>% filter(LMU==0),
    precomp %>% group_by(LMU) %>% summarise_at(pps, funs(round(mean(.)))) %>% filter(LMU==1),
    precomp %>% group_by(LMU) %>% summarise_at(pps, funs(max)) %>% filter(LMU==2)
  ) %>% mutate(
    EQ='compulsado',
    EN="50",
    R = substr(id,5,10))
  
  comp %>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,LMU) %>%
    write.csv(sprintf('./simba/buzon_senadores_compulsado/compulsadosen50%s.csv', substr(id,5,10)), row.names = F)
  
}

times %>% map(compulsator)


compulsator(times[4])
