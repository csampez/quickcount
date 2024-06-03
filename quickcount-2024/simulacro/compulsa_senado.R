##################################
##################################
########## COMPULSADOS  ##########
##########  SENADORES   ########## 
##################################
##################################

library(tidyverse)

compulsator<-function(time){
  
  pps<-c("PAN","PRI","PRD","PT","PVEM","MC","MORENA")
  #path<-"~/Desktop/quickcount/quickcount-2024/primer_simulacro/simba/"
  #setwd("~/Desktop/quickcount/quickcount-2024/primer_simulacro/")
  
  #bzn_cmp_sen<-"buzon_senadores/compulsado_senadores"
  #equipo_dir<-"equipo4compartida"
  
  id<-sprintf("050002%s",time)
  
  info<-read.csv("informacion/Info_estados_2024.csv")
  table_remesa<-sprintf("R://REMESAS%s.txt",as.character(id))
  
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
  
  
  # rdz.p <- read.csv(sprintf("%s/buzon_senadores_compulsado/rodriguez5025%s.csv",path,time)) 
  # rdz.d <- read.csv(sprintf("%s/buzon_senadores_compulsado/rodriguezsen5025%s.csv",path,time))
  # 
  # cot.p <- read.csv(sprintf("%s/buzon_senadores_compulsado/nieto5025%s.csv",path,time)) 
  # cot.d <- read.csv(sprintf("%s/buzon_senadores_compulsado/nietosen5025%s.csv",path,time))

  
  ## NUEVA FORMA___???!!!!!!!#################
  
  #tryCatch( {
  
  A<-file.exists(sprintf("J://rodriguez5002%s.csv",time))
  B<-file.exists(sprintf("I://rodriguezsen5002%s.csv",time))
  C<-file.exists(sprintf("T://nieto5002%s.csv",time))
  D<-file.exists(sprintf("O://nietosen5002%s.csv",time))
  
  if(A&B&C&D){
    
    
  rdz.p <- read.csv(sprintf("J://rodriguez5002%s.csv",time)) 
  rdz.d <- read.csv(sprintf("I://rodriguezsen5002%s.csv",time))
  
  cot.p <- read.csv(sprintf("T://nieto5002%s.csv",time)) 
  cot.d <- read.csv(sprintf("O://nietosen5002%s.csv",time))
  
  #eq.p <- read.csv(sprintf("H://equipo15002%s.csv",time))
  #eq.d <- read.csv(sprintf("G://equipo1sen5002%s.csv",time))
  
  #}, error = function(cond) {
  #  message(paste("file does not seem to exist:", time))}
  #)
  
  if(file.exists(sprintf("H://equipo15002%s.csv",time))){
    eq.p <- read.csv(sprintf("H://equipo15002%s.csv",time))
    precomp<-rbind(cot.p , rdz.p, eq.p) %>%
      select(-EQ,-R, -EN)
  }else{
    print(sprintf("Archivos insuficientes para %s", time))
    precomp<-rbind(cot.p , rdz.p) %>%
      select(-EQ,-R, -EN)
  }
    
  
  #precomp<-rbind(cot.p , rdz.p) %>%
  #  select(-EQ,-R, -EN)
  
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
    write.csv(sprintf('V://compulsado50%s.csv', substr(id,5,10)), row.names = F)
  
  ### COMPULSADO SENADURIAS
  
  if(file.exists(sprintf("P://equipo1sen5002%s.csv",time))){
    eq.d <- read.csv(sprintf("P://equipo1sen5002%s.csv",time))
    precomp<-rbind(cot.d , rdz.d, eq.d) %>%
      select(-EQ,-R, -EN)
  }else{
    print(sprintf("Archivos insuficientes para %s", time))
    precomp<-rbind(cot.d, rdz.d) %>%
      select(-EQ,-R, -EN)
  }
  
  #precomp<-rbind(cot.d, rdz.d) %>% select(-EQ,-R, -EN)
  
  comp <- rbind(
    precomp %>% group_by(LMU) %>% summarise_at(pps, funs(min)) %>% filter(LMU==0),
    precomp %>% group_by(LMU) %>% summarise_at(pps, funs(round(mean(.)))) %>% filter(LMU==1),
    precomp %>% group_by(LMU) %>% summarise_at(pps, funs(max)) %>% filter(LMU==2)
  ) %>% mutate(
    EQ='compulsado',
    EN="50",
    R = substr(id,5,10))
  
  comp %>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,LMU) %>%
    write.csv(sprintf('U://compulsadosen50%s.csv', substr(id,5,10)), row.names = F)
  print("Se escriben compulsados ")
  }else{
    print(sprintf("Falta estimacion para %s", time))
  }
  
}

#times<-c('1406','1415','1420','1425')

#times %>% map(compulsator)

while(TRUE){
  t1s<-dir("T://") %>% substr(10,13) 
  t2s<-dir("U://") %>% substr(18,21) 
  
  #setdiff(t1s,t2s) %>% map(compulsator)
  t1s %>% map(compulsator)
  Sys.sleep(10)
}

#num_cores<-detectCores()-2

#time<-now()
# setdiff(remesas_list[1:2], estimaciones_list) %>%
#    sort(decreasing = TRUE) %>%
#     mclapply(.,function(x){get_estimates_sen(x, remesa_path, output_path, output_path_escano)}, mc.cores=num_cores)
#print(now()-time)

# time<-now()
# setdiff(remesas_list, estimaciones_list) %>%
#   sort(decreasing = TRUE) %>%
#   map(.,function(x){get_estimates_sen(x, remesa_path, output_path, output_path_escano)})
# print(now()-time)


#compulsator("1515")
