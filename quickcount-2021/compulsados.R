##################################
##################################
########## COMPULSADOS ###########
##################################
##################################

buzon<-'z:\\buzon4/'
buzon_nieto<-get_ids(buzon,patron='nietodip',ini=9, fin=16) 
buzon_rodri<-get_ids(buzon,patron='rodriguezdip',ini=13, fin=20) 

get_compulsado<-function(id){
  #time<-'1700'
  pps<-c('PAN','PRI','PRD','PT', 'PVEM','MC','MORENA','PES','RSP','FPM', 'IND')
  print(sprintf("Compulsado para %s", id))
  info<-read.csv("CodigoTemp/codigo_nuevo/aux_data/Info_distritos_2021.txt")
  indir<-'z:\\unicom/cortes/pef/'
  remesa<-read.table(paste(indir,"/REMESAS",id,".txt",sep=""),header=TRUE,skip=1,sep="|")
  c0<-nrow(remesa)
  
  if(c0>0){
  est <- remesa %>%
          left_join(info %>% rename(ID_DISTRITO_FEDERAL = ID_DISTRITO),
                    by=c('ID_DISTRITO_FEDERAL', 'ID_ESTADO')) %>% 
          group_by(ID_ESTRATO) %>%
          summarise(conteo=n()) %>%
          filter(conteo>0) %>% select(ID_ESTRATO) %>% unique() %>% pull() %>% length()
  
  ### busco esa para ambos ... 
  test_nieto<-sprintf('z:\\buzon4/nieto%s.csv', substr(id,3,10))
  test_rodri<-sprintf('z:\\buzon4/rodriguez%s.csv', substr(id,3,10))
  if(file.exists(test_rodri) & file.exists(test_nieto)){
    
    cot.p <- read.csv(test_nieto)
    rdz.p <- read.csv(test_rodri)
    
    precomp<-rbind(cot.p , rdz.p) %>% select(-EQ,-R, -EN)
    
    comp <- rbind(
      precomp %>% group_by(LMU) %>% summarise_at(c(pps,'PART'), funs(min)) %>% filter(LMU==0),
      precomp %>% group_by(LMU) %>% summarise_at(c(pps,'PART'), funs(mean)) %>% filter(LMU==1),
      precomp %>% group_by(LMU) %>% summarise_at(c(pps,'PART'), funs(max)) %>% filter(LMU==2)
    ) %>% mutate(
      EQ='compulsado',
      EN="00",
      R = substr(id,5,10))
    
    comp$ESTRATOS=c(300,'','') ## Total de estratos utilizados para la estimación del compulsado
    comp$EST_REC=c(est,'','') ## Estratos recibidos para la estimación
    comp$TOT_CAS=c(6345,'','') ## Total de casillas de la muestra para la entidad
    comp$CAS_REC=c(c0,'','') ## Total de casillas de la muestra recibidas para la entidad
    comp$PORCENTAJE=c(round(c0/6345*100,2),'','') ## Total de casillas recibidas respecto del Total de la muestra (decimal 2 digitos)
    
    comp %>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,PES,RSP,FPM,IND,
                    PART,LMU,ESTRATOS,EST_REC,TOT_CAS,CAS_REC,PORCENTAJE) %>%
      write.csv(sprintf('z:\\nieto/compulsado_pef/compulsado%s.csv', substr(id,3,10)),
                row.names = F, quote = FALSE)
    
  }else{
    ## si no existe agarro la ultima
    #print("Tomando la ultima disponible")
    print(sprintf("No existen ambas para compulsado %s", id))
  }
  
  #################################
  ### COMPULSADO DIPUTACIONES #####
  
  test_nieto<-sprintf('z:\\buzon4/nietodip%s.csv', substr(id,3,10))
  test_rodri<-sprintf('z:\\buzon4/rodriguezdip%s.csv', substr(id,3,10))
  
  if(file.exists(test_nieto) & file.exists(test_rodri)){
    cot.d <- read.csv(test_nieto)
    rdz.d <- read.csv(test_rodri)
    precomp<-rbind(cot.d, rdz.d) %>% select(-EQ,-R, -EN)
    
    comp <- rbind(
      precomp %>% group_by(LMU) %>% summarise_at(pps, funs(min)) %>% filter(LMU==0),
      precomp %>% group_by(LMU) %>% summarise_at(pps, funs(round(mean(.)))) %>% filter(LMU==1),
      precomp %>% group_by(LMU) %>% summarise_at(pps, funs(max)) %>% filter(LMU==2)
    ) %>% mutate(
      EQ='compulsado',
      EN="00",
      R = substr(id,5,10))
    
    comp %>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,PES,RSP,FPM,IND,LMU) %>%
      write.csv(sprintf('z:\\nieto/compulsado_dip_pef/compulsadodip%s.csv', substr(id,3,10)),
                row.names = F, quote = FALSE)
    
  }else{
    ## si no existen aviso
    print(sprintf("SIN ESTIMACIONES PARA %s", id))
  }}else{ print("SIN DATOS EN REMESA")}
}


