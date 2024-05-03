######## --- CONTEO RAPIDO BAYESIANO --- ##########
#--- Modelo Normal - Normal Gamma univariado ---#
#--- Estima las proporciones de votos efectivos ---#
#--- Estima el numero de diputados por partido ---#

#-Paquetes requeridos-
library(parallel)
library(tidyverse)
library(truncnorm)
#-Especificacion de la remesa-
#id = identificador de remesa

#path<-"smb://172.19.122.213"
path<-'../primer_simulacro/simba'
remesa_path<-sprintf('%s/diputados',path)
output_path<-sprintf("%s/buzon_diputados_nieto",path)

get_estimates_dip<-function(remesa_name, remesa_path, output_path){
  
  #rdir<-'../primer_simulacro/simba/diputados/'
  #setwd(dir)

#-Especificacion de la remesa y confiabilidad-
#id = identificador de remesa
#id<-"0400251425"

#Nivel de confiabilidad
a<-c(0.99,0.98,0.97,0.96,0.95)
n<-c(0.6,0.7,0.8,0.9,1.0)

#-Especificaciones iniciales-
#np = numero de partidos + 2
#nest = numero de estratos
#ss = tama?o de la muestra posterior
tm<-4620
nest<-300
ss<-5000

partido<-c("PAN","PRI","PRD","PVEM","PT","MC","MORENA","CI")
partidos<-c(partido, "NOVOTO","PART")
np<-length(partidos)

#-Archivos de entrada-
info<-read.csv("informacion/Info_distritos_2024.csv")
lnest<-info$LISTA_NOMINAL
LNN<-sum(lnest)
#c1<-scan(paste(dir,"remesas_2024/REMESAS",id,".txt",sep=""),n=1)

remesa<-read.table(sprintf("%s/REMESAS0400%s.txt", remesa_path, remesa_name), 
                      header=TRUE,
                      skip=1,
                      sep="|") %>% as.data.frame()


#read.table(paste(rdir,"REMESAS",id,".txt",sep=""),header=TRUE,skip=1,sep="|") ##CUIDAR

c1<-nrow(remesa)

#
j1<-which(names(remesa)=="PAN")
j2<-which(names(remesa)=="TOTAL")
for (j in j1:j2) {
  remesa[,j]<-ifelse(is.na(remesa[,j]),0,remesa[,j])
}
c0<-nrow(remesa)
if(c1!=c0) {
  cat("Inconsistencia en el numero de casillas")
  } else {
  cat(paste("Numero de casillas en remesa =",c0),"\n")
    }

coaliciones_aux <- c("PAN_PRI_PRD",
                       "PAN_PRD",
                       "PAN_PRI",
                       "PRI_PRD",
                       "PVEM_PT_MORENA",
                       "PVEM_PT",
                       "PT_MORENA",
                       "PVEM_MORENA")
  
remesa <- remesa %>% mutate_at(c(partidos[!partidos %in% c("NOVOTO", "PART")], coaliciones_aux, 
                                   'CNR', 'NULOS', 'TOTAL'), 
                                 function(x){ifelse(is.na(x),0,x)})
  
if(c1>0){
    #-Creacion de variables-
    #Votos partidos + reparticion de votos en coalici?n
    vx <- remesa %>% as.data.frame() %>%
      mutate(
        PAN = PAN+PAN_PRI/2+PAN_PRD/2+PAN_PRI_PRD/3,
        PRI = PRI+PAN_PRI/2+PRI_PRD/2+PAN_PRI_PRD/3,
        PRD = PRD+PAN_PRD/2+PRI_PRD/2+PAN_PRI_PRD/3,
        PT = PT+PT_MORENA/2+PVEM_PT/2+PVEM_PT_MORENA/3,
        PVEM = PVEM+PVEM_PT/2+PVEM_MORENA/2+PVEM_PT_MORENA/3,
        MORENA = MORENA+PT_MORENA/2+PVEM_MORENA/2+PVEM_PT_MORENA/3,
        MC = MC,
        NOVOTO = NULOS+CNR
      ) %>% select(-ID_ESTRATO)
    
    ##VOTOS
    vx$VOTOS<-apply(vx %>% select_at(c(partido,'NOVOTO')),1,sum)
    vx <- vx  %>%
      #rename(LISTA_NOMINAL = LISTA_NOMINAL_CASILLA) %>%
      mutate(aux = LISTA_NOMINAL-VOTOS, 
             NOVOTO_ADJ = ifelse(aux>0, aux, 0), 
             LN_ADJ = ifelse(aux>0, LISTA_NOMINAL, VOTOS), ## Votacion Efectiva 
             #ID_ESTRATO = strata(as.factor(ID_ESTADO), as.factor(ID_DISTRITO_FEDERAL))
      ) %>% 
      filter(LN_ADJ!=0) %>% #Depuracion de casillas sin voto 
      left_join(info %>% rename(LISTA_NOMINAL_ESTRATO = LISTA_NOMINAL,
                                ID_DISTRITO_FEDERAL = ID_DISTRITO),
                by=c('ID_DISTRITO_FEDERAL', 'ID_ESTADO')) 
    
    ### (CASILLA NOMINAL === 00!!!)
    xy <- vx 
    
    #Tam. de muestra y estadisticas
    ## Relacion de distritos por estrato
    #tt<-table(est)
    ##Estratos presentes en remesa (ie con casillas)
    #aux<-as.numeric(names(tt))
    ## Conteo casillas por estrato
    #nc<-hist(est,breaks=0:nest,plot=FALSE)$counts
    
    ## Arreglos (primer y segundo momento)
    
    ssx <- xy %>%
      group_by(ID_ESTRATO) %>%
      summarise_at(.vars = c(partido,'NOVOTO', 'NOVOTO_ADJ'), .funs = sum)
    
    ssx2 <- xy %>% 
      mutate_at(.vars = c(partido,'NOVOTO', 'NOVOTO_ADJ'), .funs = funs((.)^2/LN_ADJ)) %>%
      group_by(ID_ESTRATO) %>% 
      summarise_at(.vars = c(partido,'NOVOTO', 'NOVOTO_ADJ'), sum)
    
    #Para cada estrato en remesa se suma el listado
    lnx <- vx %>% 
      group_by(ID_ESTRATO) %>% 
      summarise(LN_ADJ = sum(LN_ADJ, na.rm=TRUE), 
                NCASILLAS = n(), 
                LISTA_NOMINAL = sum(LISTA_NOMINAL))
    
    ############################################################
    ############## CAMBIO ######################################
    ############################################################
    
    ###
    #asignarles a los estratos sin muestra 
    #la informacion promedio del grupo al que pertenecen
    
    if(dim(ssx)[1] < 300){
      
      print("Realizando Posestratificacion")
      
      conglomerados<-read.csv("informacion/cluster_catalog_diputados.csv") 

      nc <- c(1,5,10,20,30,50,100,200,300)
      
      # Para cada configuracion (catalogo)
      ## Identificar estatos ausentes en la remesa
      ## Para cada configuracion (de 300 a 1), elegir si  
      
      ###########################################################################
      ###########################################################################
      ###########################################################################
      ###########################################################################
      
      get_post<-function(ng){
        ### Registrar #gpos en remesa
        #ng <- 10
        catx <- conglomerados %>% 
          select(c("ID_ESTRATO", sprintf("cid_ng%s", ng))) %>%
          rename(ID_GRUPO = sprintf("cid_ng%s", ng))
        bool <- catx %>%
          left_join(xy %>% select(-ORIGEN_CAPTURA, 
                             -MODIFICADO, 
                             -ANIO, -MES, -DIA, -HORA, -MINUTOS, -SEGUNDOS, 
                             -TIPO_CASILLA, -ID_CASILLA, -EXT_CONTIGUA,-ESTRATO), by='ID_ESTRATO') %>%
          group_by(ID_GRUPO) %>% 
          drop_na() %>%
          summarise(n_gpo = n()) %>% ungroup()
        
        completo<- dim(bool)[1]==ng
        #cat_id <- catx %>% pull(ID_GRUPO) %>% length()
        d<-data.frame(bool=completo, cat_id=ng)
        d
      }
      
      dfcat <- nc %>% map(get_post) %>% reduce(rbind)
      ngf <- dfcat %>% 
        filter(bool==TRUE) %>%
        arrange(-cat_id) %>%
        select(cat_id) %>% slice(1) %>% pull()
      
      print(sprintf("Se usaran %s grupos", ngf))
      catalogo <- conglomerados %>% 
        select(c("ID_ESTRATO", sprintf("cid_ng%s", ngf))) %>%
        rename(ID_GRUPO = sprintf("cid_ng%s", ngf))
      # 
      # ######## SELECCION DE LA CONF #######
      # ## OJO FILTRAR ESTRATOS SIN INFORMACION ANTES DE GENERAR PROMEDIOS!!!!
      # # Promedios para ssx a nivel grupo 
      gx <- ssx %>% left_join(catalogo, by='ID_ESTRATO') %>% 
        select(-ID_ESTRATO) %>%
        group_by(ID_GRUPO) %>%
        summarise_all(function(x){mean(x,na.rm=TRUE)})
      # 
      names(gx) <- c("ID_GRUPO", paste(names(ssx %>% select(-ID_ESTRATO)), "_GPO", sep='') )
      # 
      ssx_imp<-catalogo %>% left_join(ssx, by='ID_ESTRATO') %>%
        left_join(gx, by='ID_GRUPO') %>% 
        mutate(
          PAN = ifelse(is.na(PAN), PAN_GPO, PAN),
          PRI = ifelse(is.na(PRI), PRI_GPO, PRI),
          PRD = ifelse(is.na(PRD), PRD_GPO, PRD),
          PT = ifelse(is.na(PT), PT_GPO, PT),
          PVEM = ifelse(is.na(PVEM), PVEM_GPO, PVEM),
          MC = ifelse(is.na(MC), MC_GPO, MC),
          MORENA = ifelse(is.na(MORENA), MORENA_GPO, MORENA),
          CI = ifelse(is.na(CI), CI_GPO, CI),
          NOVOTO = ifelse(is.na(NOVOTO), NOVOTO_GPO, NOVOTO),
          NOVOTO_ADJ = ifelse(is.na(NOVOTO_ADJ), NOVOTO_ADJ_GPO, NOVOTO_ADJ)) %>% 
        select(-contains('_GPO'), -ID_GRUPO)
      
      # # Promedios para ssx2 a nivel grupo 
      gx2 <- ssx2 %>% left_join(catalogo, by='ID_ESTRATO') %>%
        select(-ID_ESTRATO) %>%
        group_by(ID_GRUPO) %>%
        summarise_all(mean)
      # 
      names(gx2) <- c("ID_GRUPO", paste(names(ssx %>% select(-ID_ESTRATO)), "_GPO", sep='') )
      # 
      ssx2_imp <- catalogo %>% left_join(ssx2, by='ID_ESTRATO') %>%
        left_join(gx2, by='ID_GRUPO') %>% 
        mutate(
          PAN = ifelse(is.na(PAN), PAN_GPO, PAN),
          PRI = ifelse(is.na(PRI), PRI_GPO, PRI),
          PRD = ifelse(is.na(PRD), PRD_GPO, PRD),
          PT = ifelse(is.na(PT), PT_GPO, PT),
          PVEM = ifelse(is.na(PVEM), PVEM_GPO, PVEM),
          MC = ifelse(is.na(MC), MC_GPO, MC),
          MORENA = ifelse(is.na(MORENA), MORENA_GPO, MORENA),
          CI = ifelse(is.na(CI), CI_GPO, CI),
          NOVOTO = ifelse(is.na(NOVOTO), NOVOTO_GPO, NOVOTO),
          NOVOTO_ADJ = ifelse(is.na(NOVOTO_ADJ), NOVOTO_ADJ_GPO, NOVOTO_ADJ)) %>% 
        select(-contains('_GPO'), -ID_GRUPO)
      
      ssx<-ssx_imp
      ssx2<-ssx2_imp
    }
    ###########################################################################
    ###########################################################################
    ###########################################################################
    #-Simulacion de la distribucion posterior-
    # Simulacion de theta
    # m -- suma de ln_adj x estrato
    # nc -- casillas x estrato
    # ss -- tamano de la simulacion cadena...
    names(ssx2)<-c("ID_ESTRATO", paste(names(ssx2 %>% select(-ID_ESTRATO)),"_BIS", sep=""))
    
    df_estrato <- left_join(ssx, ssx2, by='ID_ESTRATO') %>%
      left_join(lnx, by='ID_ESTRATO') 
    
    ########## IMPUTACION EN CASO DE MUESTRA FALTANTE ########
    
    df_estrato <- df_estrato %>% mutate(LN_ADJ=ifelse(is.na(LN_ADJ),750,LN_ADJ),
                                        LISTA_NOMINAL=ifelse(is.na(LISTA_NOMINAL),750,LISTA_NOMINAL), 
                                        NCASILLAS=ifelse(is.na(NCASILLAS),1,NCASILLAS))
    
    sim_partido<-function(id_estrato, partido){
      #id_estrato <- 1
      #partido <- 'PAN'
      vars <- c(partido, paste(partido,"_BIS",sep=''))
      dfx <- df_estrato %>% select(c(vars,'LN_ADJ', 'NCASILLAS','ID_ESTRATO')) %>%
        filter(ID_ESTRATO==id_estrato) %>% 
        mutate(A1 = max((NCASILLAS-1)/2,0.5),
               BB = (.data[[vars[[2]]]]-.data[[vars[1]]]^2/LN_ADJ)/2,
               B1 = ifelse(BB>0,BB,0.05), 
               MU1 = .data[[vars[[1]]]]/LN_ADJ) #%>%
      #print(dfx)
      if(dfx$NCASILLAS>0){
        tau<-rgamma(ss,dfx$A1,dfx$B1)
        sig1<-1/sqrt(tau*dfx$LN_ADJ)
        p<-rtruncnorm(ss,0,1,dfx$MU1,sig1)
        #print(p)
      }else{
        print("No hay casillas...")
        runif(ss,0,1)
      }
      dsim<-data.frame(p)
      names(dsim)<-'Valor'
      dsim$PARTIDO<-partido
      dsim$ID_ESTRATO<-id_estrato
      dsim$ID_SIM<-c(1:ss)
      dsim
    }
    
    sim_est<-function(id_est){
      #id_est<-1
      dfe<-mclapply( c(partido,"NOVOTO", "NOVOTO_ADJ"), function(x){sim_partido(id_est, x)}, mc.cores=detectCores()-2) %>% bind_rows()
      dfe
    }
    
    strata<-info %>% select(ID_ESTRATO) %>% unique() %>% pull()
    df_all<-mclapply(strata, sim_est, mc.cores=detectCores()-2) %>% bind_rows()
    
    theta_pre <- df_all %>%
      pivot_wider(names_from = PARTIDO, 
                  id_cols = c(ID_SIM, ID_ESTRATO), 
                  values_from = Valor)
    
    thetaC <- simplify2array(by(theta_pre %>% select(-ID_SIM, -ID_ESTRATO), theta_pre$ID_SIM, as.matrix)) #%>% s
    
    lambdaC <- df_all %>%
      left_join(info %>% select(LISTA_NOMINAL, ID_ESTRATO), by='ID_ESTRATO') %>%
      mutate(Valor = Valor*LISTA_NOMINAL/LNN ) %>% 
      select(-LISTA_NOMINAL) %>% 
      group_by(ID_SIM, PARTIDO) %>% 
      summarise(Valor=sum(Valor)) %>% 
      pivot_wider(names_from = PARTIDO, 
                  id_cols = ID_SIM, 
                  values_from=Valor)
    
    ##### SUPER DUDAS
    ### ¿¿PARTICIPACION IMPUTAMOS TMB???
    ### INDEPENDIENTES
    
    lambdaC <- lambdaC %>%
      rowwise() %>%
      #mutate(VE = sum(CI1, FPM, MC, MORENA, PAN, PES, PRD, PRI, PT, PVEM, RSP, NOVOTO)) %>% 
      mutate(VE = sum(CI, MC, MORENA, PAN, PRD, PRI, PT, PVEM)) %>% 
      mutate_at(c(partido, "NOVOTO"), funs(./VE)) %>%
      mutate(PART = (1-NOVOTO_ADJ)*sum(lnx$LN_ADJ)/sum(lnx$LISTA_NOMINAL)) %>%
      select(c(partido, "NOVOTO", "PART")) %>% ungroup()
    
    ###############################
    ### Informacion coaliciones ###
    
    coa<-read.csv("informacion/2024_Diputados_Coaliciones.csv")
    #np<-13
    #Asignacion de diputados por mayoria relativa
    pid<-matrix(NA,nrow=ss,ncol=nest)
    nd1<-matrix(NA,nrow=ss,ncol=np-2)
    
    lambda <- lambdaC %>% select(-ID_SIM)
    theta<-thetaC
    partido2<-partidos[1:(np-2)]
    partido3<-partidos[1:(np-3)]
    theta3<-theta[,1:(np-3),]

    for (i in 1:nest) {
      theta2<-theta[i,1:(np-2),]
      #
      if (coa$PAN[i]==1) {
        theta2[partido2=="PAN",]<-theta[i,partidos=="PAN",]+theta[i,partidos=="PRI",]+theta[i,partidos=="PRD",]
        theta2[partido2=="PRI",]<-0
        theta2[partido2=="PRD",]<-0
      }
      if (coa$PRI[i]==1) {
        theta2[partido2=="PAN",]<-0
        theta2[partido2=="PRI",]<-theta[i,partidos=="PAN",]+theta[i,partidos=="PRI",]+theta[i,partidos=="PRD",]
        theta2[partido2=="PRD",]<-0
      }
      if (coa$PRD[i]==1) {
        theta2[partido2=="PAN",]<-0
        theta2[partido2=="PRI",]<-0
        theta2[partido2=="PRD",]<-theta[i,partidos=="PAN",]+theta[i,partidos=="PRI",]+theta[i,partidos=="PRD",]
      }
      #
      if (coa$PVEM[i]==1) {
        theta2[partido2=="PVEM",]<-theta[i,partidos=="PVEM",]+theta[i,partidos=="PT",]+theta[i,partidos=="MORENA",]
        theta2[partido2=="PT",]<-0
        theta2[partido2=="MORENA",]<-0
      }
      if (coa$PT[i]==1) {
        theta2[partido2=="PVEM",]<-0
        theta2[partido2=="PT",]<-theta[i,partidos=="PVEM",]+theta[i,partidos=="PT",]+theta[i,partidos=="MORENA",]
        theta2[partido2=="MORENA",]<-0
      }
      if (coa$MORENA[i]==1) {
        theta2[partido2=="PVEM",]<-0
        theta2[partido2=="PT",]<-0
        theta2[partido2=="MORENA",]<-theta[i,partidos=="PVEM",]+theta[i,partidos=="PT",]+theta[i,partidos=="MORENA",]
      }
      #
      for (j in 1:(np-3)) {
        theta3[i,j,]<-ifelse(lambda[,j]>0.03,theta3[i,j,],0)
      }
      pid[,i]<-apply(theta2,2,which.max)
    }
    
    bb<-0:(np-2)
    for (s in 1:ss) {
      nd1[s,]<-hist(pid[s,],breaks=bb,plot=FALSE)$counts
    }
    nd1<-as.data.frame(nd1)
    names(nd1)<-partido2
    
    #Creacion de eta
    eta<-matrix(NA,nrow=ss,ncol=np-3)
    for (j in 1:(np-3)){
      eta[,j]<-apply(lnest*theta3[,j,]/LNN,2,sum)
    }
    ve2<-apply(eta,1,sum)
    eta<-eta/ve2
    colnames(eta)<-partido3
    eta<-as.data.frame(eta)
    
    #Asignacion de diputados por rep. proporcional
    eta2<-eta
    nd2<-floor(200*eta)
    rm2<-200*eta-nd2
    pm<-floor(500*(eta+0.08))
    pr<-pm
    pi<-pm
    rtot<-200-apply(nd2,1,sum)
    mm<-t(apply(rm2,1,order,decreasing=TRUE))
    im<-1:ss
    for (i in 1:ss) {
      im[i]<-length(rm2[i,rm2[i,]>0])
      for (j in 1:min(rtot[i],im[i])) {
        nd2[i,mm[j]]<-nd2[i,mm[j]]+1
      }
    }
    #
    for (k in 1:3) {
      for (j in 1:(np-3)) {
        nd2[,j]<-ifelse(nd1[,j]+nd2[,j]>pm[,j],pm[,j]-nd1[,j],nd2[,j])
        pr[,j]<-ifelse(nd1[,j]+nd2[,j]>pm[,j],0,pm[,j]-nd1[,j]-nd2[,j])
        pi[,j]<-ifelse(pr[,j]>0,1,0)
        #Correccion por rep.prop. negativa
        nd2[,j]<-ifelse(nd2[,j]<0,0,nd2[,j])
        pr[,j]<-ifelse(pr[,j]<0,0,pr[,j])
        #---------------------------------
      }
      rtot<-200-apply(nd2*(1-pi),1,sum)
      eta2<-pi*eta2/apply(pi*eta2,1,sum)
      nd2<-nd2*(1-pi)+floor(rtot*eta2)
      rm2<-pi*(rtot*eta2-floor(rtot*eta2))
      rtot<-200-apply(nd2,1,sum)
      mm<-t(apply(rm2,1,order,decreasing=TRUE))
      im<-1:ss
      for (i in 1:ss) {
        im[i]<-length(rm2[i,rm2[i,]>0])
        for (j in 1:min(rtot[i],im[i])) {
          nd2[i,mm[j]]<-nd2[i,mm[j]]+1
        }
      }
    }
    nd<-nd1
    nd[,-(np-2)]<-nd1[,-(np-2)]+nd2
    
    
    #Seleccion de confiabilidad
    l<-1
    while (c1/tm>n[l]) {l<-l+1}

    #Esritura de archivos COTECORA
    id2<-substr(remesa_name,3,10)
    id3<-substr(remesa_name,5,10)
    
    #Escritura de archivos con estimaciones
    out.p<-apply(lambda,2,quantile,probs=c((1-a[l])/2,0.5,(1+a[l])/2))
    out.c<-apply(nd,2,quantile,probs=c((1-a[l])/2,0.5,(1+a[l])/2))
    # write.csv(out.p,file=paste("../primer_simulacro/simba/buzon_diputados_nieto/nieto40",id,".csv",sep=""))
    # write.csv(out.c,file=paste("../primer_simulacro/simba/buzon_diputados_nieto/nietodip40",id,".csv",sep=""))

    #Esritura de archivos COTECORA
    id2<-substr(id,3,10)
    id3<-substr(id,5,10)
    cot.p<-cbind(rep("nieto",3),
                 rep("40",3),
                 rep(id3,3), 
                 round(out.p[,-9]*100,1),
                c(0,1,2))

    dimnames(cot.p)[[2]][1:3]<-c("EQ","EN","R")
    dimnames(cot.p)[[2]][11]<-"IND"
    dimnames(cot.p)[[2]][13]<-"LMU"
    
    write.csv(cot.p,
              file=sprintf("%s/nieto40%s.csv",output_path,remesa_name),
              row.names=FALSE,
              quote=FALSE)
    #
    cot.c<-cbind(rep("nieto",3),
                 rep("40",3),
                 rep(id3,3),
                 round(out.c,0),
                 c(0,1,2))

    dimnames(cot.c)[[2]][1:3]<-c("EQ","EN","R")
    dimnames(cot.c)[[2]][11]<-"IND"
    dimnames(cot.c)[[2]][12]<-"LMU"
    
    write.csv(cot.c,
              file=sprintf("%s/nietodip40%s.csv",output_path,remesa_name),
              row.names=FALSE,quote=FALSE)

    
  }else{
    print("SIN DATOS PARA ESTIMACIONES")}
}

# dir("../primer_simulacro/simba/diputados/") %>% map(function(x){substr(x,8,17)}) %>% unlist() %>% sort(decreasing = TRUE) %>% map(get_estimates_dip)

estimaciones_list<-dir(sprintf("%s/", output_path)) %>% map(function(x){substr(x,11,16)}) %>% unlist 
remesas_list<-dir(sprintf("%s/", remesa_path)) %>% map(function(x){substr(x,12,17)}) %>% unlist

num_cores<-2

time<-now()
setdiff(remesas_list, estimaciones_list) %>%
   sort(decreasing = TRUE) %>%
    mclapply(.,function(x){get_estimates_dip(x, remesa_path, output_path)}, mc.cores=num_cores)
print(now()-time)

