######## --- CONTEO RAPIDO BAYESIANO --- ##########
#--- Modelo Normal - Normal Gamma univariado ---#
#--- Estima las proporciones de votos efectivos ---#
#--- Estima el numero de diputados por partido ---#

#-Paquetes requeridos-
library(truncnorm)
library(tidyverse)
library(parallel)
#-Especificacion de la remesa-
#id = identificador de remesa

#path<-"smb://172.19.122.213"
path<-'../primer_simulacro/simba'
remesa_path<-sprintf('%s/senadores',path)
output_path<-sprintf("%s/buzon_senadores_nieto",path)

get_estimates_sen<-function(remesa_name, remesa_path, output_path){

  # rdir<-'../primer_simulacro/simba/senadores/'
  #setwd("~/ConteoRapido")
  #print(sprintf("REMESA %s",id))
  #Nivel de confiabilidad
  a<-c(0.99,0.98,0.97,0.96,0.95)
  n<-c(0.6,0.7,0.8,0.9,1.0)
  
  #-Especificaciones iniciales-
  #np = numero de partidos + 2
  #nest = numero de estratos
  #ss = tamano de la muestra posterior
  #tm = tamano de la muestra  
  tm<-1620
  
  partido<-c("PAN","PRI","PRD","PVEM","PT","MC","MORENA")
  partidos <- c(partido,"NOVOTO","PART")
  
  #np<-12 
  np<-length(partidos)  #12
  nest<-32
  ss<-10000
  
  #-Archivos de entrada-
  ## Lista Nominal 
  info<-read.csv("../informacion_2024/Info_estados_2024.csv")
  lnest<-info$LISTA_NOMINAL
  lnn<-sum(lnest)
  
  remesa<-read.table(sprintf("%s/REMESAS0500%s.txt", remesa_path, remesa_name), 
                      header=TRUE,
                      skip=1,
                      sep="|") %>% as.data.frame() 
                    
  c0<-nrow(remesa)
  #tm <- 170190
  
  c1 <- c0
  #c1<-scan(paste(rdir,"/REMESAS",id,".txt",sep=""),n=1)
  
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
  
  if(c1!=c0) {
    cat("Inconsistencia en el numero de casillas")
  } else {
    cat(paste("Numero de casillas en remesa =", c0), "\n")
  }
  
  if(c1>0){
    #-Creacion de variables-
    #Votos partidos + reparticion de votos en coalici?n
    vx <- remesa %>%
      mutate(
        PAN = PAN + PAN_PRI/2 + PAN_PRD/2 + PAN_PRI_PRD/3,
        PRI = PRI + PAN_PRI/2 + PRI_PRD/2 + PAN_PRI_PRD/3,
        PRD = PRD + PAN_PRD/2 + PRI_PRD/2 + PAN_PRI_PRD/3,
        PVEM = PVEM + PVEM_PT/2 + PVEM_MORENA/2 + PVEM_PT_MORENA/3,
        PT = PT + PT_MORENA/2 + PVEM_PT/2 + PVEM_PT_MORENA/3,
        MORENA = MORENA + PT_MORENA/2 + PVEM_MORENA/2 + PVEM_PT_MORENA/3,
        NOVOTO = NULOS + CNR)
    
    ##VOTOS
    vx$VOTOS<-apply(vx %>% select_at(c(partido,'NOVOTO')),1,sum)
    vx <- vx %>% 
      mutate(aux = LISTA_NOMINAL-VOTOS, 
             NOVOTO_ADJ = ifelse(aux>0, aux, 0), 
             LN_ADJ = ifelse(aux>0, LISTA_NOMINAL, VOTOS) ## Votacion Efectiva 
      ) %>% 
      filter(LN_ADJ!=0) %>% #Depuracion de casillas sin voto 
      left_join(info %>% rename(LISTA_NOMINAL_ESTRATO = LISTA_NOMINAL),
                by='ID_ESTADO') 
    
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
      group_by(ID_ESTADO) %>%
      summarise_at(.vars = c(partido,'NOVOTO', 'NOVOTO_ADJ'), .funs = sum)
    
    ssx2 <- xy %>% 
      mutate_at(.vars = c(partido,'NOVOTO', 'NOVOTO_ADJ'), .funs = funs((.)^2/LN_ADJ)) %>%
      group_by(ID_ESTADO) %>% 
      summarise_at(.vars = c(partido,'NOVOTO', 'NOVOTO_ADJ'), sum)
    
    #Para cada estrato en remesa se suma el listado
    lnx <- vx %>% 
      group_by(ID_ESTADO) %>% 
      summarise(LN_ADJ = sum(LN_ADJ, na.rm=TRUE), 
                NCASILLAS = n(), 
                LISTA_NOMINAL = sum(LISTA_NOMINAL))
    
    ############################################################
    ############## CAMBIO ######################################
    ############################################################
    
    ###
    #asignarles a los estratos sin muestra 
    #la informacion promedio del grupo al que pertenecen
    
    if(dim(ssx)[1] < 32){
      
      print("Realizando Posestratificacion")
      
      conglomerados<-read.csv("./informacion_2018/cluster_catalog_senadores_2018.csv") 
      nc <-  c(1,3,5,10,15,20,25,32)
      
      # Para cada configuracion (catalogo)
      ## Identificar estatos ausentes en la remesa
      ## Para cada configuracion (de 300 a 1), elegir si  
      
      ###########################################################################
      ###########################################################################
      ###########################################################################
      ###########################################################################
      
      get_post<-function(ng){
        ### Registrar #gpos en remesa
        
        #ng <- 20
        catx <- conglomerados %>% 
          select(c("ID_ESTADO", sprintf("cid_ng%s", ng))) %>%
          rename(ID_GRUPO = sprintf("cid_ng%s", ng))
        bool <- catx %>%
          left_join(xy %>% 
                      select(-ORIGEN_CAPTURA, 
                             -MODIFICADO, 
                             -ANIO, -MES, -DIA, -HORA, -MINUTOS, -SEGUNDOS, -ESTRATO), by='ID_ESTADO') %>%
          group_by(ID_GRUPO) %>% drop_na() %>% ## AGUAS CON LOS NAs!
          summarise(n_gpo = n()) %>% ungroup()
        
        completo<- dim(bool)[1]==ng
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
        select(c("ID_ESTADO", sprintf("cid_ng%s", ngf))) %>%
        rename(ID_GRUPO = sprintf("cid_ng%s", ngf))
      # 
      # ######## SELECCION DE LA CONF #######
      # ## OJO FILTRAR ESTRATOS SIN INFORMACION ANTES DE GENERAR PROMEDIOS!!!!
      # # Promedios para ssx a nivel grupo 
      gx <- ssx %>% left_join(catalogo, by='ID_ESTADO') %>% 
        select(-ID_ESTADO) %>%
        group_by(ID_GRUPO) %>%
        summarise_all(mean)
      # 
      names(gx) <- c("ID_GRUPO", paste(names(ssx %>% select(-ID_ESTADO)), "_GPO", sep='') )
      # 
      ssx_imp<-catalogo %>% left_join(ssx, by='ID_ESTADO') %>%
        left_join(gx, by='ID_GRUPO') %>% 
        mutate(
          PAN = ifelse(is.na(PAN), PAN_GPO, PAN),
          PRI = ifelse(is.na(PRI), PRI_GPO, PRI),
          PRD = ifelse(is.na(PRD), PRD_GPO, PRD),
          PT = ifelse(is.na(PT), PT_GPO, PT),
          PVEM = ifelse(is.na(PVEM), PVEM_GPO, PVEM),
          MC = ifelse(is.na(MC), MC_GPO, MC),
          MORENA = ifelse(is.na(MORENA), MORENA_GPO, MORENA),
          NOVOTO = ifelse(is.na(NOVOTO), NOVOTO_GPO, NOVOTO),
          NOVOTO_ADJ = ifelse(is.na(NOVOTO_ADJ), NOVOTO_ADJ_GPO, NOVOTO_ADJ)) %>% 
        select(-contains('_GPO'), -ID_GRUPO)
      
      # # Promedios para ssx2 a nivel grupo 
      gx2 <- ssx2 %>% left_join(catalogo, by='ID_ESTADO') %>%
        select(-ID_ESTADO) %>%
        group_by(ID_GRUPO) %>%
        summarise_all(mean)
      # 
      names(gx2) <- c("ID_GRUPO", paste(names(ssx %>% select(-ID_ESTADO)), "_GPO", sep='') )
      # 
      ssx2_imp <- catalogo %>% left_join(ssx2, by='ID_ESTADO') %>%
        left_join(gx2, by='ID_GRUPO') %>% 
        mutate(
          PAN = ifelse(is.na(PAN), PAN_GPO, PAN),
          PRI = ifelse(is.na(PRI), PRI_GPO, PRI),
          PRD = ifelse(is.na(PRD), PRD_GPO, PRD),
          PT = ifelse(is.na(PT), PT_GPO, PT),
          PVEM = ifelse(is.na(PVEM), PVEM_GPO, PVEM),
          MC = ifelse(is.na(MC), MC_GPO, MC),
          MORENA = ifelse(is.na(MORENA), MORENA_GPO, MORENA),
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
    names(ssx2)<-c("ID_ESTADO", paste(names(ssx2 %>% select(-ID_ESTADO)),"_BIS", sep=""))
    
    df_estrato <- left_join(ssx, ssx2, by='ID_ESTADO') %>%
      left_join(lnx, by='ID_ESTADO') 
    
    ########## IMPUTACION EN CASO DE MUESTRA FALTANTE ########
    ## IMPUTACION LISTA NOMINAL ## CAMBIAR 750 por 7000 ¿?
    
    df_estrato <- df_estrato %>% mutate(LN_ADJ=ifelse(is.na(LN_ADJ),750,LN_ADJ),
                                        LISTA_NOMINAL=ifelse(is.na(LISTA_NOMINAL),750,LISTA_NOMINAL), 
                                        NCASILLAS=ifelse(is.na(NCASILLAS),1,NCASILLAS))
    
    ## SIMULACION PARA THETA
    sim_partido<-function(id_estrato, partido){
      #id_estrato <- 1
      #partido <- 'PAN'
      vars <- c(partido, paste(partido,"_BIS",sep=''))
      dfx <- df_estrato %>% select(c(vars,'LN_ADJ', 'NCASILLAS','ID_ESTADO')) %>%
        filter(ID_ESTADO==id_estrato) %>% 
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
      dsim$ID_ESTADO<-id_estrato
      dsim$ID_SIM<-c(1:ss)
      dsim
    }
    
    sim_est<-function(id_est){
      #id_est<-1

      dfe<-mclapply( c(partido,"NOVOTO", "NOVOTO_ADJ"), function(x){sim_partido(id_est, x)}, mc.cores=detectCores()-2) %>% bind_rows()
      dfe
    }
    
    strata<-info %>% select(ID_ESTADO) %>% unique() %>% pull()
    df_all<-mclapply(strata,sim_est, mc.cores=detectCores()-2) %>% bind_rows()
    
    theta_pre <- df_all %>%
      pivot_wider(names_from = PARTIDO, 
                  id_cols = c(ID_SIM, ID_ESTADO), 
                  values_from = Valor)
    
    thetaC <- simplify2array(by(theta_pre %>% select(-ID_SIM, -ID_ESTADO), theta_pre$ID_SIM, as.matrix)) #%>% s
    
    ## GENERACION DE LAMBDA
    lambdaC <- df_all %>%
      left_join(info %>% select(LISTA_NOMINAL, ID_ESTADO), by='ID_ESTADO') %>%
      mutate(Valor = Valor*LISTA_NOMINAL/lnn ) %>% 
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
      mutate(VE = sum(MC, MORENA, PAN, PRD, PRI, PT, PVEM)) %>% 
      mutate_at(c(partido, "NOVOTO"), funs(./VE)) %>%
      mutate(PART = (1-NOVOTO_ADJ)*sum(lnx$LN_ADJ)/sum(lnx$LISTA_NOMINAL)) %>%
      select(c(partido, "NOVOTO", "PART")) %>% ungroup()
    
    ###############################
    ### Informacion coaliciones ###
    
    coa1<-read.csv("../informacion_2024/coalicion2024_1a.csv")
    coa2<-read.csv("../informacion_2024/coalicion2024_2a.csv")
    
    np<-length(partidos)
    
    #Asignacion de diputados por mayoria relativa
    pid1<-matrix(NA,nrow=ss,ncol=nest)
    pid2<-matrix(NA,nrow=ss,ncol=nest)
    pid3<-matrix(NA,nrow=ss,ncol=nest)
    nd1<-matrix(NA,nrow=ss,ncol=np-2)
    nd2<-matrix(NA,nrow=ss,ncol=np-2)
    nd3<-matrix(NA,nrow=ss,ncol=np-2)
    
    lambda <- lambdaC %>% select(-ID_SIM)
    theta<-thetaC
    
    partido2<-partidos[1:(np-2)]
    partido3<-partidos[1:(np-2)]
    theta3<-theta[,1:(np-2),]
    
    for (i in 1:nest) {
      #coa1
      #i<-1
      theta2<-theta[i,1:(np-2),]
      #
      if (coa1$PAN[i]==1) {
        theta2[partido2=="PAN",]<-theta[i,partidos=="PAN",]+theta[i,partidos=="PRI",]+theta[i,partidos=="PRD",]
        theta2[partido2=="PRD",]<-0
        theta2[partido2=="PRI",]<-0
      }
      if (coa1$PRD[i]==1) {
        theta2[partido2=="PAN",]<-0
        theta2[partido2=="PRD",]<-theta[i,partidos=="PAN",]+theta[i,partidos=="PRD",]+theta[i,partidos=="PRI",]
        theta2[partido2=="PRI",]<-0
      }
      if (coa1$PRI[i]==1) {
        theta2[partido2=="PRI",]<-theta[i,partidos=="PRI",]+theta[i,partidos=="PRD",]+theta[i,partidos=="PAN",]
        theta2[partido2=="PRD",]<-0
        theta2[partido2=="PAN",]<-0
      }
      if (coa1$PVEM[i]==1) {
        theta2[partido2=="MORENA",]<-0
        theta2[partido2=="PVEM",]<-theta[i,partidos=="PVEM",]+theta[i,partidos=="PT",]+theta[i,partidos=="MORENA",]
        theta2[partido2=="PT",]<-0
      }
      #
      if (coa1$PT[i]==1) {
        theta2[partido2=="PT",]<-theta[i,partidos=="PT",]+theta[i,partidos=="MORENA",]+theta[i,partidos=="PVEM",]
        theta2[partido2=="MORENA",]<-0
        theta2[partido2=="PVEM",]<-0
      }
      if (coa1$MORENA[i]==1) {
        theta2[partido2=="PT",]<-0
        theta2[partido2=="MORENA",]<-theta[i,partidos=="PT",]+theta[i,partidos=="MORENA",]+theta[i,partidos=="PVEM",]
        theta2[partido2=="PVEM",]<-0
      }
      pid1[,i]<-apply(theta2,2,which.max)
      pid3[,i]<-apply(theta2,2,order,decreasing=TRUE)[2,]
      #coa2
      theta2<-theta[i,1:(np-2),]
      
      #### SEGUNDA MAYORIA 
      
      if (coa2$PAN[i]==1) {
        theta2[partido2=="PAN",]<-theta[i,partidos=="PAN",]+theta[i,partidos=="PRI",]+theta[i,partidos=="PRD",]
        theta2[partido2=="PRD",]<-0
        theta2[partido2=="PRI",]<-0
      }
      if (coa2$PRD[i]==1) {
        theta2[partido2=="PAN",]<-0
        theta2[partido2=="PRD",]<-theta[i,partidos=="PAN",]+theta[i,partidos=="PRD",]+theta[i,partidos=="PRI",]
        theta2[partido2=="PRI",]<-0
      }
      if (coa2$PRI[i]==1) {
        theta2[partido2=="PRI",]<-theta[i,partidos=="PRI",]+theta[i,partidos=="PRD",]+theta[i,partidos=="PAN",]
        theta2[partido2=="PRD",]<-0
        theta2[partido2=="PAN",]<-0
      }
      if (coa2$PVEM[i]==1) {
        theta2[partido2=="MORENA",]<-0
        theta2[partido2=="PVEM",]<-theta[i,partidos=="PVEM",]+theta[i,partidos=="PT",]+theta[i,partidos=="MORENA",]
        theta2[partido2=="PT",]<-0
      }
      #
      if (coa2$PT[i]==1) {
        theta2[partido2=="PT",]<-theta[i,partidos=="PT",]+theta[i,partidos=="MORENA",]+theta[i,partidos=="PVEM",]
        theta2[partido2=="MORENA",]<-0
        theta2[partido2=="PVEM",]<-0
      }
      if (coa2$MORENA[i]==1) {
        theta2[partido2=="PT",]<-0
        theta2[partido2=="MORENA",]<-theta[i,partidos=="PT",]+theta[i,partidos=="MORENA",]+theta[i,partidos=="PVEM",]
        theta2[partido2=="PVEM",]<-0
      }
      
      pid2[,i]<-apply(theta2,2,which.max)
      
      #### OJO AQUI CAMBIO !!! 
      
      for (j in 1:(np-2)) {
        theta3[i,j,]<-ifelse(lambda[,j]>0.03,theta3[i,j,],0)
      }
    }
    
    bb<-0:(np-2)
    for (s in 1:ss) {
      nd1[s,]<-hist(pid1[s,],breaks=bb,plot=FALSE)$counts
      nd2[s,]<-hist(pid2[s,],breaks=bb,plot=FALSE)$counts
      nd3[s,]<-hist(pid3[s,],breaks=bb,plot=FALSE)$counts
    }
    nd1<-as.data.frame(nd1)
    nd2<-as.data.frame(nd2)
    nd3<-as.data.frame(nd3)
    names(nd1)<-partido2
    names(nd2)<-partido2
    names(nd3)<-partido2
    
    #Creacion de eta
    eta<-matrix(NA,nrow=ss,ncol=np-2)
    for (j in 1:(np-2)){
      eta[,j]<-apply(lnest*theta3[,j,]/lnn,2,sum)
    }
    ve2<-apply(eta,1,sum)
    eta<-eta/ve2
    colnames(eta)<-partido3
    eta<-as.data.frame(eta)
    
    #Asignacion de senadoress por rep. proporcional
    totrp<-32
    eta2<-eta
    nd4<-floor(totrp*eta)
    rm2<-totrp*eta-nd4
    #
    rtot<-totrp-apply(nd4,1,sum)
    mm<-t(apply(rm2,1,order,decreasing=TRUE))
    im<-1:ss
    
    for (i in 1:ss) {
      im[i]<-length(rm2[i,rm2[i,]>0])
      for (j in 1:min(rtot[i],im[i])) {
        nd4[i,mm[i,j]]<-nd4[i,mm[i,j]]+1
      }
    }
    #senadores totales
    nd<-nd1+nd2+nd3+nd4

    #Seleccion de confiabilidad
    l<-1
    while (c1/tm>n[l]) {l<-l+1}
    
    #Escritura de archivos con estimaciones
    out.p<-apply(lambda,2,quantile,probs=c((1-a[l])/2,0.5,(1+a[l])/2))
    out.c<-apply(nd,2,quantile,probs=c((1-a[l])/2,0.5,(1+a[l])/2))
    
    #Escritura de archivos COTECORA
    pps<-c('PAN','PRI','PRD','PT','PVEM','MC','MORENA')
    
    orden_cotecora<-c('EQ', 'EN','R', pps,
                      'PART','LMU')
    
    hora<-substr(remesa_name,3,6)

    cot.p <- round(out.p[,-11]*100,1) %>% data.frame()
    cot.p <- cot.p %>% mutate(EQ = "nieto",
                              EN = "50",
                              R = substr(remesa_name,1,10))
    cot.p$LMU<-c(0,1,2)

    ##extraer hora

    write.csv(cot.p %>% 
                select(all_of(orden_cotecora)),
              file=sprintf("%s/nieto50%s.csv",output_path,remesa_name),
              row.names=FALSE, 
              quote=FALSE)
    
    # write.csv(cot.p %>% 
    #             select(all_of(orden_cotecora)),
    #           file=paste("../primer_simulacro/simba/buzon_senadores_nieto/nieto50",substr(remesa_name,5,10),".csv",sep=""),
    #           row.names=FALSE,
    #           quote=FALSE)
    
    ######## Senadurias ########
    
    orden_cotecora<-c('EQ', 'EN','R', pps,'LMU')
    
    cot.d <- out.c %>% data.frame()
    cot.d <- cot.d %>% mutate(EQ = "nieto",
                              EN = "50",
                              R = hora)
    cot.d$LMU<-c(0,1,2)
    write.csv(cot.d %>% 
                select(all_of(orden_cotecora)),
              
              file=sprintf("%s/nietosen50%s.csv",output_path, remesa_name), 
              row.names=FALSE, 
              quote=FALSE)
    
    # write.csv(cot.d %>% 
    #             select(all_of(orden_cotecora)),
    #           file=paste("../primer_simulacro/simba/buzon_senadores_nieto/nietosen50", substr(remesa_name,5,10), ".csv",sep=""),
    #           row.names=FALSE, 
    #           quote=FALSE)
    
  }else{
    print("SIN DATOS PARA ESTIMACIONES")}
}

estimaciones_list<-dir(sprintf("%s/", output_path)) %>% map(function(x){substr(x,11,16)}) %>% unlist 
remesas_list<-dir(sprintf("%s/", remesa_path)) %>% map(function(x){substr(x,12,17)}) %>% unlist

num_cores<-detectCores()-2

time<-now()
setdiff(remesas_list, estimaciones_list) %>%
   sort(decreasing = TRUE) %>%
    mclapply(.,function(x){get_estimates_sen(x, remesa_path, output_path)}, mc.cores=num_cores)
print(now()-time)


#num_cores<-detectCores()-2

