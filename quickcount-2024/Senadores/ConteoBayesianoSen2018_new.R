### --- CONTEO RAPIDO BAYESIANO --- ###
#--- Modelo Normal - Normal Gamma univariado ---#
#--- Estima las proporciones de votos efectivos ---#
#--- Estima el n?mero de diputados por partido ---

#-Paquetes requeridos-
#install.packages("truncnorm")
library(truncnorm)
library(stringr)
#-Directorio de trabajo-
dir<-"~/Desktop/quickcount/quickcount-2024/Senadores/"

#dir<-"c:/lnieto/Proyectos/INE2024/Conteo/Senadores/"
setwd(dir)

#-Especificacion de la remesa 
args<-commandArgs(trailingOnly=TRUE)
remesa_name<-args[1]

## TEST
#id = identificador de remesa
#id<-"1"

#Nivel de confiabilidad
a<-c(0.99,0.98,0.97,0.96,0.95)
n<-c(0.6,0.7,0.8,0.9,1.0)

#-Especificaciones iniciales-
#np = numero de partidos + 2
#nest = numero de estratos
#ss = tamano de la muestra posterior
#tm = tamano de la muestra  
#tm<-960

nest<-32
ss<-1000

partido<-c("PAN","PRI","PRD","PVEM","PT","MC","PNA",
           "MORENA","PES","CI1","NULOSNR","PART")

np<-length(partido)  #12

#-Archivos de entrada-

## Lista Nominal 
info<-read.csv("Info_estados_2018.txt")
lnest<-info$LISTA_NOMINAL
lnn<-sum(lnest)

#c1<-scan(paste(dir,"REMESAS",id,".txt",sep=""),n=1)
#c1<-tm

## mkdir Senadores
## mkdir Senadores/remesas

#remesa<-read.table(paste("./remesas/REMESAS",id,".txt",sep=""),header=TRUE,skip=0,sep="|")

remesa<-read.table(paste("./remesas_anidadas/",remesa_name,sep=""),header=TRUE,skip=0,sep="|")

#remesa<-read.table(paste("./remesas/",remesa_name,sep=""),header=TRUE,skip=0,sep="|")

tm <- 157069
#tm <- nrow(remesa)

## En este caso tm == remesa (son completas)

c1 <- tm
#remesa<-read.table(paste(dir,"REMESAS",id,".txt",sep=""),header=TRUE,skip=1,sep="|")
###

## POSITION ORIENTED 

j1<-which(names(remesa)=="PAN")
j2<-which(names(remesa)=="TOTAL_VOTOS")
for (j in j1:j2) {
  remesa[,j]<-ifelse(is.na(remesa[,j]),0,remesa[,j])
}
c0<-nrow(remesa)
if(c1!=c0) {
  cat("Inconsistencia en el numero de casillas")
  } else {
  cat(paste("Numero de casillas en remesa =",c0),"\n")
    }

#-Creacion de variables-
#Votos partidos + reparticion de votos en coalici?n
x<-matrix(NA,nrow=c0,ncol=np)
x[,1]<-remesa$PAN+remesa$PAN_PRD/2+remesa$PAN_MC/2+remesa$PAN_PRD_MC/3
x[,2]<-remesa$PRI+remesa$PRI_PVEM/2+remesa$PRI_NA/2+remesa$PRI_PVEM_NA/3
x[,3]<-remesa$PRD+remesa$PAN_PRD/2+remesa$PRD_MC/2+remesa$PAN_PRD_MC/3
x[,4]<-remesa$PVEM+remesa$PRI_PVEM/2+remesa$PVEM_NA/2+remesa$PRI_PVEM_NA/3
x[,5]<-remesa$PT+remesa$PT_MORENA/2+remesa$PT_ES/2+remesa$PT_MORENA_ES/3
x[,6]<-remesa$MC+remesa$PAN_MC/2+remesa$PRD_MC/2+remesa$PAN_PRD_MC/3
x[,7]<-remesa$NA.+remesa$PRI_NA/2+remesa$PVEM_NA/2+remesa$PRI_PVEM_NA/3
x[,8]<-remesa$MORENA+remesa$PT_MORENA/2+remesa$MORENA_ES/2+remesa$PT_MORENA_ES/3
x[,9]<-remesa$ES+remesa$PT_ES/2+remesa$MORENA_ES/2+remesa$PT_MORENA_ES/3
x[,10]<-remesa$CAND_IND1+remesa$CAND_IND2+remesa$CAND_IND3+remesa$CAND_IND4+remesa$CAND_IND5+remesa$CAND_IND6+remesa$CAND_IND7
x[,11]<-remesa$NUM_VOTOS_NULOS+remesa$NUM_VOTOS_CAN_NREG
#Lista nominal y no voto
y1<-remesa$LISTA_NOMINAL
y2<-apply(x[,-np],1,sum)
aux<-y1-y2
x[,np]<-ifelse(aux>0,aux,0)
lnc<-ifelse(aux>0,y1,y2)
#Depuracion de casillas sin voto
remesa<-remesa[lnc!=0,]
x<-x[lnc!=0,]
lnc<-lnc[lnc!=0]
#Estrato 
est<-remesa$ID_ESTADO

#Tam. de muestra y estadisticas
tt<-table(est)
aux<-as.numeric(names(tt))
nc<-hist(est,breaks=0:nest,plot=FALSE)$counts
sx<-matrix(NA,nrow=nest,ncol=np)
sx2n<-matrix(NA,nrow=nest,ncol=np)
for (j in 1:np){
  sx[aux,j]<-as.matrix(by(x[,j],est,sum))[,1]
  sx2n[aux,j]<-as.matrix(by((x[,j]^2)/lnc,est,sum))[,1]
}
m<-rep(NA,nest)
m[aux]<-as.matrix(by(lnc,est,sum))[,1]

#Ajuste para estratos sin muestra
nc0<-1
m0<-750
nc[-aux]<-nc0
if (length(aux)>1) {
  sx[-aux,]<-apply(sx[aux,],2,mean)/mean(m[aux])*m0
  sx2n[-aux,]<-apply(sx2n[aux,],2,mean)/mean(m[aux])*m0
} else {
  sx[-aux,]<-mean(sx[aux,])/mean(m[aux])*m0
  sx2n[-aux,]<-mean(sx2n[aux,])/mean(m[aux])*m0
}
m[-aux]<-m0

#-Simulacion de la distribucion posterior-
#Simulacion de theta
theta<-array(NA,dim=c(nest,np,ss))
for (i in 1:nest){
  for (j in 1:np){
    a1<-max((nc[i]-1)/2,0.5)
    bb<-(sx2n[i,j]-(sx[i,j]^2)/m[i])/2
    b1<-ifelse(bb>0,bb,0.05)
    if (nc[i]>0) {
      tau<-rgamma(ss,a1,b1)
      mu1<-sx[i,j]/m[i]
      sig1<-1/sqrt(tau*m[i])
      p<-rtruncnorm(ss,0,1,mu1,sig1)
    } else {
      p<-runif(ss,0,1)
    }
    theta[i,j,]<-p
  }
}
#
#Creacion de lambda
lam<-matrix(NA,nrow=ss,ncol=np)
for (j in 1:np){
  lam[,j]<-apply(lnest*theta[,j,]/lnn,2,sum)
}
vve<-apply(lam[,-c(np-1,np)],1,sum)
lambda<-lam
lambda[,-c(np-1,np)]<-lam[,-c(np-1,np)]/vve
lambda[,np-1]<-0
#correccion en part x listado nominal
lambda[,np]<-(1-lam[,np])*sum(lnc)/sum(y1)
colnames(lambda)<-partido
lambda<-as.data.frame(lambda)

#---------------------------------------------

#Info coaliciones
coa1<-read.csv("coalicion2018_1a.csv")
coa2<-read.csv("coalicion2018_2a.csv")

#Asignacion de diputados por mayoria relativa
pid1<-matrix(NA,nrow=ss,ncol=nest)
pid2<-matrix(NA,nrow=ss,ncol=nest)
pid3<-matrix(NA,nrow=ss,ncol=nest)
nd1<-matrix(NA,nrow=ss,ncol=np-2)
nd2<-matrix(NA,nrow=ss,ncol=np-2)
nd3<-matrix(NA,nrow=ss,ncol=np-2)
partido2<-partido[1:(np-2)]
partido3<-partido[1:(np-3)]
theta3<-theta[,1:(np-3),]
for (i in 1:nest) {
  #coa1
  theta2<-theta[i,1:(np-2),]
  #
  if (coa1$PAN[i]==1) {
    theta2[partido2=="PAN",]<-theta[i,partido=="PAN",]+theta[i,partido=="PRD",]+theta[i,partido=="MC",]
    theta2[partido2=="PRD",]<-0
    theta2[partido2=="MC",]<-0
  }
  if (coa1$PRD[i]==1) {
    theta2[partido2=="PAN",]<-0
    theta2[partido2=="PRD",]<-theta[i,partido=="PAN",]+theta[i,partido=="PRD",]+theta[i,partido=="MC",]
    theta2[partido2=="MC",]<-0
  }
  if (coa1$MC[i]==1) {
    theta2[partido2=="PAN",]<-0
    theta2[partido2=="PRD",]<-0
    theta2[partido2=="MC",]<-theta[i,partido=="PAN",]+theta[i,partido=="PRD",]+theta[i,partido=="MC",]
  }
  #
  if (coa1$PRI[i]==1) {
    theta2[partido2=="PRI",]<-theta[i,partido=="PRI",]+theta[i,partido=="PVEM",]+theta[i,partido=="PNA",]
    theta2[partido2=="PVEM",]<-0
    theta2[partido2=="PNA",]<-0
  }
  if (coa1$PVEM[i]==1) {
    theta2[partido2=="PRI",]<-0
    theta2[partido2=="PVEM",]<-theta[i,partido=="PRI",]+theta[i,partido=="PVEM",]+theta[i,partido=="PNA",]
    theta2[partido2=="PNA",]<-0
  }
  if (coa1$PNA[i]==1) {
    theta2[partido2=="PRI",]<-0
    theta2[partido2=="PVEM",]<-0
    theta2[partido2=="PNA",]<-theta[i,partido=="PRI",]+theta[i,partido=="PVEM",]+theta[i,partido=="PNA",]
  }
  #
  if (coa1$PT[i]==1) {
    theta2[partido2=="PT",]<-theta[i,partido=="PT",]+theta[i,partido=="MORENA",]+theta[i,partido=="PES",]
    theta2[partido2=="MORENA",]<-0
    theta2[partido2=="PES",]<-0
  }
  if (coa1$MORENA[i]==1) {
    theta2[partido2=="PT",]<-0
    theta2[partido2=="MORENA",]<-theta[i,partido=="PT",]+theta[i,partido=="MORENA",]+theta[i,partido=="PES",]
    theta2[partido2=="PES",]<-0
  }
  if (coa1$PES[i]==1) {
    theta2[partido2=="PT",]<-0
    theta2[partido2=="MORENA",]<-0
    theta2[partido2=="PES",]<-theta[i,partido=="PT",]+theta[i,partido=="MORENA",]+theta[i,partido=="PES",]
  }
  pid1[,i]<-apply(theta2,2,which.max)
  pid3[,i]<-apply(theta2,2,order,decreasing=TRUE)[2,]
  #coa2
  theta2<-theta[i,1:(np-2),]
  #
  if (coa2$PAN[i]==1) {
    theta2[partido2=="PAN",]<-theta[i,partido=="PAN",]+theta[i,partido=="PRD",]+theta[i,partido=="MC",]
    theta2[partido2=="PRD",]<-0
    theta2[partido2=="MC",]<-0
  }
  if (coa2$PRD[i]==1) {
    theta2[partido2=="PAN",]<-0
    theta2[partido2=="PRD",]<-theta[i,partido=="PAN",]+theta[i,partido=="PRD",]+theta[i,partido=="MC",]
    theta2[partido2=="MC",]<-0
  }
  if (coa2$MC[i]==1) {
    theta2[partido2=="PAN",]<-0
    theta2[partido2=="PRD",]<-0
    theta2[partido2=="MC",]<-theta[i,partido=="PAN",]+theta[i,partido=="PRD",]+theta[i,partido=="MC",]
  }
  #
  if (coa2$PRI[i]==1) {
    theta2[partido2=="PRI",]<-theta[i,partido=="PRI",]+theta[i,partido=="PVEM",]+theta[i,partido=="PNA",]
    theta2[partido2=="PVEM",]<-0
    theta2[partido2=="PNA",]<-0
  }
  if (coa2$PVEM[i]==1) {
    theta2[partido2=="PRI",]<-0
    theta2[partido2=="PVEM",]<-theta[i,partido=="PRI",]+theta[i,partido=="PVEM",]+theta[i,partido=="PNA",]
    theta2[partido2=="PNA",]<-0
  }
  if (coa2$PNA[i]==1) {
    theta2[partido2=="PRI",]<-0
    theta2[partido2=="PVEM",]<-0
    theta2[partido2=="PNA",]<-theta[i,partido=="PRI",]+theta[i,partido=="PVEM",]+theta[i,partido=="PNA",]
  }
  #
  if (coa2$PT[i]==1) {
    theta2[partido2=="PT",]<-theta[i,partido=="PT",]+theta[i,partido=="MORENA",]+theta[i,partido=="PES",]
    theta2[partido2=="MORENA",]<-0
    theta2[partido2=="PES",]<-0
  }
  if (coa2$MORENA[i]==1) {
    theta2[partido2=="PT",]<-0
    theta2[partido2=="MORENA",]<-theta[i,partido=="PT",]+theta[i,partido=="MORENA",]+theta[i,partido=="PES",]
    theta2[partido2=="PES",]<-0
  }
  if (coa2$PES[i]==1) {
    theta2[partido2=="PT",]<-0
    theta2[partido2=="MORENA",]<-0
    theta2[partido2=="PES",]<-theta[i,partido=="PT",]+theta[i,partido=="MORENA",]+theta[i,partido=="PES",]
  }
  pid2[,i]<-apply(theta2,2,which.max)
  #
  for (j in 1:(np-3)) {
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
eta<-matrix(NA,nrow=ss,ncol=np-3)
for (j in 1:(np-3)){
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
nd<-nd1+nd2+nd3
nd[,-(np-2)]<-nd1[,-(np-2)]+nd2[,-(np-2)]+nd3[,-(np-2)]+nd4


# Calculo de Indicadores

resultados <- read.csv('resultados_wiki_2018.csv')

nd$NEMA_CI1 <- nd$CI1-resultados$CI1
nd$NEMA_PES <- nd$PES-resultados$PES
nd$NEMA_MC <- nd$MC-resultados$MC
nd$NEMA_MORENA <- nd$MORENA-resultados$MORENA
nd$NEMA_PNA <- nd$PNA-resultados$PNA
nd$NEMA_PAN <- nd$PAN-resultados$PAN
nd$NEMA_PRD <- nd$PRD-resultados$PRD
nd$NEMA_PRI <- nd$PRI-resultados$PRI
nd$NEMA_PT <- nd$PT-resultados$PT
nd$NEMA_PVEM <- nd$PVEM-resultados$PVEM

#Suma Norma |.| NEMAs
nd$NEMA  <- abs(nd$NEMA_CI1)+
            abs(nd$NEMA_PES)+
            abs(nd$NEMA_MC)+
            abs(nd$NEMA_MORENA)+
            abs(nd$NEMA_PNA)+
            abs(nd$NEMA_PAN)+
            abs(nd$NEMA_PRD)+
            abs(nd$NEMA_PRI)+
            abs(nd$NEMA_PT)+
            abs(nd$NEMA_PVEM)

# Promedio del NEMA
nd$PNEMA  <- 1/10*(
          nd$NEMA_CI1+
          nd$NEMA_PES+
          nd$NEMA_MC+
          nd$NEMA_MORENA+
          nd$NEMA_PNA+
          nd$NEMA_PAN+
          nd$NEMA_PRD+
          nd$NEMA_PRI+
          nd$NEMA_PT+
          nd$NEMA_PVEM)

# Maximo del NEMA
nd$MNEMA  <-  max(c(
          nd$NEMA_CI1,
          nd$NEMA_PES,
          nd$NEMA_MC,
          nd$NEMA_MORENA,
          nd$NEMA_PNA,
          nd$NEMA_PAN,
          nd$NEMA_PRD,
          nd$NEMA_PRI,
          nd$NEMA_PT,
          nd$NEMA_PVEM))

out<-as.data.frame(apply(nd,2,quantile,probs=c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)))

## Norma |.|_1 para los estimadores medianas (q50%) 
out$NEMA_NEW <-  abs(out$NEMA_CI1)+
                  abs(out$NEMA_PES)+
                  abs(out$NEMA_MC)+
                  abs(out$NEMA_MORENA)+
                  abs(out$NEMA_PNA)+
                  abs(out$NEMA_PAN)+
                  abs(out$NEMA_PRD)+
                  abs(out$NEMA_PRI)+
                  abs(out$NEMA_PT)+
                  abs(out$NEMA_PVEM)

out$PNEMA_NEW <- 1/10*(out$NEMA_CI1+
                         out$NEMA_PES+
                         out$NEMA_MC+
                         out$NEMA_MORENA+
                         out$NEMA_PNA+
                         out$NEMA_PAN+
                         out$NEMA_PRD+
                         out$NEMA_PRI+
                         out$NEMA_PT+
                         out$NEMA_PVEM)

out$MNEMA_NEW <- max(out$NEMA_CI1,
                     out$NEMA_PES,
                     out$NEMA_MC,
                     out$NEMA_MORENA,
                     out$NEMA_PNA,
                     out$NEMA_PAN,
                     out$NEMA_PRD,
                     out$NEMA_PRI,
                     out$NEMA_PT,
                     out$NEMA_PVEM)


print(sprintf("Se escriben resultados para remesa %s", remesa_name))
prename<-stringr::str_replace(string=remesa_name, pattern = '\\.txt', replacement = '')
out$REMESA<-prename
out$CUANTIL<-rownames(out)

outdir<-"./estimaciones_anidadas"

write.csv(out,file=sprintf("%s/senado2018_%s.csv", outdir,prename), row.names=FALSE) 

#Seleccion de confiabilidad
l<-1
while (c1/tm>n[l]) {l<-l+1}

#Escritura de archivos con estimaciones
out.p<-apply(lambda,2,quantile,probs=c((1-a[l])/2,0.5,(1+a[l])/2))
out.c<-apply(nd,2,quantile,probs=c((1-a[l])/2,0.5,(1+a[l])/2))
write.csv(out.p,file=paste(outdir,"/prop_",prename,".csv",sep=""))
write.csv(out.c,file=paste(outdir,"/camarasenadores_",prename,".csv",sep=""))

#print("FIN")
#print(l)
#print(n[l])
