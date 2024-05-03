##################################
##################################
########## COMPULSADOS  ##########
##########  Diputados   ########## 
##################################
##################################

path<-"~/Desktop/quickcount/quickcount-2024/primer_simulacro/simba/"

compulsator<-function(time){

pps<-c("PAN","PRI","PRD","PT","PVEM","MC","MORENA")
path<-"~/Desktop/quickcount/quickcount-2024/primer_simulacro/simba/"

#setwd(path)

bzn_cmp_dip<-"buzon_diputados_compulsado"
#bzn_dip_nieto <-"buzon_diputados_nieto"
#bzn_dip_rdz <-"buzon_diputados_rdz"
#remesa_dir<-"diputados"
equipo_dir<-"equipo4compartida"

#get_comp<-function(time){
#time<-'1700'
id<-sprintf("040025%s",time)

info<-read.csv("informacion/Info_distritos_2024.csv")
#lnest<-info$LISTA_NOMINAL
#LNN<-sum(lnest)
#c1<-scan(paste(dir,"/REMESAS",id,".txt",sep=""),n=1)
table_remesa<-sprintf("%s/diputados/REMESAS%s.txt",path,as.character(id))

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

tryCatch(
{
  rdz.p <- read.csv(sprintf("%s/buzon_diputados_compulsado/rodriguez4025%s.csv",path,time)) 
  rdz.d <- read.csv(sprintf("%s/buzon_diputados_compulsado/rodriguezdip4025%s.csv",path,time))

  cot.p <- read.csv(sprintf("%s/buzon_diputados_compulsado/nieto4025%s.csv",path,time)) 
  cot.d <- read.csv(sprintf("%s/buzon_diputados_compulsado/nietodip4025%s.csv",path,time))

# eq.p <- read.csv(sprintf("%s/buzon_diputados_compulsado/equipdip%s.csv",path,time))
# eq.d <- read.csv(sprintf("%s/buzon_diputados_compulsado/equipdip%s.csv",path,time))

precomp<-rbind(cot.p , rdz.p) %>% select(-EQ,-R, -EN)

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

comp %>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,
                PART,LMU,ESTRATOS,EST_REC,TOT_CAS,CAS_REC,PORCENTAJE) %>%
  write.csv(sprintf('%s/buzon_diputados_compulsado/compulsado40%s.csv', path, substr(id,5,10)), row.names = F)

### COMPULSADO DIPUTACIONES

#rdz.d <- read.csv(sprintf('z:\\buzon4/rodriguezdip%s.csv',substr(id,3,10)))
precomp<-rbind(cot.d, rdz.d) %>% select(-EQ,-R, -EN)

comp <- rbind(
  precomp %>% group_by(LMU) %>% summarise_at(pps, funs(min)) %>% filter(LMU==0),
  precomp %>% group_by(LMU) %>% summarise_at(pps, funs(round(mean(.)))) %>% filter(LMU==1),
  precomp %>% group_by(LMU) %>% summarise_at(pps, funs(max)) %>% filter(LMU==2)
) %>% mutate(
  EQ='compulsado',
  EN="40",
  R = substr(id,5,10))

comp %>% select(EQ,EN,R,PAN,PRI,PRD,PT,PVEM,MC,MORENA,LMU) %>%
  write.csv(sprintf('%s/buzon_diputados_compulsado/compulsadodip40%s.csv',path, substr(id,5,10)), row.names = F)

},
error = function(cond) {
            message(paste("file does not seem to exist:", time))},
finally = message(paste("file does not seem to exist:", time)))

}

times<-dir(sprintf("%s/senadores/", path)) %>% substr(14,17) %>% sort(decreasing=TRUE) 

for (t in times){
   compulsator(t)
}

# times<-c('1406','1415','1420','1710','1715','1420','1425', )
# compulsator(times[1])



#     tryCatch(
#         {
#             # Just to highlight: if you want to use more than one
#             # R expression in the "try" part then you'll have to
#             # use curly brackets.
#             # 'tryCatch()' will return the last evaluated expression
#             # in case the "try" part was completed successfully

#             message("This is the 'try' part")

#             suppressWarnings(readLines(url))
#             # The return value of `readLines()` is the actual value
#             # that will be returned in case there is no condition
#             # (e.g. warning or error).
#         },
#         error = function(cond) {
#             message(paste("URL does not seem to exist:", url))
#             message("Here's the original error message:")
#             message(conditionMessage(cond))
#             # Choose a return value in case of error
#             NA
#         },
#         warning = function(cond) {
#             message(paste("URL caused a warning:", url))
#             message("Here's the original warning message:")
#             message(conditionMessage(cond))
#             # Choose a return value in case of warning
#             NULL
#         },
#         finally = {
#             # NOTE:
#             # Here goes everything that should be executed at the end,
#             # regardless of success or error.
#             # If you want more than one expression to be executed, then you
#             # need to wrap them in curly brackets ({...}); otherwise you could
#             # just have written 'finally = <expression>' 
#             message(paste("Processed URL:", url))
#             message("Some other message at the end")
#         }
#     )
# }