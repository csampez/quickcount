## Script para reporte de resultados

library(tidyverse)
#library(kable)
library(kableExtra)
setwd("~/Desktop/QuickCount/ConteoRapido2024/Senadores/")

rem_list <- dir('./estimaciones/')
rem_list <- rem_list[str_detect(pattern="senado2018",string=rem_list)]
#rem_df<-paste('estimaciones/senado2018', rem_list, sep='') %>% map(read_csv) %>% reduce(rbind)
#rem_df<-paste('estimaciones/', rem_list, sep='') %>% map(read_csv) %>% reduce(rbind)

rem_df <- read.csv('reporte_diseno2018/tables/resumen_estimaciones.csv')

err_df<-rem_df %>%
       gather(key = 'partido',
              value='value',
              -REMESA, -CUANTIL) %>%
      spread(key='CUANTIL',
             value='value') %>%
      mutate(error=(`97.5%`-`2.5%`)/2) %>%
      separate(REMESA, sep = '_', into=c('rem', 'n_muestra', 'id')) %>%
  select(-rem, -id) 
  # %>%
  # mutate(partido=case_when(partido=='CI1'~'IND',
  #                          partido=='PNA'~'NVAL',
  #                          partido=='MORENA'~'MOR',
  #                          TRUE ~ partido))

theme_set(theme_minimal())

#theme_set(theme_())

## ##TODO 
## get_plot()

### NEMA PLOT

err_df <- err_df %>% mutate(n_muestra = factor(n_muestra, levels=c("320","640","1280","1920","2560","3200","3840"))) %>% filter(n_muestra %in% c("640","1280","1920","2560","3200"))

err_aux <- err_df  %>% filter(partido=='NEMA_NEW') %>% mutate(`50%` = round(`50%`))

err_aux %>% group_by(n_muestra) %>%
            summarise( `$q_{0.025}$`=quantile(`50%`,.025),
                       `$q_{0.05}$`=quantile(`50%`,.05),
                       `$q_{0.25}$`=quantile(`50%`,.25),
                       `$q_{0.5}$`=quantile(`50%`,.5),
                       `$q_{0.75}$`=quantile(`50%`,.75),
                       `$q_{0.95}$`=quantile(`50%`,.95),
                       `$q_{0.975}$`=quantile(`50%`,.975)) %>% 
            kable(format='latex', digits=3, align = 'c', escape = FALSE) %>%
            writeLines('reporte_diseno2018/tables/cuantiles_pema.txt')


p1<-ggplot(data = err_aux, aes(x=`50%`, fill=n_muestra) )+
  geom_histogram(alpha=0.6, position = 'identity', breaks=c(0:max(err_aux$`50%`))-0.5, aes(y = ..density..) )+ 
  xlab('NEMA') +
  ylab('Masa de Probabilidad') +
  facet_wrap(~n_muestra, ncol=1) + 
  #guides(fill="none") + 
  ggtitle('Distribución Número de Escaños Mal Asignados') +theme(plot.title = element_text(hjust=0.5))

ggsave(filename = 'reporte_diseno2018/figs/PEMA_dist.pdf',
       plot = p1,
       #device = cairo_pdf,
       width = 240,
       height = 180, 
       units = "mm")

ggsave(filename = 'reporte_diseno2018/figs/PEMA_dist.png',
       plot = p1,
       #device = cairo_pdf,
       width = 240,
       height = 180, 
       units = "mm")


#### SPRINGER PLOT ####
# 
# p1<-ggplot(data = err_aux, aes(x=`50%`, fill=n_muestra) )+
#   geom_histogram(alpha=0.6, position = 'identity', breaks=c(0:max(err_aux$`50%`))-0.5, aes(y = ..density..) )+ 
#   xlab('NEMA') +
#   ylab('Masa de Probabilidad') +
#   facet_wrap(~n_muestra, ncol=1) + 
#guides(fill=FALSE) + 
#   ggtitle('Distribución Número de Escaños Mal Asignados') +theme(plot.title = element_text(hjust=0.5))
# 

## PEMA PLOT

err_aux<-err_df  %>% filter(partido=='NEMA_NEW') %>% mutate(`50%` = round(`50%`)/11)

err_aux %>% group_by(n_muestra) %>%
  summarise( `$q_{0.025}$`=quantile(`50%`,.025),
             `$q_{0.05}$`=quantile(`50%`,.05),
             `$q_{0.25}$`=quantile(`50%`,.25),
             `$q_{0.5}$`=quantile(`50%`,.5),
             `$q_{0.75}$`=quantile(`50%`,.75),
             `$q_{0.95}$`=quantile(`50%`,.95),
             `$q_{0.975}$`=quantile(`50%`,.975)) %>% kable(format='latex', digits=3, align = 'c', escape = FALSE) %>%
  writeLines('reporte_diseno2018/tables/cuantiles_pema.txt')

p1<-ggplot(data = err_aux, aes(x=`50%`, fill=n_muestra) )+
  geom_histogram(alpha=0.6, position = 'identity', breaks=c(0:max(err_aux$`50%`))-0.5, aes(y = ..density..) )+ 
  xlab('PEMA') +
  ylab('Masa de Probabilidad') +
  stat_bin(geom="text",
           aes(label=paste(round(..density..,2)*100,'%',sep=''), y=1.1*..density..), 
           breaks=c(0:max(err_aux$`50%`))-0.5, colour="black",  size = 4)+  #theme(strip.text.x = element_text(size = 12, face='bold'))+
  facet_wrap(n_muestra~., ncol=1) + 
  #guides(fill=FALSE, size=FALSE) +
  ggtitle('Distribución Promedio de Escaños Mal Asignados') +theme(plot.title = element_text(hjust=0.5))

ggsave(filename = 'reporte_diseno2018/figs/PEMA_dist.pdf',
       plot = p1,
       #device = cairo_pdf,
       width = 320,
       height = 200, 
       units = "mm")

ggsave(filename = 'reporte_diseno2018/figs/PEMA_dist.png',
       plot = p1,
       #device = cairo_pdf,
       width = 320,
       height = 200, 
       units = "mm")

#### SPRINGER PLOT ####

theme_set(theme_bw())



p1<-ggplot(data = err_aux, aes(x=`50%`, fill=n_muestra) )+
  geom_histogram(position = 'identity', breaks=c(0:max(err_aux$`50%`))-0.5, aes(y = ..density..) )+ 
  xlab('ANMS') +
  ylab('Probability Mass') +
  # stat_bin(geom="text",
  #          aes(label=paste(round(..density..,2)*100,'%',sep=''),
  #              y=1.1*..density..), 
  #          breaks=c(0:max(err_aux$`50%`))-0.5, colour="black",  size = 4)+  #theme()+
  facet_wrap(n_muestra~., ncol=1) + 
  #guides(fill=FALSE, size=FALSE) + 
  ggtitle('Average Number of Misallocated Seats Distribution') +
  theme(plot.title = element_text(size=10, hjust=0.5),
        strip.text.x = element_text(size = 8
                                  #, face='bold'
                                  ))

ggsave(filename = 'reporte_diseno2018/figs_springer/PEMA_dist.eps',
       plot = p1,
       #device = cairo_pdf,
       dpi=1200,
       width = 320,
       height = 200, 
       units = "mm")


#########################################################

### PNEMA PLOT
# err_aux<-err_df  %>% filter(partido=='PNEMA_NEW') #%>% mutate(`50%` = round(`50%`))
# 
# err_aux %>% group_by(n_muestra) %>%
#   summarise( `$q_{0.025}$`=quantile(`50%`,.025),
#              `$q_{0.05}$`=quantile(`50%`,.05),
#              `$q_{0.25}$`=quantile(`50%`,.25),
#              `$q_{0.5}$`=quantile(`50%`,.5),
#              `$q_{0.75}$`=quantile(`50%`,.75),
#              `$q_{0.95}$`=quantile(`50%`,.95),
#              `$q_{0.975}$`=quantile(`50%`,.975)) %>% kable(format='latex', digits=3, align = 'c', escape = FALSE) %>%
#   writeLines('reporte_diseno2018/tables/cuantiles_pema.txt')
# 
# p1<-ggplot(data = err_aux, aes(x=`50%`, fill=n_muestra) )+
#   geom_histogram(alpha=0.6, position = 'identity', bins=10, aes(y = ..density..) )+ 
#   xlab('Mediana PNEMA') + ylab('Masa de Probabilidad') +
#   facet_wrap(~n_muestra, ncol=1) + 
#guides(fill=FALSE) + ggtitle('Distribución Promedio de Escaños Mal Asignados') +theme(plot.title = element_text(hjust=0.5))
# 
# ggsave(filename = 'reporte_diseno2018/figs/PEMA_dist.pdf',
#        plot = p1,
#        #device = cairo_pdf,
#        width = 240,
#        height = 180, 
#        units = "mm")

### PNEMA PLOT
err_aux<-err_df  %>% filter(partido=='MNEMA_NEW') %>% mutate(`50%` = round(`50%`))

err_aux %>% group_by(n_muestra) %>%
  summarise( `$q_{0.025}$`=quantile(`50%`,.025),
             `$q_{0.05}$`=quantile(`50%`,.05),
             `$q_{0.25}$`=quantile(`50%`,.25),
             `$q_{0.5}$`=quantile(`50%`,.5),
             `$q_{0.75}$`=quantile(`50%`,.75),
             `$q_{0.95}$`=quantile(`50%`,.95),
             `$q_{0.975}$`=quantile(`50%`,.975)) %>% kable(format='latex', digits=3, align = 'c', escape = FALSE) %>%
  writeLines('reporte_diseno2018/tables/cuantiles_mnema.txt')

p1<-ggplot(data = err_aux, aes(x=`50%`, fill=n_muestra) )+
  geom_histogram(alpha=0.6, position = 'identity', breaks=c(0:max(err_aux$`50%`))-0.5, aes(y = ..density..) )+ 
  xlab('MEMA') +
  ylab('Masa de Probabilidad') +
  facet_wrap(~n_muestra, ncol=1) + 
  #guides(fill=FALSE) +
  ggtitle('Distribución Máximo de Escaños Mal Asignados') +theme(plot.title = element_text(hjust=0.5))

ggsave(filename = 'reporte_diseno2018/figs/MEMA_dist.pdf',
       plot = p1,
       #device = cairo_pdf,
       width = 240,
       height = 180, 
       units = "mm")

ggsave(filename = 'reporte_diseno2018/figs/MEMA_dist.png',
       plot = p1,
       #device = cairo_pdf,
       width = 240,
       height = 180, 
       units = "mm")

####### SPRINGER ####

p1<-ggplot(data = err_aux, aes(x=`50%`, fill=n_muestra) )+
  geom_histogram(position = 'identity', breaks=c(0:max(err_aux$`50%`))-0.5, aes(y = ..density..) )+ 
  xlab('MNMS') +
  ylab('Probability Mass') +
  facet_wrap(~n_muestra, ncol=1) + 
  #guides(fill=FALSE) +
  ggtitle('Maximum Number of Misallocated Seats') +
  theme(plot.title = element_text(size=10, hjust=0.5),
        strip.text.x = element_text(size = 8))

ggsave(filename = 'reporte_diseno2018/figs_springer/MEMA_dist.eps',
       plot = p1,
       #device = cairo_pdf,
       dpi=1200,
       width = 320,
       height = 200, 
       units = "mm")

## NEMA_j PLOTS

library(stringr)

dfnema<-err_df  %>% filter(str_detect(partido,'NEMA_')) %>% filter(!partido %in% c('NEMA_NEW','PNEMA_NEW','MNEMA_NEW')) %>%
  mutate(partido=str_replace_all(partido, 'NEMA_','')) 
         # partido=case_when(partido=='CAND_IND'~'IND',
         #           partido=='NVA_ALIANZA'~'NVAL',
         #           partido=='MORENA'~'MOR',
         #           TRUE ~ partido))

p2<-ggplot(data = dfnema , aes(x=`50%`, fill=partido) )+
  geom_histogram(alpha=0.6, position = 'identity',  breaks=c(0:max(dfnema$`50%`))-0.5)+ geom_vline(xintercept = 0, linetype='dashed')+
  xlab('NEMA_j') +
  ylab('Conteo') +
  facet_grid(partido~n_muestra, scales = 'free') + 
  #guides(fill=FALSE) +
  ggtitle('Distribución NEMA por Partido') +theme(plot.title = element_text(hjust=0.5))

ggsave(filename = 'reporte_diseno2018/figs/NEMA_dist_partido.pdf',
       plot = p2,
       #device = cairo_pdf,
       width = 240,
       height = 320, 
       units = "mm")

ggsave(filename = 'reporte_diseno2018/figs/NEMA_dist_partido.png',
       plot = p2,
       #device = cairo_pdf,
       width = 240,
       height = 320, 
       units = "mm")


#### SPRINGER PLOT ####

p2<-ggplot(data = dfnema , aes(x=`50%`, fill=partido) )+
  geom_histogram(position = 'identity',  breaks=c(0:max(dfnema$`50%`))-0.5)+ geom_vline(xintercept = 0, linetype='dashed')+
  xlab('NMS_j') +
  ylab('Count') +
  facet_grid(partido~n_muestra, scales = 'free') + 
  
  #guides(fill=FALSE) + 
  ggtitle('Number of Misallocated Seats per Party') +
  theme(plot.title = element_text(hjust=0.5, size=10),
        strip.text.x = element_text(size = 8))

ggsave(filename = 'reporte_diseno2018/figs_springer/NEMA_dist_partido.eps',
       plot = p2,
       #device = cairo_pdf,
       dpi=1200,
       width = 320,
       height = 200, 
       units = "mm")

#ggplot( data=err_df, aes(x=error, fill=n_muestra))+geom_histogram(alpha=0.2, position = 'identity', bins=10) + facet_wrap(.~partido, scales = 'free')

# ggplot(data=err_df, 
#        aes(x=n_muestra,   
#            y=error,
#            fill=n_muestra))+
#   geom_violin() + 
#   stat_summary(fun = mean,
#                fun.min = function(x) quantile(x,0.025), 
#                fun.max = function(x) quantile(x,0.975), 
#                geom = "pointrange", aes(group=partido)) +
#   stat_summary(fun = mean,
#                geom = "line", aes(group=partido))+
#   facet_wrap(.~partido, scales = 'free_y') 
# 
# 
# ggplot(data=err_df, 
#        aes(x=n_muestra, 
#            y=error,
#            fill=n_muestra))+
#   geom_violin() + 
#   stat_summary(fun = mean,
#                fun.min = function(x) quantile(x,0.025),
#                fun.max = function(x) quantile(x,0.975),
#                geom = "pointrange", aes(group=partido)) +
#   stat_summary(fun = mean,
#                geom = "line", aes(group=partido))+
#   facet_wrap(.~partido, scales = 'free_y') 


pj<-ggplot(data=err_df, 
       aes(x=n_muestra, 
           y=error,
           colour=n_muestra))+
  geom_jitter() + 
  stat_summary(fun = mean,
               fun.min = function(x) quantile(x,0.025),
               fun.max = function(x) quantile(x,0.975),
               geom = "pointrange", aes(group=partido), colour='black') +
  stat_summary(fun = mean,
               geom = "line", aes(group=partido), colour='black')+
  facet_wrap(.~partido, scales = 'free_y') 

pv<-ggplot(data=err_df %>% filter(!partido %in% c("MNEMA", "PNEMA", "MNEMA_NEW")), 
       aes(x=n_muestra, 
           y=error,
           colour=n_muestra))+
  geom_violin() + 
  stat_summary(fun = mean,
               fun.min = function(x) quantile(x,0.025),
               fun.max = function(x) quantile(x,0.975),
               geom = "pointrange", aes(group=partido), colour='black') +
  stat_summary(fun = mean,
               geom = "line", aes(group=partido), colour='black')+
  facet_wrap(.~partido, scales = 'free_y') + theme(axis.text.x=element_text(angle=90)) +labs(y="PEMA", x=NULL)

ggsave(filename = 'reporte_diseno2018/figs/violin_plot.pdf',
       plot = pv,
       device = cairo_pdf,
       width = 397,
       height = 250, 
       units = "mm")

ggsave(filename = 'reporte_diseno2018/figs/jitter_plot.pdf',
       plot = pj,
       device = cairo_pdf,
       width = 397,
       height = 250, 
       units = "mm")

#ggplot( data=err_df, aes(x=error, fill=n_muestra))+geom_histogram(alpha=0.6, position = 'identity', bins=10) + facet_wrap(.~partido, scales = 'free')

ggplot( data=err_df, aes(x=error, fill=partido))+geom_density(alpha=0.6, position = 'identity') + facet_wrap(.~partido, scales = 'free')

#ggplot( data=err_df, aes(x=error, fill=partido))+geom_density(alpha=0.2, position = 'identity') #+ facet_wrap(.~partido, scales = 'free')
