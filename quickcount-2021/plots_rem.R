library(tidyverse)
library(ggplot2)
library(stringr)

files<-dir('z:\\nieto/pef/',  full.names=T)[dir('z:\\nieto/pef/') %>% str_detect(.,'nieto')]

data<-files %>% 
  map(read_csv) %>%
  reduce(rbind) %>% 
  select(-EN,-EQ) %>%
  mutate(LMU = paste("l",LMU,sep=""), 
         R=substr(R,3,6))

theme_set(theme_bw())
data %>% 
  pivot_longer(c(PAN:PART)) %>% 
  pivot_wider(id_cols=c(name,R), names_from=LMU) %>%
  ggplot() + geom_ribbon(aes(x=R, ymin=l0, ymax=l2, group=name, fill=name), alpha=0.3) +
             geom_line(aes(x=R, y=l1, group=name)) +
             facet_wrap(name~., scales='free') + 
             theme(axis.text.x = element_text(angle = 90))

files<-files<-dir('z:\\nieto/diputaciones_pef/',  full.names=T)[dir('z:\\nieto/diputaciones_pef/')%>%str_detect(.,'dip')]

data<-files %>% 
  map(read_csv) %>% 
  reduce(rbind) %>% 
  select(-EN,-EQ) %>%
  mutate(LMU = paste("l",LMU,sep=""),
         R=substr(R,3,6))

theme_set(theme_bw())
data %>% 
  pivot_longer(c(PAN:IND)) %>% 
  pivot_wider(id_cols=c(name,R), names_from=LMU) %>%
  ggplot() + geom_ribbon(aes(x=R, ymin=l0, ymax=l2, group=name, fill=name), alpha=0.3) +
  geom_line(aes(x=R, y=l1, group=name)) +
  geom_text(aes(x=R, y=l1, label=l1, angle=65), size=3) +
  facet_wrap(name~., scales='free') + 
  theme(axis.text.x = element_text(angle = 90))
