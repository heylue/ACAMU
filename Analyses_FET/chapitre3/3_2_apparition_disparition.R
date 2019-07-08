################################
## Apparition et disparition d'OH
## Traitements pour l'analyse du rythme du changement, chapitre 3
## ACAMU : ANALYSES_TF
## L. Nahassia, 2019
## Zenodo. https://doi.org/10.5281/zenodo.3256673
###############################

# # données : ToToPI_V3/OH_attributs
# # espace ? tous les OH même celles sans géométrie
# # fonctions ? V_USAGE 11 & >= 70)
# # temps ? -25 > 2015
# # autre : sans OH ajoutés par LN
# # compte : 1312 obs


library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggthemes)
source("3_themes_legendes_global.R")

#------------------------------------------ PREPARATION DONNEES ----

#Import sur selection temporelle et fonctionnelle
src <- src_sqlite("../ToToPI_V3.gpkg")
OH_source <- tbl(src, "OH_attributs") %>% 
  as_tibble() %>% 
  filter(V_USAGE != 11 & V_USAGE < 70 & DATE_FIN > -25 & (is.na(REFERENCE) | REFERENCE!="Nahassia 2019"))
OH <- OH_source


#mise en forme variables
OH$PORTEE <- as.factor(OH$PORTEE)
OH$v_urb <- cut (OH$V_USAGE,
                 breaks=c(0,20,30,40,50,60,70),
                 labels=c(1,2,3,4,5,6),
                 right=FALSE,
                 include.lowest = TRUE) %>% as.factor()


#------------------------------------------ CONSTRUCTION TABLEAUX ----

#---- nb fonction ----
#(idem 6_1_répartition élémentaire)
nb_vurb_an <- as.data.frame(matrix(nrow=max(OH$DATE_FIN)+26,ncol=6))
colnames(nb_vurb_an) <-c(1:6)
nb_vurb_an$annee <- as.numeric(rownames(nb_vurb_an))-26
for (n in 1:6){ 
  for (i in 1:(nrow(nb_vurb_an)+1)) {
    annee <- i-26
    nb_vurb_an[i,n] <- sum (OH$v_urb==n & OH$DATE_DEB<=annee & OH$DATE_FIN>=annee)
  }
}
nb_vurb_an <- nb_vurb_an %>%  filter(annee != 0)
nb_vurb_an$somme <- rowSums(nb_vurb_an)-nb_vurb_an$annee
m_vurb_an <- nb_vurb_an %>% select(-somme) %>% melt(id.vars="annee") %>% rename("v_urb"=variable)


#---- nb PORTEE----
#(idem 6_1_répartition élémentaire)
nb_PORTEE_an <- as.data.frame(matrix(nrow=max(OH$DATE_FIN)+26,ncol=4))
colnames(nb_PORTEE_an) <-c(1:4)
nb_PORTEE_an$annee <- as.numeric(rownames(nb_PORTEE_an))-26
for (n in 1:4){
  for (i in 1:(nrow(nb_PORTEE_an+1))) {
    annee <- i-26
    nb_PORTEE_an[i,n] <- sum (OH$PORTEE==n & OH$DATE_DEB<=annee & OH$DATE_FIN>=annee)
  } 
}
nb_PORTEE_an <- nb_PORTEE_an %>%  filter(annee != 0)
nb_PORTEE_an$somme <- rowSums(nb_PORTEE_an)-nb_PORTEE_an$annee
m_portee_an <- nb_PORTEE_an %>% select(-somme) %>% melt(id.vars="annee") %>% rename("PORTEE"=variable)

#---- apparition par vurb ----
apparition_vurb <- OH %>% plyr::count(c("DATE_DEB","v_urb")) %>% filter(DATE_DEB >= -25)
disparition_vurb<- OH %>% plyr::count(c("DATE_FIN","v_urb"))%>% filter(DATE_FIN >= -25)

#---- apparition par portee ----
apparition_portee <- OH %>% plyr::count(c("DATE_DEB","PORTEE")) %>% filter(DATE_DEB >= -25)
disparition_portee <- OH %>% plyr::count(c("DATE_FIN","PORTEE"))%>% filter(DATE_FIN >= -25)

#---- apparition par vusage ----
apparition_vusage <- OH %>% plyr::count(c("DATE_DEB","V_USAGE")) %>% filter(DATE_DEB >= -25)%>% mutate(v_urb=substr(V_USAGE,1,1))
disparition_vusage <- OH %>% plyr::count(c("DATE_FIN","V_USAGE"))%>% filter(DATE_FIN >= -25)%>% mutate(v_urb=substr(V_USAGE,1,1))


#------------------------------------------ PLOTS ----

labels_portees <- c("1"="1.portée courte",
                    "2"="2.portée moyenne",
                    "3"="3.grande portée",
                    "4"="4.portée exceptionnelle")

labels_vurb=c("1"="1.aménagements",
              "2"="2.structures défensives et militaires",
              "3"="3.constructions civiles",
              "4"="4.édifices regifieux",
              "5"="5.lieux d'inhumation",
              "6"="6.lieux de commerce, artisanat, production")



#---- plot vurb -----
ggplot()+
  geom_line(data = m_vurb_an,
            aes(x=annee, y=value),
            color="grey60")+
  geom_col(data=apparition_vurb,
           aes(x=DATE_DEB, y=freq,fill=v_urb),
           width=15)+
  geom_col(data=disparition_vurb,
           aes(x=DATE_FIN, y=-freq, fill=v_urb),
           alpha=0.5,
           width=15)+
  scale_fill_manual(values=colors_vu)+
  facet_wrap(~v_urb, ncol=1, scales="free_y",
             labeller = as_labeller(labels_vurb))+
  labs(x="année",
       y="nombre d'OH",
       title="Rythme d'apparition et de disparition des OH par valeurs urbaines",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT"
  )+
  theme_fivethirtyeight()+
  scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
  theme_ln()+
  theme(strip.text=element_text(hjust = 0))

#---- plot portee -----
ggplot()+
  geom_line(data = m_portee_an,
            aes(x=annee, y=value),
            color="grey60")+
  geom_col(data=apparition_portee,
           aes(x=DATE_DEB, y=freq,fill=PORTEE),
           width=15)+
  geom_col(data=disparition_portee,
           aes(x=DATE_FIN, y=-freq, fill=PORTEE),
           alpha=0.5,
           width=15)+
  scale_fill_manual(values=colors_p)+
  facet_wrap(~PORTEE, ncol=1, scales="free_y",
             labeller = as_labeller(labels_portees))+
  labs(x="année",
       y="nombre d'OH",
       title="Rythme d'apparition et de disparition des OH par portées",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT"
  )+
  theme_fivethirtyeight()+
  scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
  theme_ln()+
  theme(strip.text=element_text(hjust = 0))

#---- plot valeurs d'usage -----
ggplot()+
  geom_col(data=apparition_vusage,
           aes(x=DATE_DEB, y=freq,fill=v_urb),
           width=15)+
  geom_col(data=disparition_vusage,
           aes(x=DATE_FIN, y=-freq, fill=v_urb),
           alpha=0.5,
           width=15)+
  scale_fill_manual(values=colors_vu)+
  facet_wrap(~V_USAGE, ncol=4, scales="free_y")+
  labs(x="année",
       y="nombre d'OH",
       title="Rythme d'apparition et de disparition des OH par valeurs d'usage",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT"
  )+
  theme_fivethirtyeight()+
  scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
  theme_ln()+
  theme(strip.text=element_text(hjust = 0),
        axis.text.x=element_text(angle = 90))

