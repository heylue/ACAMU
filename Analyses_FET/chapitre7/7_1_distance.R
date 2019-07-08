###############################################
## Analyse des rapports de distance entre activités : distances entre objets
## Traitements pour l'analyse spatiale horizontale, chapitre 7
## ACAMU : ANALYSES_FET
## L. Nahassia, 2019
## Zenodo. https://doi.org/10.5281/zenodo.3256673
##############################################

# # données : ToToPI_GDB/ToToPI_v3.gpkg

#library
library(sp)
library(sf)
library(rgdal)
library("RSQLite")
library("rgeos")
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(ggpubr)
library(plotly)
library(reshape2)
library(forcats)


#------------------------------------
#-------------- IMPORT --------------
#------------------------------------

#Import OH
points <- st_read(dsn="../ToToPI_V3.gpkg", layer="oh_pt_uniques", quiet=TRUE)
lignes <- st_read(dsn="../ToToPI_V3.gpkg", layer="oh_pl_uniques", quiet=TRUE)
polygones <- st_read(dsn="../ToToPI_V3.gpkg", layer="oh_pg_uniques", quiet=TRUE)
OH_geom_base <- rbind(points,lignes,polygones)
OH_p <- st_read(dsn="../ToToPI_V3.gpkg", layer="OH_ponctuels")

#mise en forme
OH_geom_base$OH_NUM <- as.numeric(OH_geom_base$OH_NUM)
OH_geom_base  <- OH_geom_base [OH_geom_base$V_USAGE != 11
                               & OH_geom_base$V_USAGE < 70
                               & OH_geom_base$DATE_FIN > -25,]
OH_geom_base$V_URB <- cut (OH_geom_base$V_USAGE,
                           breaks=c(0,20,30,40,50,60,70),
                           labels=c(1,2,3,4,5,6),
                           right=FALSE,
                           include.lowest = TRUE)
OH_geom_base <- mutate(OH_geom_base, V_USAGE = as.factor(V_USAGE), PORTEE=as.factor(PORTEE))

#import ZST
ZST <-  st_read(dsn="../ToToPI_V3.gpkg", layer="ensembles_urbains", quiet=TRUE)




#----------------------------------------------------------------
#-------------- ANALYSE DE DISTANCE ENTRE DEUX OBJETS -----------
#----------------------------------------------------------------

##############################################################
#----1.édifices religieux par rapport aux espaces urbains----
#############################################################

#OH étudié
OH_dist_urb <- OH_geom_base
#objet de référence
zones_urbaines <- ZST %>% filter(occupation=="urbaine")

#distance au plus proche voisin
for (i in 1:nrow(OH_dist_urb)){ 
  date <- OH_dist_urb[i,]$DATE_DEB
  OH_dist_urb[i, "dist_urb"] <- st_distance(OH_dist_urb[i,],
                                            zones_urbaines %>% filter(date_debut <= date & date_fin >date)) %>% min()
}


#----1.a. activités religieuse par rapport aux espaces urbains----

OH_rel <- OH_dist_urb %>% filter(V_URB==4)

max(OH_rel$dist_urb)

#distances dans le temps
ggplot(OH_rel,aes(x=DATE_DEB, y=dist_urb))+
  geom_text_repel(
    data = OH_rel %>% filter(dist_urb > 20),
    aes(x=DATE_DEB, y=dist_urb, label=NOM),
    alpha= 0.5,
    segment.alpha=0.5,
    size=2.5,
    force=3
  )+
  geom_point(aes(color=PORTEE),
             size=3, alpha=0.8)+
  # scale_color_manual(values=colors_p,
  #                    labels=c("petite","moyenne","grande","exceptionnelle"),
  #                    name="Portée des édifices religieux")+
  scale_y_continuous(breaks = seq(0,1000,100))+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  labs(title="Distance des édifices religieux à la zone urbaine la plus proche au moment de leur apparition",
       subtitle="OH valeur urbaine 6 & ZST occupation du sol urbaine",
       y = "distance à la zone urbaine la plus proche (m)",
       x= "année d'apparition",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(legend.position="bottom",
        plot.title= element_text(size=9, face="bold"))


#distribution nombre/distance
ggplot(OH_rel,aes(x=dist_urb)) +
  geom_histogram(binwidth=10)+
  # geom_freqpoly(binwidth=10)+
  scale_x_continuous(breaks = seq(0,1000,100))+
  labs(title="Distribution des plus courtes distances entre les édifices religieux et les zones urbaines",
       y = "nombre d'OH",
       x= "distance à la zone urbaine la plus proche (m)",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(plot.title = element_text(size=8.5, face="bold"))
ggsave("distribution_distance_rel_urb.png", width=14.5, height=7, units="cm")


OH_rel$dist_urb %>% as.factor() %>% summary() %>% sum()
189/229

#----1.b.tous OH par rapport aux espaces urbains ----

OH_autre <- OH_dist_urb %>% filter(V_URB != 4, DATE_DEB >=-25)

#distances dans le temps
ggplot(OH_rel,aes(x=DATE_DEB, y=dist_urb))+
  geom_point(data=OH_autre,
             aes(x=DATE_DEB, y=dist_urb),
             color="grey", alpha=0.4)+
  geom_point(aes(color=PORTEE),
             size=3, alpha=0.8)+
  scale_color_manual(values=colors_p,
                     labels=c("petite","moyenne","grande","exceptionnelle"),
                     name="Portée des édifices religieux")+
  # scale_y_continuous(breaks = seq(0,1000,100))+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  labs(title="Distance des édifices religieux à la zone urbaine la plus proche au moment \nde leur apparition",
       y = "distance (m)",
       x= "année",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(legend.position="bottom")


########################################################################################
#----2. lieux d'inhumation par rapport aux églises (édifices cultuels catholiques) ----
########################################################################################

#OH étudié (pas les cimetières de joué car pas dans l'espace d'étude au moment de leur apparition)
OH_inhum <- OH_geom_base %>% filter(V_URB==5 &!grepl("joué", ignore.case = TRUE, NOM))
#objet de référence
OH_eglises <- OH_geom_base %>% filter(V_USAGE==42)
eglises_saisies <- st_read(dsn="../saisie_eglises_cimetieres.gpkg", layer="eglises_cimetieres", quiet=TRUE)
eglises_saisies <- eglises_saisies %>% rename(DATE_DEB=date_debut, DATE_FIN=date_fin)
eglises_ref <- rbind(OH_eglises %>% select(DATE_DEB,DATE_FIN)
                     ,eglises_saisies %>% select(DATE_DEB, DATE_FIN))


#distance au plus proche voisin
for (i in 1:nrow(OH_inhum)){ 
  date <- OH_inhum[i,]$DATE_DEB
  OH_inhum[i, "dist_egl"] <- st_distance(OH_inhum[i,],
                                         eglises_ref %>% filter(DATE_DEB <= date & DATE_FIN >date)) %>% min()
}
#si inf = pas d'église au moment de l'apparition
OH_inhum[OH_inhum$dist_egl=="Inf",]$dist_egl <- NA

#correction chapelle manquante pour Cimetiere de Saint-Jacques-de-l'Orme-Robert
OH_inhum[OH_inhum$OH_NUM==1421,]$dist_egl <- 0
#à plus ou - de 50m des églises
OH_inhum <- OH_inhum %>% mutate(loin= if_else((dist_egl>21),true="église la plus proche à plus de 20 m", false="église la plus proche à moins de 20 m"))

OH_inhum<-OH_inhum %>% mutate(loin=as.factor(loin))
  
OH_inhum %>% as_tibble() %>% filter(DATE_DEB>450 & DATE_DEB <=1800) %>% select(loin) %>% summary()
OH_inhum %>% as_tibble() %>% filter(DATE_DEB>450 & DATE_DEB <=800) %>% select(loin) %>% summary()
OH_inhum %>% as_tibble() %>% filter(DATE_DEB>800 & DATE_DEB <=1500) %>% select(loin) %>% summary()
OH_inhum %>% as_tibble() %>% filter(DATE_DEB>1500 & DATE_DEB <=1800) %>% select(loin) %>% summary()



#distance dans le temps
inhum_plot<- ggplot(OH_inhum %>% filter(!is.na(loin))
                    , aes(x=DATE_DEB, y=dist_egl))+
  geom_point(aes(color=loin),
             shape=16,
             size=3, alpha=0.9)+
  scale_color_manual(values = c("#703ab9","#a481d2"),name="")+
  scale_y_continuous(breaks = seq(0,3000,200), minor_breaks =seq(0,3000,100) )+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  labs(title="Distance des lieux d'inhumation à l'église la plus proche au moment de leur apparition",
       subtitle="OH valeur urbaine 5 & OH valeur d'usage 42", 
       y = "distance à l'église la plus proche (m)",
       x= "année d'apparition",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    legend.position="bottom",
    panel.grid.minor.y = element_line(color ="grey90"),
    plot.title = element_text(size=8.6, face="bold"))
ggsave("distance_inhum_eglise.png", inhum_plot, width=14.5, height=16, unit="cm")

#plus ou moins de 50m
ggplot(OH_inhum %>% filter(!is.na(loin)) 
       , aes(x=DATE_DEB))+
  geom_histogram(
    position="stack",
    position="fill",
    binwidth = 50,
    aes(fill=forcats::fct_rev(loin)))+
  scale_fill_manual(values=c("#a05050", "#c8ab37"),
                    name="église la plus proche")+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  scale_y_continuous(labels=scales::percent)+
  labs(title="Type d'apparition des lieux d'inhumation par périodes de 50ans",
       x="année",
       y="part des apparitions à côté ou loin des églises",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
  theme_fivethirtyeight()+
  theme_ln() +
  theme(
    legend.position="bottom",
    strip.text.y = element_text(size=8),
    axis.text=element_text(size=8),
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))



#distribution nombre/distance
ggplot(OH_inhum,aes(x=dist_egl))+
  geom_histogram(binwidth=20)+
  scale_x_continuous(breaks = seq(0,3000,500))+
  labs(title="Distribution des plus courtes distances entre les lieux d'inhumation et les églises",
       y = "nombre d'OH",
       x= "distance à l'église la plus proche (m)",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(plot.title = element_text(size=9, face="bold"))

ggsave("distribution_distance_inhum_eglise.png", width=14.5, height=7, units="cm")


#mediane boite à moustache dans le temps
ggplot(OH_inhum, aes(x=DATE_DEB, y=dist_egl))+
  geom_text(data = inhum_sans_eglise, aes(x=DATE_DEB, y=dist_egl),
            label="*",
            size=10,
            color="black"
  )+
  geom_boxplot(
    outlier.shape = NA,
    color = "#703ab9",
    fill = adjustcolor("#703ab9", alpha=0.5)) +
  scale_y_continuous(breaks = seq(0,3000,500), minor_breaks =seq(0,3000,250) )+
  # stat_summary(fun.y=mean, geom="point")
  labs( title="Distribution (médiane et quantile) des distance des lieux d'inhumation à l'église la plus proche",
        x="année d'apparition",
        y="distance à l'église la plus proche (en m)",
        caption="L. Nahassia, Géographie-cités, 2017 | Sources : ToToPI, LAT, CITERES")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(panel.grid.minor.y = element_line(color ="grey90"),
        axis.text.x=element_text(angle=90))


##############################################
#---- 4. pôles civils VS poles religieux ----
##############################################

#taille de référence de l'espace urbain (plus grand axe du polygone)
taille_urb <- data.frame(annee=seq(-25, 2015,1), taille=NA)

for(i in taille_urb$annee){
  urb <- ZST %>% filter(date_debut<=i, date_fin>=i)
  urb_points <- st_cast(
    st_union(urb), #union de tous les polygones existants
    to = "POINT", #en points
    do_split=TRUE)
  taille_urb[taille_urb$annee==i,]$taille <- max(st_distance(urb_points,urb_points)) #distance entre les points
  
}


ggplot(taille_urb, aes(x=annee, y=taille))+geom_line()

#----4.1. religeux TO civils----

#OH étudié
OH_pouvoir_rel <- OH_geom_base%>% filter(V_URB==4 & as.numeric(PORTEE)>2) 
#objet de référence
OH_ref <- OH_geom_base %>% filter(V_URB==3 & as.numeric(PORTEE)>1 & !grepl("porte", ignore.case = TRUE, NOM))

#distance au plus proche voisin
for (i in 1:nrow(OH_pouvoir_rel)){ 
  date <- OH_pouvoir_rel[i,]$DATE_DEB
  OH_pouvoir_rel[i, "dist_ref"] <- st_distance(OH_pouvoir_rel[i,],
                                               OH_ref %>% filter(DATE_DEB <= date & DATE_FIN >date)) %>% min()
}
#si inf =objet manquant au moment de l'apparition
OH_pouvoir_rel[OH_pouvoir_rel$dist_ref=="Inf",]$dist_ref <- NA

#distance dans le temps
ggplot(OH_pouvoir_rel,
       aes(x=DATE_DEB, y=dist_ref))+
  geom_text_repel(
    data = OH_pouvoir_rel %>% filter(dist_ref>300),
    aes(x=DATE_DEB, y=dist_ref, label=NOM),
    alpha= 0.5,
    segment.alpha=0.5,
    size=2.5,
    force=3,
    point.padding = 0.1
  )+
  geom_point(color="#2fc6a0",
             shape=16,
             size=3, alpha=0.9)+
  scale_y_continuous(breaks = seq(0,600,100), minor_breaks =seq(0,600,50), limits=c(0,600) )+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  labs(title="B. Distance des lieux de pouvoir religieux au lieu de pouvoir civil le plus \nproche au moment de leur apparition",
       y = "distance au lieu de pouvoir \npublic le plus proche (m)",
       x= "année d'apparition",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(panel.grid.minor.y = element_line(color ="grey90"),
        plot.title = element_text(size=9, face="bold"))

ggsave("distance_rel_pub.png", width=14.5, height=7, units="cm")


#----4.2. civils TO religieux ----

#OH étudié
OH_pouvoir_civ <- OH_geom_base %>% filter(V_URB==3 & as.numeric(PORTEE)>1 & !grepl("porte", ignore.case = TRUE, NOM))
#objet de référence
OH_ref <- OH_geom_base %>% filter(V_URB==4 & as.numeric(PORTEE)>2) 

#distance au plus proche voisin
for (i in 1:nrow(OH_pouvoir_civ)){ 
  date <- OH_pouvoir_civ[i,]$DATE_DEB
  OH_pouvoir_civ[i, "dist_ref"] <- st_distance(OH_pouvoir_civ[i,],
                                               OH_ref %>% filter(DATE_DEB <= date & DATE_FIN >date)) %>% min()
}
#si inf =objet manquant au moment de l'apparition
OH_pouvoir_civ[OH_pouvoir_civ$dist_ref=="Inf",]$dist_ref <- NA



#distance dans le temps
ggplot(OH_pouvoir_civ,
       aes(x=DATE_DEB, y=dist_ref2))+
  geom_text_repel(
    data = OH_pouvoir_civ %>% filter(dist_ref2>300),
    aes(x=DATE_DEB, y=dist_ref, label=NOM),
    alpha= 0.5,
    segment.alpha=0.5,
    size=2.5,
    point.padding=0.2
  )+
  geom_point(color="#f6b01a",
             shape=16,
             size=3, alpha=0.9)+
  # scale_y_continuous(breaks = seq(0,600,100), minor_breaks =seq(0,600,50), limits=c(0,600) )+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  labs(title="A. Distance des lieux de pouvoir civils au lieu de pouvoir religieux le plus \nproche au moment de leur apparition",
       y = "distance au lieu de pouvoir \nreligieux le plus proche (m)",
       x= "année d'apparition",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(panel.grid.minor.y = element_line(color ="grey90"),
        plot.title = element_text(size=9, face="bold"))

ggsave("distance_rel_civ.png", width=14.5, height=7, units="cm")

#----4.2. tous ensembles ----

#OH étudié
OH_pouvoir <- OH_geom_base %>% filter(
  (V_URB==3 & as.numeric(PORTEE)>1 & !grepl("porte", ignore.case = TRUE, NOM)) | (V_URB==4 & as.numeric(PORTEE)>2))
#objet de référence
OH_ref <- OH_pouvoir

#distance au plus proche voisin
for (i in 1:nrow(OH_pouvoir)){ 
  date <- OH_pouvoir[i,]$DATE_DEB
  OH_pouvoir[i, "dist_ref"] <- st_distance(OH_pouvoir[i,],
                                           OH_ref %>% filter(DATE_DEB <= date & DATE_FIN >date & OH_NUM != OH_pouvoir[i,]$OH_NUM)) %>% min()
}
#si inf =objet manquant au moment de l'apparition
OH_pouvoir[OH_pouvoir$dist_ref=="Inf",]$dist_ref <- NA

#distance dans le temps
ggplot(OH_pouvoir,
       aes(x=DATE_DEB, y=dist_ref))+
  geom_text_repel(
    aes(label=NOM),
    alpha= 0.5,
    segment.alpha=0.5,
    size=2.5,
    point.padding=0.2
  )+
  geom_point(aes(color=V_URB),
             shape=16,
             size=3, alpha=0.9)+
  # scale_y_continuous(breaks = seq(0,600,100), minor_breaks =seq(0,600,50), limits=c(0,600) )+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  labs(title="A. Distance des lieux de pouvoir civils au lieu de pouvoir religieux le plus \nproche au moment de leur apparition",
       y = "distance au lieu de pouvoir \nreligieux le plus proche (m)",
       x= "année d'apparition",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(panel.grid.minor.y = element_line(color ="grey90"),
        plot.title = element_text(size=9, face="bold"))

ggsave("distance_rel_civ.png", width=14.5, height=7, units="cm")

#---- 4.3. Distances relatives ----

OH_pouvoir_civ <- left_join(OH_pouvoir_civ, taille_urb, by=c("DATE_DEB"="annee"))
OH_pouvoir_civ$dist_ref2 <- OH_pouvoir_civ$dist_ref/OH_pouvoir_civ$taille
OH_pouvoir_rel<- left_join(OH_pouvoir_rel, taille_urb, by=c("DATE_DEB"="annee"))
OH_pouvoir_rel$dist_ref2 <- OH_pouvoir_rel$dist_ref/OH_pouvoir_rel$taille

  plot_taille <- ggplot(taille_urb, aes(x=annee, y=taille/1000))+
  geom_line()+
  scale_y_continuous(breaks=seq(0,20,5), minor_breaks = seq(0,20,1), limits=c(0,17))+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015), limits=c(-25,2015))+
  labs(title="A.Taille de l'espace urbain tourangeau au cours du temps",
       subtitle="plus grand axe de la zone constituée par les ZST d'occupation urbaine",
       y = "taille du plus grand axe (km)",
       x= "année")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    panel.grid.minor.y = element_line(color ="grey90"),
    plot.title = element_text(size=8.5, face="bold"),
    axis.text.x=element_text(size=7, angle = 60, hjust=0.8))

plot_civ <- ggplot(OH_pouvoir_civ,
                   aes(x=DATE_DEB, y=dist_ref2))+
  geom_point(color="#f6b01a",
             shape=16,
             size=3, alpha=0.9)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,0.5))+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015), limits=c(-25,2015))+
  labs(title="B. Distance relative des lieux de pouvoir civils au lieu de pouvoir religieux le plus \nproche au moment de leur apparition",
       y = "distance relative à la taille de \nl'espace urbain au cours du temps",
       x= "année d'apparition")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(panel.grid.minor.y = element_line(color ="grey90"),
        plot.title = element_text(size=8, face="bold"),
        axis.text.x=element_text(size=7, angle = 60, hjust=0.8))

plot_rel <- ggplot(OH_pouvoir_rel,
                   aes(x=DATE_DEB, y=dist_ref2))+
  geom_point(color="#2fc6a0",
             shape=16,
             size=3, alpha=0.9)+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,0.5))+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015), limits=c(-25,2015))+
  labs(title="C. Distance relative des lieux de pouvoir religieux au lieu de pouvoir civil le plus \nproche au moment de leur apparition",
       y = "distance relative à la taille de \nl'espace urbain au cours du temps",
       x= "année d'apparition")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(panel.grid.minor.y = element_line(color ="grey90"),
        plot.title = element_text(size=8, face="bold"),
        axis.text.x=element_text(size=7, angle = 60, hjust=0.8))


planche_dist <- arrangeGrob(
  plot_taille,plot_civ,plot_rel
) %>% as_ggplot()

ggsave("planche_dist.png", planche_dist, width=14.5, height=20, units="cm")





#---- les deux sur le même graphique----
#distance dans le temps
ggplot(bind_rows( OH_pouvoir_civ %>% as_tibble(), OH_pouvoir_rel %>% as_tibble()),
       aes(x=DATE_DEB, y=dist_ref))+
  geom_point(aes(color=V_URB),
             shape=16,
             size=3, alpha=0.9)+
  scale_y_continuous(breaks = seq(0,600,100), minor_breaks =seq(0,600,50), limits=c(0,600) )+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  # facet_grid(V_USAGE ~.)+
  labs(title="Distance des lieux de pouvoir covoms au lieu de pouvoir religieux le plus \nproche au moment de leur apparition",
       y = "distance au lieu de pouvoir \nreligieux le plus proche (m)",
       x= "année d'apparition",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(panel.grid.minor.y = element_line(color ="grey90"),
        plot.title = element_text(size=9, face="bold"))

ggsave("distance_rel_civ.png", width=14.5, height=7, units="cm")



###########################
#---- 5. explorations ----
###########################


#OH étudié
OH_etude <- OH_geom_base %>% filter(V_USAGE==16 & PORTEE==1) #à changer selon l'objet étudié
#objet de référence
OH_ref <- OH_geom_base %>% filter(V_USAGE==37) #à changer selon l'objet de référence
# ZST %>% filter(occupation=="urbaine") %>% mutate(DATE_DEB=date_debut, DATE_FIN=date_fin)


#distance au plus proche voisin
for (i in 1:nrow(OH_etude)){ 
  date <- OH_etude[i,]$DATE_DEB
  OH_etude[i, "dist_ref"] <- st_distance(OH_etude[i,],
                                         OH_ref %>% filter(DATE_DEB <= date & DATE_FIN >date)) %>% min()
}
#si inf =objet manquant au moment de l'apparition
OH_etude[OH_etude$dist_ref=="Inf",]$dist_ref <- NA

#distribution dans le temps
ggplot(OH_etude,
       aes(x=DATE_DEB, y=dist_ref))+
  # geom_text_repel(
  #   aes(label=NOM),
  #   alpha= 0.5,
  #   segment.alpha=0.5,
  #   size=2.5,
  #   point.padding	=0.1
  # )+
  geom_point(color="#e72535",
             shape=16,
             size=3, alpha=0.9)+
  # scale_y_continuous(breaks = seq(0,1000,200), minor_breaks =seq(0,1000,100) )+
  scale_y_continuous(limits=c(0,47) )+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  # facet_grid(V_USAGE ~.)+
  labs(title="A.Distance des fontaines aux habitations les plus \nproches au moment de leur apparition",
       subtitle="fontaines : V_USAGE 16 et portée 1 & habitat : V_USAGE 37",
       y = "distance (m)",
       x= "année",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(panel.grid.minor.y = element_line(color ="grey90"))

ggsave("distance_OH_explo.png")

#distribution nombre/distance
ggplot(OH_etude,aes(x=dist_ref)) +
  geom_histogram(binwidth=5)+
  scale_x_continuous(breaks = seq(0,3000,5))+
  labs(title="B.Distribution de la distance entre les fontaines et \nl'habitation la plus proche",
       y = "nombre d'OH",
       x= "distance (m)",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()
ggsave("distribution_distance_OH_explo.png",
       width=14.5, height=6, units="cm")





#---- pour faire apparaître les OH A quand B n'existe pas :----

inhum_sans_eglise <- OH_inhum %>% filter(is.na(dist_egl))  %>% mutate(dist_egl=3000)

geom_point(data = inhum_sans_eglise %>% mutate(bla="pas d'église au moment de l'apparition"), #inhum sans églises
           aes(x=DATE_DEB, y=dist_egl, shape=bla),
           size=10,color="black")+ 
  scale_shape_manual(values="*",name="")+
  theme(
    legend.position = "top",
    legend.justification = c(0,0),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.margin = margin(-10,-10,-10,-10))


#---- graphique moyenne/ecart-type ----

dist_moyenne <- OH_inhum %>% 
  group_by(DATE_DEB) %>% 
  summarise(distance_moyenne=mean(dist_egl), ecart_type=sd(dist_egl))


#moyenne et ecart-type
ggplot(dist_moyenne, aes(x=DATE_DEB, y=distance_moyenne)) + 
  geom_errorbar(aes(ymin=distance_moyenne-ecart_type, ymax=distance_moyenne+ecart_type),
                color="grey60")+
  geom_point(color="#703ab9")+
  labs( title="Distance moyenne des lieux d'inhumation à l'église la plus proche",
        x="année d'apparition",
        y="distance moyenne (en m)",
        caption="L. Nahassia, Géographie-cités, 2017 | Sources : ToToPI, LAT, CITERES")+
  theme_fivethirtyeight()+
  theme_ln()

