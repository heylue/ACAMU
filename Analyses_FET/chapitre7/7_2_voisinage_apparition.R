################################
## Analyse de la localisation de OH les unes par rapport aux autres :
## identification et analyse d'apparition dans un voisinage
## L. Nahassia, 2019
###############################

# # données : ToToPI_GDB/ToToPI_v3.gpkg

#library
library(sp)
library(sf)
library(rgdal)
library("RSQLite")
library("rgeos")
library(tidyverse)
library(leaflet)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(plotly)
library(RColorBrewer)
library("wesanderson")
library(reshape2)
library(scales)
library(fmsb)


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



#nombre d'année ou une seule apparition par catégorie
OH_geom_base %>% filter(V_URB==1) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()
OH_geom_base %>% filter(V_URB==2) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()
OH_geom_base %>% filter(V_URB==3) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()
OH_geom_base %>% filter(V_URB==4) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()
OH_geom_base %>% filter(V_URB==5) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()
OH_geom_base %>% filter(V_URB==6) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()
OH_geom_base %>% filter(PORTEE==1) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()
OH_geom_base %>% filter(PORTEE==2) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()
OH_geom_base %>% filter(PORTEE==3) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()
OH_geom_base %>% filter(PORTEE==4) %>% as.tibble() %>%  select(DATE_DEB) %>% table () %>% as.tibble %>% filter(n==1) %>% nrow()


#------------------------------------------------------------------------
#-------------- TAILLE DU VOISINAGE : DIST AU PLUS PROCHE VOISIN -----------
#------------------------------------------------------------------------
#OH 
OH_plusprochevoisin <- OH_geom_base %>% filter(REFERENCE!="Nahassia 2019" | is.na(REFERENCE))
#distance au plus proche voisin
for (i in OH_plusprochevoisin$OH_NUM){
  print(i)
  date <- OH_plusprochevoisin[OH_plusprochevoisin$OH_NUM==i,]$DATE_DEB
  OH_existants <- OH_plusprochevoisin %>% filter(DATE_DEB <= date & DATE_FIN >date & OH_NUM != i)
  OH_plusprochevoisin[OH_plusprochevoisin$OH_NUM==i, "dist_plusprochevoisin"] <- st_distance(OH_plusprochevoisin[OH_plusprochevoisin$OH_NUM==i,],OH_existants) %>% min()
}

OH_plusprochevoisin[OH_plusprochevoisin$dist_plusprochevoisin=="Inf",]$dist_plusprochevoisin <- NA
OH_plusprochevoisin <- OH_plusprochevoisin %>% filter(V_USAGE!=21) #enlever les enceintes construites 'en continu'


summary(OH_plusprochevoisin$dist_plusprochevoisin)

#distribution des distances au plus proche voisin
ggplot(OH_plusprochevoisin,
       aes(x=dist_plusprochevoisin))+
  geom_histogram(binwidth=10,
                 aes(y = (..count..)/sum(..count..)),
                 alpha=0.8)+
  labs(title="Distances des OH à leur plus proche voisin toutes périodes confondues",
       y ="% d'OH",
       x=  "distance au plus proche voisin (m)",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  scale_x_continuous(expand = c(0.01,0.01), breaks = seq(0,1500,50))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1), minor_breaks=seq(0,1,0.05))+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    axis.text.x=element_text(angle = 90),
    panel.grid.minor.x = element_line(linetype="dotted", color ="grey90"),
    panel.grid.minor.y = element_line(color ="grey90"))

ggsave("distance_ppv.png")
#répartition des distances dans le temps


#calcul des distance médianes et moyennes
resumes_ppv <- OH_plusprochevoisin %>% as_tibble %>% group_by(DATE_DEB) %>% 
  summarise(distance_moyenne=mean(dist_plusprochevoisin), distance_mediane=median(dist_plusprochevoisin))
OH_ppv_resume <- left_join(OH_plusprochevoisin, resumes_ppv)
OH_ppv_resume <- OH_ppv_resume %>%  
  mutate(place = if_else(dist_plusprochevoisin <= distance_moyenne, 
                         true="distance inférieur ou égale à la médiane des distances pour l'année",
                         false="distance supérieure à la médiane des distances pour l'année"),
         metres=if_else(dist_plusprochevoisin<=25,
                        true="distance inférieure ou égale à 25 m",
                        false="distance supérieure à 25 m"))

ggplot(OH_ppv_resume, 
       aes(x=DATE_DEB, y=dist_plusprochevoisin,color=place))+
  geom_point(alpha=0.6, shape=16 )+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  scale_y_continuous(breaks=c(25,seq(0,1500,100)), minor_breaks = seq(0,1500,50))+
  labs(title="Distance des OH a leur plus proche voisin au moment de leur apparition dans le temps",
       y ="distance au plus proche voisin (m)",
       x=  "année",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    legend.title = element_blank(),
    legend.position="bottom",
    legend.direction ="vertical",
    panel.grid.minor.y = element_line(color ="grey90"))

ggsave("distance_temps_mediane_ppv.png")

test <- OH_plusprochevoisin
cuts <-cut(test$DATE_DEB, breaks=seq(0,2050,20))
moyenne_intervalles <- tapply(test$dist_plusprochevoisin, cuts, mean) %>% data.frame()
moyenne_intervalles$annees <- row.names(moyenne_intervalles)


ggplot(resumes_ppv %>% filter(distance_moyenne>=0), 
       aes(x=DATE_DEB, y=distance_moyenne))+
  geom_path(color="grey40", linetype="dotted")+
  geom_point(alpha=0.6, shape=16 )+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
  scale_y_continuous(breaks=c(seq(0,1500,50)), minor_breaks = seq(0,1500,25))+
  labs(title="Distance des OH a leur plus proche voisin au moment de leur apparition dans le temps",
       y ="distance au plus proche voisin (m)",
       x=  "année",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    axis.text.x=element_text(angle = 90),
    panel.grid.minor.y = element_line(color ="grey90"))

ggsave("distance_temps_ppv.png")



#---------------------------------------------------------
#-------------- CALCUL VOISINAGE APPARITION --------------
#---------------------------------------------------------


#---- tableau 1 OH_voisins : colonne voisins_app = liste des OH dans le voisinage au moment de l'apparition----
OH_voisins <- OH_geom_base %>% mutate (voisins_app="null") %>% filter(REFERENCE!="Nahassia 2019" | is.na(REFERENCE)) #sans les cimetières ajoutés
#calcul pour chaque OH de la liste des OH dans un rayon de Xm autour de lui au moment de son apparition
for(i in OH_voisins$OH_NUM){
  OH_test <-OH[OH$OH_NUM==i,] #OH qui apparait
  #taille du buffer selon l'époque (25 et 600[ : 100m, 600 et 1550[ :60 m, 1550 et 2015 : 35 m)
  if(OH_test$DATE_DEB < 600){taille_buffer=100
  }else if(OH_test$DATE_DEB >= 600 & OH_test$DATE_DEB < 1550){taille_buffer=60
  }else if(OH_test$DATE_DEB >= 1550){taille_buffer=35}
  OH_autour <- OH %>% subset(DATE_DEB <= OH_test$DATE_DEB & DATE_FIN >= OH_test$DATE_DEB)#ensemble des OH existants au moment de l'apparition
  inters <- st_intersects(OH_test%>% st_buffer(60), OH_autour) #intersection
  inters_OHNUM <- OH_autour[inters[[1]],]$OH_NUM # liste sous forme des OH_NUM 
  inters_OHNUM <- inters_OHNUM[inters_OHNUM != i] #suppression de OH_test de la liste
  OH_voisins[OH_voisins$OH_NUM==i,]$voisins_app <- list(inters_OHNUM)
}


 OH_voisins$voisins_app[[2]] %>% length()
plot(OH_test$geom %>% st_buffer(60), col="grey")
plot(OH_autour$geom, col="green", add=TRUE)
plot(OH_autour[inters[[1]],]$geom, col="blue", add=TRUE)
plot(OH_test$geom, col="red",add=TRUE)

#---- tableau 2 OH_voisinage : OH étudié avec caractéristiques + nombre de chaque type d'OH voisins (vurb / portee) (1 colonne par type)----
#1.OH_voisins_long
OH_voisins_long <- OH_voisins %>% as_tibble() %>% select(OH_NUM, V_USAGE, V_URB, PORTEE, -geom) %>% mutate(OHref=PORTEE)
OH_voisins_long <- OH_voisins_long[0,]
for(i in OH_voisins$OH_NUM){
  tmp <- OH_voisins %>% as_tibble () %>% select(OH_NUM, V_USAGE, V_URB, PORTEE, -geom) %>% 
    filter(OH_NUM %in% OH_voisins[OH_voisins$OH_NUM==i,]$voisins_app[[1]]) %>% #récuperer les OH qui sont dans le voisinage
    mutate(OHref = i) #ajouter la colonne 
  OH_voisins_long <- rbind(OH_voisins_long, tmp)
}

#2.OH_voisins_wide_sum
#sans les v_usage
#manière reschape2
OH_voisins_wide_sum <- OH_voisins_long %>% select(-OH_NUM, -V_USAGE) %>% 
  mutate(V_URB=paste("urb_",V_URB,sep=""),PORTEE=paste("p_",PORTEE,sep="")) %>% #pour que les catégories v_urb/portée (mêmes id) ne soient pas mélangées
  melt(id.vars="OHref") %>% 
  dcast(OHref ~ value, length) %>% 
  as_tibble()
#ajouter les row où pas de voisinage
OH_voisins_wide_sum <- add_row(OH_voisins_wide_sum, OHref = setdiff(OH_voisins$OH_NUM,OH_voisins_wide_sum$OHref))
OH_voisins_wide_sum[is.na(OH_voisins_wide_sum)] <- 0

#3. OH_voisinage
#1 colonne avec nombre d'OH de chaque type (valeur urbaine + portée) dans le voisinage au moment de l'apparition
#ajout des colonnes OH_geom_base + mise en forme du tableau
OH_voisinage <- left_join(OH_geom_base %>% select(OH_NUM, NOM, DATE_DEB, DATE_FIN, V_USAGE, V_URB, PORTEE, geom),
                          OH_voisins_wide_sum, 
                          by=c("OH_NUM"="OHref"))

# manière DPLYR
# test %>% select(-OH_NUM, -V_USAGE) %>% mutate(V_URB=paste("urb_",V_URB,sep=""),PORTEE=paste("p_",PORTEE,sep="")) %>% 
#   gather(Var, Val, 2:3) %>% 
#   group_by(OHref, Val) %>% 
#   summarise(n=n()) %>% 
#   spread(Val, n, fill=0)


plot(OH_voisinage$geom)
ggplot()+geom_sf(data=OH_voisinage)


#--------------------------------------------------------
#-------------- TABLEAU POUR ANALYSE ETAT  --------------
#--------------------------------------------------------

#fonction qui cree le tableau pour le plot en fonction de l'année étudiée (i)
# voisnages calculés dans les 10 ans autour de la date étudiée
# retourne :  1) tableau des voisinages moyens par type d'activité
#             2) tableau des voisinages moyens centrés par type d'activité
#             3) tableau du voisinage de reférence
creation_tableau_annee <- function(i){
  #tableaux pour 10 ans autour de l'année i
  subtab <- OH_voisinage %>% filter (DATE_DEB<=i+10 & DATE_FIN>=i-10) %>% as_tibble() %>% select(-geom)
  
  #tableau par valeur urbaine ou portée : changer "V_URB" par "PORTEE"
  # sélection et moyenne
  subtab_app <- subtab %>% filter(DATE_DEB>=i-10 & DATE_DEB<=i+10) %>% 
    select(V_URB, urb_1, urb_2, urb_3, urb_4, urb_5, urb_6, p_1, p_2, p_3, p_4) %>% 
    group_by(V_URB) %>% summarise_all(list(mean)) %>% 
    rename(type_OH=V_URB)
  #tableau centré réduit
  subtab_app_c <-subtab_app %>% mutate_at(vars(-type_OH), funs(scale))
  
  #ref : moyenne de tous les voisinages à l'appartition toutes catégories confondues à t
  ref_vois <- subtab_app %>% select(-type_OH) %>% summarise_all(list(mean)) %>% mutate(type_OH="ref")
  
  #tableaux en format long pour plot
  #normal
  subtab_app_long <- subtab_app %>% gather(key="type_voisins", value="nombre_voisins",-type_OH)%>% 
    mutate(type_g = if_else(grepl("^u",type_voisins), true="valeur urbaine", false="portee"))
  ref_long <- ref_vois  %>% gather(key="type_voisins", value="ref",-type_OH)
  tab_plot <-left_join(subtab_app_long, 
                       ref_long %>% select(type_voisins, ref),
                       by="type_voisins")
  #centré-réduit
  subtab_app_long_c <- subtab_app_c %>% gather(key="type_voisins", value="nombre_voisins",-type_OH)%>% 
    mutate(type_g = if_else(grepl("^u",type_voisins), true="valeur urbaine", false="portee"))
  tab_plot_c <- subtab_app_long_c %>% mutate(ref=0)
  
  #ref
  ref_long <- ref_vois  %>% gather(key="type_voisins", value="ref",-type_OH)%>% 
    mutate(type_g = if_else(grepl("^u",type_voisins), true="valeur urbaine", false="portee"))
  
  return(list(tab_plot, tab_plot_c, ref_long))
}


#-----------------------------------------------
#-------------- ANALYSE PAR ETAT  --------------
#-----------------------------------------------

#labels pour facet_grid
facet_labels_vois <- c("portee"="voisinage selon la \nportée des OH", "valeur urbaine"="voisinage selon la \nvaleur urbaine des OH")
labels_types <- c( "urb_1"="1.aménagements",
            "urb_2"="2.structures défensives \net militaires",
            "urb_3"="3.constructions civiles",
            "urb_4"="4.édifices religieux",
            "urb_5"="5.lieux d'inhumation",
            "urb_6"="6.lieux de commerce, \nartisanat, production",
            "p_1"="1.faible",
            "p_2"="2.moyenne",
            "p_3"="3.grande",
            "p_4"="4.exceptionnelle")

#plot 0 : voisinage pour un OH individuel
plot.individuel <- function(tableau,titre){
  ggplot(tableau)+
    geom_segment(aes(x=0, xend=nombre_voisins, y=type_voisins, yend=type_voisins, color=type_voisins), 
                 alpha=0.5, size=0.7)+
    geom_point(aes(x=nombre_voisins, y=type_voisins, color=type_voisins), 
               size=4, alpha=0.8,stroke=0, shape=16)+
    scale_color_manual(values=palette_vurb_p)+
    scale_y_discrete(labels=labels_types)+
    facet_wrap(type_g ~., scales = "free",
               labeller=labeller(type_g=facet_labels_vois))+
    labs(title=titre,
         y ="type de voisins",
         x=  "nombre de voisins",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(linetype ="dotted", color ="grey75")
    )
}

tab_individuel <- OH_voisinage %>% filter(OH_NUM==2099) %>% 
  as_tibble() %>% select(V_URB, urb_1, urb_2, urb_3, urb_4, urb_5, urb_6, p_1, p_2, p_3, p_4, -geom)%>%
  gather(key="type_voisins", value="nombre_voisins",-V_URB) %>% 
  mutate(type_g = if_else(grepl("^u",type_voisins), true="valeur urbaine", false="portee"))
         
plot.individuel(tab_individuel, 
                paste("Profil du voisinage de l'église Saint-Denis au moment de son apparition en 1023"))
ggsave("voisinage_stdenis.png")

#plot 1 : voisinage de référence
plot.ref <- function(tableau,date){
  ggplot(tableau)+
    geom_segment(aes(x=0, xend=ref, y=type_voisins, yend=type_voisins, color=type_voisins), 
                 alpha=0.5, size=0.7)+
    geom_point(aes(x=ref, y=type_voisins, color=type_voisins), 
               size=4, alpha=0.8,stroke=0, shape=16)+
    scale_color_manual(values=palette_vurb_p)+
    scale_y_discrete(labels=labels_types)+
    facet_wrap(type_g ~., scales = "free",
               labeller=labeller(type_g=facet_labels_vois))+
    labs(title=paste("Profil du voisinage de référence des OH entre",date-10,"et",date+10),
         subtitle = paste("moyenne de tous les voisinages d'apparition"),
         y ="type de voisins",
         x=  "nombre de voisins",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(linetype ="dotted", color ="grey75")
    )
}

#plot 2 : voisinage des OH en facette selon valeur urbaine > pbm avec free x/y
plot.vois_facette <- function(tableau,date){
  ggplot(tableau)+
    geom_segment(aes(x=ref, xend=nombre_voisins, y=type_voisins, yend=type_voisins), 
                 color="grey",alpha=0.5, size=0.7)+
    geom_point(aes(x=ref, y=type_voisins), 
               color="grey",size=4)+
    geom_point(aes(x=nombre_voisins, y=type_voisins, color=type_voisins),
               size=4,alpha=0.7,stroke=0, shape=16)+
    scale_y_discrete(labels=labels_types)+
    scale_color_manual(values=palette_vurb_p)+
    facet_grid( type_OH ~type_g, switch="y",
                labeller=labeller(type_OH=facet_labels_vurb, type_g=facet_labels_vois))+
    labs(title=paste("Profil du voisinage moyen des OH en fonction de leur valeur urbaine en",date),
         subtitle="en gris : voisinage de référence",
         y = "type de voisins",
         x= "nombre de voisins",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(linetype ="dotted", color ="grey75")
    )
}

#plot 3 : voisinage des OH sélectionnés
plot.vois <- function(tableau,titre){
  ggplot(tableau)+
    geom_segment(aes(x=ref, xend=nombre_voisins, y=type_voisins, yend=type_voisins), 
                 color="grey",alpha=0.5, size=0.7)+
    geom_point(aes(x=ref, y=type_voisins), 
               color="grey",size=4)+
    geom_point(aes(x=nombre_voisins, y=type_voisins, color=type_voisins),
               size=4,alpha=0.7,stroke=0, shape=16)+
    scale_color_manual(values=palette_vurb_p)+
    scale_y_discrete(labels=labels_types)+
    facet_wrap(type_g ~., scales = "free",
               labeller=labeller(type_g=facet_labels_vois))+
    labs(title=titre,
         subtitle="en gris : voisinage de référence",
         y = "type de voisins",
         x= "nombre de voisins",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(linetype ="dotted", color ="grey75")
    )
}




#---- analyses--------
date <- 1300
tabs <- creation_tableau_annee(date) #ignorer le warning
tab_plot <- tabs[[1]] #moyenne
tab_plot_c <- tabs[[2]] #centré-réduit
tab_ref <- tabs[[3]] #voisinage de ref
plot.ref(tab_ref,date)
plot.individuel(tab_plot,
                titre =paste("Profil du voisinage moyen des  édifices religieux (OH de valeur urbaine 4)\napparaissant entre",
                             date-10,"et",date+10)
)
plot.vois(tab_plot %>% filter(type_OH==3),
          titre=paste("A. Profil du voisinage moyen des édifices religieux (OH de valeur urbaine 4)\napparaissant entre",
                      date-10,"et",date+10)
)
plot.vois(tab_plot_c %>% filter(type_OH==3),
          titre=paste("B. Voisinage moyen centré-réduit des édifices religieux (OH de valeur urbaine 4)\napparaissant entre",
                      date-10,"et",date+10)
)

ggsave("1023_moyenne.png")
ggsave("1300_ref_vois.png")
ggsave("1300_rel.png")
ggsave("1300_rel_ref1.png")
ggsave("1300_rel_ref2.png")


OH_voisinage %>% filter(OH_NUM==28)



ggplot(tab_plot_c %>% filter(type_g=="valeur urbaine"))+
  geom_path(aes(x=type_voisins, y=nombre_voisins, group=type_g))+
  geom_point(aes(x=type_voisins, y=ref), 
             color="grey",size=2)+
  geom_point(aes(x=type_voisins, y=nombre_voisins, color=type_OH),
             size=2,alpha=0.7,stroke=0, shape=16)+
  scale_color_manual(values=palette_vurb2)+
  scale_y_continuous(breaks=c(-1,0,1,2))+ 
  coord_radar()+
  facet_wrap(type_OH ~.,
             labeller=labeller(type_OH=facet_labels_vurb, type_g=facet_labels_vois))+
  labs(title=paste("profil du voisinage (moyenne) des OH en fonction de leur valeur urbaine en",date),
       subtitle="en gris : voisinage de référence",
       y = "nombre de voisins",
       x= "type de voisins",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype ="dotted", color ="grey75")
  )




coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}


View(OH_rel %>% filter(dist_urb>700) %>% select(NOM, V_USAGE, PORTEE, dist_urb))


#---------------------------------------------------
#-------------- ANALYSE LONGITUDINALE --------------
#---------------------------------------------------

#---- tableau pour toutes les années ----
#par valeur urbaine
#structure du tableau
voisinages_moyens_longi <- data.frame(type_OH=NA,
                                       annee=NA, 
                                       urb_1=NA, urb_2=NA, urb_3=NA, urb_4=NA, urb_5=NA, urb_6=NA,
                                       p_1=NA, p_2=NA, p_3=NA, p_4=NA)
#remplissage du tableau
for(i in OH_voisinage$DATE_DEB){ 
tab_tmp <- OH_voisinage %>%
  mutate(annee=i) %>% #ajout de l'année calculée
  as_tibble()%>% 
  filter(DATE_DEB>=i-10 & DATE_DEB<=i+10) %>% #intervalle de temps autour de cette année
  select(annee, V_URB, urb_1, urb_2, urb_3, urb_4, urb_5, urb_6, p_1, p_2, p_3, p_4, - geom) %>% #choix des colonnes à conserver
  group_by(V_URB) %>% summarise_all(list(mean)) %>%  #moyenne par type d'OH
  rename(type_OH=V_URB) 
voisinages_moyens_longi <- rbind(voisinages_moyens_longi,tab_tmp) #ajout des lignes pour l'année au tableau entier
  
}
#suppression de tous les duplicatas
voisinages_moyens_longi <- voisinages_moyens_longi %>% distinct() 


#----tableaux en format long pour plot----
#voisinages moyens
vois_longi_m_long <- voisinages_moyens_longi %>% gather(key="type_voisins", value="nombre_voisins",-annee, -type_OH) %>% 
  mutate(type_g = if_else(grepl("^u",type_voisins), true="valeur urbaine", false="portee"))
#choix de la valeur urbaine à analyser
vois_longi_m_long_4 <- vois_longi_m_long %>% filter(type_OH==4)


#---- plot moyenne par année ----
ggplot(vois_longi_m_long_4) +
  geom_segment(aes(x=annee, xend=annee,yend=nombre_voisins, y=0), 
               color="grey",alpha=0.5, size=0.7)+
  # geom_line(aes(x=annee, y=nombre_voisins), 
  #           color="grey", size=0.5,alpha=0.8)+
  geom_point(aes(x=annee, y=nombre_voisins, color=ifelse(nombre_voisins<0,"grey",type_voisins)), alpha=0.8)+
  scale_color_manual(values=palette_vurb_p)+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015), limits=c(-25, 2015))+
  facet_grid(type_voisins~.)+
  labs(title="Voisinage moyen des activités religieuses",
       subtitle="valeur urbaine 4",
       y = "nombre moyen de voisins par valeurs urbaine et portées",
       x= "année",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    axis.text.x = element_text(angle=90)
  )

ggsave("moyen_temps.png")

#plot que portée
plot_portee <- ggplot(vois_longi_m_long_4 %>% filter(type_g=="portee")) +
  geom_segment(aes(x=annee, xend=annee,yend=nombre_voisins, y=0), 
               color="grey",alpha=0.5, size=0.7)+
  geom_point(aes(x=annee, y=nombre_voisins, color=type_voisins), alpha=0.8)+
  scale_color_manual(values=palette_vurb_p)+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015), limits=c(-25, 2015))+
  facet_grid(type_voisins~type_g, scales = "free_y",
             labeller=labeller(type_voisins=labels_types,
                               type_g=facet_labels_vois))+
  labs(y = "nombre d'OH)",
       x= "année")+
  theme_fivethirtyeight()+
  theme_ln()

#plot que vurb
plot_vurb <- ggplot(vois_longi_m_long_4 %>% filter(type_g=="valeur urbaine")) +
  geom_segment(aes(x=annee, xend=annee,yend=nombre_voisins, y=0), 
               color="grey",alpha=0.5, size=0.7)+
  geom_point(aes(x=annee, y=nombre_voisins, color=type_voisins), alpha=0.8)+
  scale_color_manual(values=palette_vurb_p)+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015), limits=c(-25, 2015))+
  facet_grid(type_voisins~type_g, scales = "free_y",
             labeller=labeller(type_voisins=labels_types,
                               type_g=facet_labels_vois))+
  labs(y = "nombre d'OH)",
       x= "année")+  theme_fivethirtyeight()+
  theme_ln()


grid.arrange(
  top="Profil de voisinage des édifices religieux",
  bottom="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT",
  plot_portee,plot_vurb,
  # ncol=2,
  # nrow=6,
  layout_matrix=rbind(c(1,2),
                      c(1,2),
                      c(1,2),
                      c(1,2),
                      c(NA,2)))




#----tableau centré-réduit par rapport à la moyenne toute dates confondues----


#voisinages de référence = voisinages moyens pour chaque années toutes catégories confondues
ref_vois_longi <- vois_longi_m_long %>% group_by(annee, type_voisins) %>%  summarise(nombre_voisins_moyen=mean(nombre_voisins))
vois_longi_m_long_4b <- left_join(vois_longi_m_long_4,
                                  ref_vois_longi %>% select(type_voisins, annee, nombre_voisins_moyen),
                                  b=c("type_voisins","annee")
                                  ) %>% mutate(centre=nombre_voisins-nombre_voisins_moyen)
            


#---- plot moyenne par année ----

ggplot(vois_longi_m_long_4b) +
  geom_hline(aes(yintercept = 0), color="darkgrey", size=1)+
  geom_segment(aes(x=annee, xend=annee,yend=centre, y=0), 
               color="grey",alpha=0.5, size=0.7)+
  geom_point(aes(x=annee, y=centre, color=type_voisins), alpha=0.8)+
  scale_color_manual(values=palette_vurb_p)+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015), limits=c(-25, 2015))+
  facet_grid(type_voisins~.)+
  labs(title="Profil de voisinage centré-réduit des activités religieuses",
       y = "variation autour de la moyenne (variables centrées-réduites)",
       x= "année",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
theme(
  axis.text.x = element_text(angle=90)
)

ggsave("centre_longi.png")







#----  autre voisinage de référence longi ----
#voisinage de référence toutes périodes confondues pour la valeur urbaine 
rm(ref_longi_4) OH_voisinage_4 %>% select(urb_1, urb_2, urb_3, urb_4, urb_5, urb_6, p_1, p_2, p_3, p_4) %>% 
  summarise_all(list(mean)) %>% mutate(V_URB=4)

#moyenne des voisinages pour chaque année 
vois_longi_m <- OH_voisinage_4 %>%  select(DATE_DEB, urb_1, urb_2, urb_3, urb_4, urb_5, urb_6, p_1, p_2, p_3, p_4) %>% 
  group_by (DATE_DEB) %>% summarise_all(list(mean))
#moyenne centrée-reduités
vois_longi_c <- vois_longi_m %>% mutate_at(vars(-DATE_DEB),  funs(scale))

#tableau en format long pour plot
#voisinages moyens
vois_longi_m_long <- vois_longi_m %>% gather(key="type_voisins", value="nombre_voisins",-DATE_DEB)%>% 
  mutate(type_g = if_else(grepl("^u",type_voisins), true="valeur urbaine", false="portee"),
         ref=0)
#voisinage centre reduits
vois_longi_c_long <- vois_longi_c %>% gather(key="type_voisins", value="nombre_voisins",-DATE_DEB)%>% 
  mutate(type_g = if_else(grepl("^u",type_voisins), true="valeur urbaine", false="portee"),
         ref=0)
#reference
ref_longi_long <- ref_longi_4 %>% gather(key="type_voisins", value="nombre_voisins",-V_URB)%>% 
  mutate(type_g = if_else(grepl("^u",type_voisins), true="valeur urbaine", false="portee"))



#----plot centré-réduit par rapport à la moyenne toute dates confondues----

ggplot(vois_longi_c_long) +
  geom_hline(aes(yintercept = ref), color="darkgrey", size=1)+
  geom_segment(aes(x=DATE_DEB, xend=DATE_DEB,yend=nombre_voisins, y=0), 
              color="grey",alpha=0.5, size=0.7)+
  geom_point(aes(x=DATE_DEB, y=nombre_voisins, color=type_voisins), alpha=0.8)+
  scale_color_manual(values=palette_vurb_p)+
  scale_x_continuous(breaks = c(1,seq(100,1900,100),2015), limits=c(-25, 2015))+
  facet_grid(type_voisins~., scales = "free_y")+
  labs(title="B. Profil de voisinage moyen par rapport au voisinage de référence",
       subtitle="en gris : voisinage de référence",
       y = "variation autour de la moyenne (variables centrées-réduites)",
       x= "année",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()

ggsave("rel_longi.png", width=16, height = 17, units="cm")


#☺voisinage de référence
ggplot(ref_longi_long)+
  geom_segment(aes(y=type_voisins, yend=type_voisins, x=0, xend=nombre_voisins, color=type_voisins), 
               alpha=0.5, size=0.7)+
  geom_point(aes(y=type_voisins, x=nombre_voisins, color=type_voisins), 
             size=4, alpha=0.8,stroke=0, shape=16)+
  scale_color_manual(values=palette_vurb_p)+
  facet_wrap(.~type_g , scales = "free_y",
             labeller=labeller(type_g=facet_labels_vois))+
  labs(title=paste("A. Voisinage de référence des OH de valeur urbaine 4 toutes époques confondues"),
       # subtitle="référence : moyenne toutes époques confondues ",
       x = "nombre de voisins",
       y = "type de voisins",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linetype ="dotted", color ="grey75")
  )

ggsave("ref_epoques.png")


facet_grid(type_OH ~ type_g, scales = "free_x",
           labeller=labeller(type_OH=facet_labels_vurb, type_g=facet_labels_vois))+
  
  
  
  


