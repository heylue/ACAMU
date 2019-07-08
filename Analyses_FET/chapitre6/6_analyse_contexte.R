###############################################
## Analyse des activités en fonction de l'environnement
## Traitements pour l'analyse spatiale verticale, chapitre 6
## Analyses_FET
## L. Nahassia, 2019
## Zenodo. https://doi.org/10.5281/zenodo.3256673
##############################################

# # données :
# # ToToPI_V3

#library
library(sp)
library(sf)
library(rgdal)
library(RSQLite)
library(rgeos)

library(ggplot2)
library(ggthemes)
library(ggstance)
library(ggpubr)
library(ggExtra)
library(gridExtra)
library(plotly)
library(dplyr)
library(tidyr)
library(TraMineR)
library(forcats)
library(arules)


#----------------------------------
#-------------- IMPORT ------------
#----------------------------------

EPSG = make_EPSG()
proj_2154 = EPSG[which(EPSG$code == 2154), "prj4"]

#Import OH
OH_p <- st_read(dsn="../ToToPI_V3.gpkg", layer="OH_ponctuels")

#mise en forme
OH_p$OH_NUM <- as.numeric(OH_p$OH_NUM)
OH_p  <- OH_p [OH_p$V_USAGE != 11
                 & OH_p$V_USAGE < 70
                 & OH_p$DATE_FIN > -25,]
OH_p$V_URB <- cut (OH_p$V_USAGE,
                     breaks=c(0,20,30,40,50,60,70),
                     labels=c(1,2,3,4,5,6),
                     right=FALSE,
                     include.lowest = TRUE)

#transformation en sp pour over
OH_base <- as(OH_p, "Spatial")

#Import ens_urb
ens_urb_total <- st_read(dsn="../ToToPI_V3.gpkg", layer="ensembles_urbains")## à revoir
write.csv(ens_urb_total %>% as_tibble() %>% select(-geom), "zst_table.csv")## à revoir
##mise en forme
##ajouter un ens_urb DUMMY pour que la fonction over ne bug pas quand il n'y a pas d'ensembles urbains > sur QGIS / plus la peine ?
#transformation en sp pour over
ens_urb_base <- as(ens_urb_total, "Spatial")

plot(ens_urb_base)
plot(OH_base, add=TRUE)


#--------------------------------------------------------------------------------------------------------------------------
#-------------- CREATION D'UN TABLEAU RECENSANT LES ZONES DE LOCALISATION DES OH TOUT AU LONG DE LEUR EXISTENCE ------------
#--------------------------------------------------------------------------------------------------------------------------
#---- intersection entre localisation des centroides des OH et les ensembles urbains

##### Le tableau indique le type de localisation de chaque OH au cours de son existence
## tableau long : un ligne par période de localistion, à chaque fois qu'un OH change de zone > une nouvelle ligne ="période de localisation"
# OH_NUM : identifiant de l'OH - non unique (une ligne par type de localisation)
# date_debut : début de la période de localisation
# date_fin : fin de la période de localisation
# ID : identifiant de la zone de localisation
# ID_nom : nom de la zone de localisation
# occupation : type d'occupation de la zone de localisation (urbaine/intermédiaire/non urbaine)
# densité : type de densité de la zone de localisation (1/2/3)
# n_zone : nombre de zones de localisation différentes par OH (1 zone = pas de changements)
# diff_zone_occ : est-ce que la zone d'occupation est différente de la zone précédente (T/F)
# n_diff_occ : nombre de zones d'occupation différentes par OH
# diff_zone_dens : est-ce que la zone de densité est différente de la zone précédente (T/F)
# n_diff_dens : nombre de zones de densité différentes par OH
# date_debut_OH : date de début de l'existence de l'OH
# date_fin_OH : date de fin de l'existence de l'OH
# NOM : nom de l'OH
# V_USAGE : valeur d'usage de l'OH
# NOM_USAGE : nom de la valeur d'usage de l'OH
# V_URB : valeur urbaine de l'OH
# PORTEE : portée de l'OH

OH2 <- OH_base 
ens_urb2 <- ens_urb_base
ens_urb2@data$OH_NUM <- NA

#---- création ----
#structure
df_exi <- ens_urb2@data[c(),]
#remplissage avec intersection
for (i in OH2$OH_NUM){
  OH_tmp <- OH2[OH2$OH_NUM==i,]
  #over
  over_a <- over(OH_tmp, 
                 subset(ens_urb2, date_debut <= OH_tmp$DATE_FIN & date_fin >= OH_tmp$DATE_DEB), 
                 returnList = TRUE)
  over_aa <- eval(parse(text=paste("over_a","$\"",row.names(OH_tmp),"\"",sep=""))) #récupère le résultat de over$xxx (xxx = rowname de l'OH testé)
  #si pas d'appartenance > ajout d'une ligne null
  if(is.null(over_aa) || nrow(over_aa)<1) {
    df_tmp <- data.frame(ID=NA, 
                         ID_nom=NA,
                         description=NA, 
                         date_debut=OH_tmp$DATE_DEB,
                         date_fin=OH_tmp$DATE_FIN,
                         occupation=NA,
                         densite=NA,
                         justif_geom=NA,
                         sources_geom=NA,
                         sources_densite=NA,
                         remarques=NA,
                         OH_NUM=OH_tmp$OH_NUM,
                         date_debut_OH=OH_tmp$DATE_DEB,
                         date_fin_OH=OH_tmp$DATE_FIN)
    df_exi <- rbind(df_exi, df_tmp)
    #sinon Rbind et ajout des potentielle ligne avant après
  } else {
    #ajout numéro d'OH, date de début et date de fin
    over_aa$OH_NUM <- OH_tmp$OH_NUM
    over_aa$date_debut_OH=OH_tmp$DATE_DEB
    over_aa$date_fin_OH=OH_tmp$DATE_FIN
    #ajustement date début et fin
    #si OH apparaît dans aucune zone > ajout d'une ligne nulle
    if (OH_tmp$DATE_DEB %>% as.numeric() < min(over_aa$date_debut) %>% as.numeric()) {
      df_app_tmp <- data.frame(ID=NA, 
                               ID_nom=NA,
                               description=NA, 
                               date_debut=OH_tmp$DATE_DEB,
                               date_fin=min(over_aa$date_debut)-1,
                               occupation=NA,
                               densite=NA,
                               justif_geom=NA,
                               sources_geom=NA,
                               sources_densite=NA,
                               remarques=NA,
                               OH_NUM=OH_tmp$OH_NUM,
                               date_debut_OH=OH_tmp$DATE_DEB,
                               date_fin_OH=OH_tmp$DATE_FIN)
      df_exi <- rbind(df_exi, df_app_tmp)
      #sinon ajuster la date de début
    }else{
      over_aa[which.min(over_aa$date_debut),]$date_debut <- OH_tmp$DATE_DEB
    }
    #si OH disparait dans aucune zone > ajout d'une ligne nulle
    if(OH_tmp$DATE_FIN %>% as.numeric() > max(over_aa$date_fin) %>% as.numeric()){
      df_disp_tmp <- data.frame(ID=NA, 
                                ID_nom=NA,
                                description=NA, 
                                date_debut=max(over_aa$date_fin)+1,
                                date_fin=OH_tmp$DATE_FIN,
                                occupation=NA,
                                densite=NA,
                                justif_geom=NA,
                                sources_geom=NA,
                                sources_densite=NA,
                                remarques=NA,
                                OH_NUM=OH_tmp$OH_NUM,
                                date_debut_OH=OH_tmp$DATE_DEB,
                                date_fin_OH=OH_tmp$DATE_FIN)
      df_exi <- rbind(df_exi, df_disp_tmp)
    }else{
      over_aa[which.max(over_aa$date_fin),]$date_fin <- OH_tmp$DATE_FIN
    }
    
    #bind reste du tableau
    df_exi <- rbind(df_exi, over_aa)
  }
}

#---- mise en forme du tableau ----
OH_over_exi <- df_exi %>% select(OH_NUM,date_debut,date_fin,ID,ID_nom,occupation,densite,date_debut_OH, date_fin_OH)
OH_over_exi <-OH_over_exi %>%  filter(date_debut!=date_fin) #suppression quand une zone est sur une seule année)
OH_over_exi[OH_over_exi$date_fin == OH_over_exi$date_fin_OH-1,]$date_fin <- OH_over_exi[OH_over_exi$date_fin == OH_over_exi$date_fin_OH-1,]$date_fin_OH 
OH_over_exi$OH_NUM <- as.numeric(OH_over_exi$OH_NUM)
OH_over_exi <- arrange(OH_over_exi, OH_NUM, date_debut)
#mise en forme densite
OH_over_exi$densite[is.na(OH_over_exi$densite)] <- "0"
OH_over_exi$densite <- factor(OH_over_exi$densite, levels=c("3","2","1","0"))
# mise en forme occupation
# urbaine/intermédiaire/non urbaine
OH_over_exi$occupation[is.na(OH_over_exi$occupation)] <- "non urbaine"
OH_over_exi$occupation <- as.factor(OH_over_exi$occupation)
#nombre de zone par OH (1 zone = 0 changements)
compte_zones<- count(OH_over_exi, OH_NUM) %>% as.data.frame() %>% mutate(n_zone = n) %>% select(OH_NUM, n_zone)
OH_over_exi <- left_join(OH_over_exi, compte_zones, by=c("OH_NUM"="OH_NUM"))
#est-ce que la zone est différente de la zone précédente (occupation/densité)
OH_over_exi$diff_zone_occ <- lag(OH_over_exi$occupation) != OH_over_exi$occupation
OH_over_exi[OH_over_exi$date_debut==OH_over_exi$date_debut_OH,]$diff_zone_occ <- F
OH_over_exi$diff_zone_dens <- lag(OH_over_exi$densite) != OH_over_exi$densite
OH_over_exi[OH_over_exi$date_debut==OH_over_exi$date_debut_OH,]$diff_zone_dens <- F
#nombre de changement de zones (occupation/densité)
compte_diff_occ <- OH_over_exi %>% group_by(OH_NUM) %>% summarise(n_diff_occ=sum(diff_zone_occ==T))
OH_over_exi <- left_join(OH_over_exi, compte_diff_occ, by=c("OH_NUM"="OH_NUM"))
compte_diff_dens <- OH_over_exi %>% group_by(OH_NUM) %>% summarise(n_diff_dens=sum(diff_zone_dens==T))
OH_over_exi <- left_join(OH_over_exi, compte_diff_dens, by=c("OH_NUM"="OH_NUM"))
#join nom, valeurs d'usage et valeurs urbaines
OH_over_exi<- left_join(OH_over_exi, OH2@data %>% select(OH_NUM, NOM, V_USAGE, NOM_USAGE, V_URB, PORTEE), by=("OH_NUM"="OH_NUM"))
#mise en forme OH_NUM en factor
OH_over_exi$OH_NUM <- as.factor(OH_over_exi$OH_NUM)
#temporaire : suppression des cimetières de joué (hors zone d'étude)
OH_over_exi <-  filter(OH_over_exi,!grepl("joué", ignore.case = TRUE, OH_over_exi$NOM))
#suppression si date fin > -25 et enceintes
OH_over_exi <- OH_over_exi %>% filter(date_fin>-25, V_USAGE != 21)
####
#tableau avec uniquement les zones au moment de l'apparition + suppression des cimetières hors zones quand apparaissent
####
OH_over_app <- OH_over_exi %>% filter(date_debut == date_debut_OH)
#formatage plus petite date début (observation commence en -25)
OH_over_exi$date_debut[OH_over_exi$date_debut< -25] <- -25
OH_over_app$date_debut[OH_over_app$date_debut< -25] <- -25
# seul diff entre les 2 : 1771 qui apparaît avant 


#tableau pour application
write.csv(OH_over_exi,"OH_over_exi.csv", fileEncoding = "UTF8")
write.csv(OH_over_app,"OH_over_app.csv", fileEncoding = "UTF8")

test <- OH_over_exi %>% filter(date_fin == date_fin_OH -1)

#----------------------------------------------------------------------------------------
#-------------- ANALYSE 1 :  APPARITION  ----------
#----------------------------------------------------------------------------------------
#modif urbaine > A, intermediaire > B, non urbaine > C pour gérer l'ordre dans les facettes
OH_over_app$occupation <- factor(OH_over_app$occupation, levels=c("urbaine", "intermediaire","non urbaine"))

#---- A. Résumés statistiques ----

#répartition OH * zones
OH_over_app$occupation %>% summary()
OH_over_app$occupation %>% summary() * 100 / length(OH_over_app$occupation)
OH_over_app$densite %>%  summary()
OH_over_app$densite %>% summary() * 100 / length(OH_over_app$densite)
contingence <- table(OH_over_app$occupation, OH_over_app$densite)
prop.table(contingence,1)*100
prop.table(contingence,2)*100



#répartition valeurs d'usage * zones
table(OH_over_app$occupation, OH_over_app$V_USAGE) %>% chisq.test() #NA
table(OH_over_app$densite, OH_over_app$V_USAGE) %>% chisq.test() #indépendance rejetée

OH_tab <- OH_over_app %>% select(V_USAGE,occupation)
repartition_urb <- OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(V_USAGE) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(occupation,ptg, fill=0)

OH_tab <- OH_over_app %>% select(V_USAGE,densite)
repartition_dens <- OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(V_USAGE) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(densite,ptg, fill=0)

#répartition valeurs urbaines * zones
table(OH_over_app$occupation, OH_over_app$V_URB) %>% chisq.test() #indépendance rejetée
table(OH_over_app$densite, OH_over_app$V_URB) %>% chisq.test()#indépendance rejetée

OH_tab <- OH_over_app %>% select(V_URB,occupation)
OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(V_URB) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(occupation,ptg, fill=0) %>% View()

OH_tab <- OH_over_app %>% select(V_URB,densite)
OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(V_URB) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(densite,ptg, fill=0) %>% View()

#répartitions portées * zones
table(OH_over_app$occupation, OH_over_app$PORTEE) %>% chisq.test() #indépendance rejetée
table(OH_over_app$densite, OH_over_app$PORTEE) %>% chisq.test() #indépendance rejetée

OH_tab <- OH_over_app %>% select(PORTEE,occupation) 
OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(PORTEE) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(occupation,ptg, fill=0) %>% View()

OH_tab <- OH_over_app %>% select(PORTEE,densite)
OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(PORTEE) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(densite,ptg, fill=0)%>% View()

# rapport taille des OH/zone de densite (plus grand == zone de faible densité ?)
OH_polygones <- st_read(dsn="../ToToPI_V3.gpkg", layer="OH_pg_uniques") %>% filter(V_USAGE != 11  
                                                                                   & V_USAGE < 70
                                                                                   & DATE_FIN > -25
                                                                                   & !grepl("joué", ignore.case = TRUE, NOM))
OH_polygones$OH_NUM <- as.character(OH_polygones$OH_NUM)
OH_over_app$OH_NUM <- as.character(OH_over_app$OH_NUM)
OH_polygones <- left_join(OH_polygones,OH_over_app %>% select(OH_NUM, densite)) 
OH_polygones$aire <- st_area(OH_polygones) %>% as.numeric()
OH_polygones %>%  group_by(densite) %>% summarise(max=max(aire), med=median(aire), min=min(aire), moy=mean(aire),sd=sd(aire))
st_write(OH_polygones, dsn="inhum_poly.geojson", driver="geoJSON")


ggplot(OH_polygones, aes(x=aire)) + geom_histogram(bins=200)


a <- ggplot(OH_polygones, aes(x=densite, y=aire)) + 
  geom_boxplot(outlier.color="grey40", color="grey30")+
  stat_summary(fun.y=mean, geom="point", shape=1, size=3, color="indianred1")+ 
  theme_fivethirtyeight()+theme_ln()+
  scale_y_log10()+
labs(title="Tailles des des OH en fonction de la \ndensité de leur zone d'apparition",
     y="taille (m2)",
     x="densité de la zone d'apparition",
     caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")
a
#pour enlever les outliers
# ylim1 <- boxplot.stats(OH_polygones$aire)$stats[c(1,5)]
# a+coord_cartesian(ylim=ylim1)

#---- B. Plots ----

#labels des facettes des plots
facet_labels_occupation <- c("urbaine"="Zone \nd'occupation \nurbaine", "intermediaire"="Zone \nd'occupation \nintermédiaire", "non urbaine"="Zone \nd'occupation \nnon urbaine")
facet_labels_portee <- c("1"="portée\npetite (1)", "2"="portée\nmoyenne (1)", "3"= "portée\ngrande (1)", "4"="portée\nexceptionnelle (4)")
facet_labels_dens <- c("3"="zone de densité \nforte (3)", "2"="zone de densité \nmoyenne (2)", "1"="zone de faible \ndensité (1)", "0"="Zone non urbaine")
# facettes possibles pour les plots
occ2 <- "occupation~PORTEE"
occ1 <- "occupation~."
dens2 <-"densite~PORTEE"
dens1 <- "deb_densite~."

####
#création d'un plot qui représente le nombre d'OH apparaissant dans chaque type d'occupation au cours du temps
#df = tableau OH_over_app 
#sous-titre = sous-titre
#wrap= 1 facettes (occupations) ou 2 (occupations*portées) - par défaut 2 facettes
plot_occupation <- function(df, sous_titre, wrap=occ2) {
    ggplot(df, aes(x=date_debut)) +
    geom_bar(
      stat="count",
      width=6,
      fill="grey10") +
    geom_histogram( 
      binwidth=100,
      aes(alpha=0.6, fill=occupation)) +
    facet_grid(
      wrap,
      labeller=labeller(occupation=facet_labels_occupation, PORTEE=facet_labels_portee))+
    scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
    scale_fill_manual(values=c("urbaine"="#a05050", "intermediaire"="#c8ab37", "non urbaine"="darkgrey"))+
    labs(title="Localisation des OH au moment de leur apparition par type d'occupation du sol",
         subtitle=sous_titre,
         x="année d'apparition",
         y="nombre d'OH",
         caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln() +
    theme(strip.text.y = element_text(size=10),
          axis.text=element_text(size=9),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}

####
#création d'un plot qui représente la part des apparitions par type d'occupation
#df = tableau OH_over_app 
#sous-titre = sous-titre
plot_part_occupation <- function(df, sous_titre) {
  
  ggplot(df, aes(x=date_debut)) +
    geom_histogram(
      position="fill",
      bins=20,
      alpha=0.5,
      aes( fill=occupation)) +
    scale_fill_manual(values=c("urbaine"="#a05050", "intermediaire"="#c8ab37", "non urbaine"="darkgrey"),
                      name="Type d'occupation de la zone d'apparition")+
    scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
    scale_y_continuous(labels=scales::percent)+
    labs(title="Répartition des zones d'apparition des OH par type d'occupation du sol",
         subtitle=sous_titre,
         x="année",
         y="part des apparitions dans \nchaque type d'occupation du sol",
         caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln() +
    theme(
          legend.position="bottom",
          strip.text.y = element_text(size=8),
          axis.text=element_text(size=8),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}

####
#création d'un plot qui représente le nombre d'OH apparaissant dans chaque type de densité au cours du temps
#df = tableau OH_over_app 
#sous-titre = sous-titre
#wrap= 1 facettes (densite) ou 2 (densite*portées) - par défaut 2 facettes
plot_densite <- function(df, sous_titre, wrap=dens2) {
  ggplot(df,aes(x=date_debut)) +
    geom_bar(
      stat="count",
      width=6,
      fill="grey10") +
    geom_histogram( 
      binwidth=100,
      aes(alpha=0.4, fill=densite)) +
    facet_grid(
      wrap,
      labeller=labeller(densite=facet_labels_dens, PORTEE=facet_labels_portee))+
    scale_x_continuous(breaks = c(-25,1,seq(100,1900,100),2015))+
    scale_fill_manual(values=c("3"="#016957", "2"="#3ab29d", "1"="#aaddd4", "0"="darkgrey"))+
    labs(title="Localisation des OH au moment de leur apparition par type de densité du bâti",
         subtitle=sous_titre,
         x="année d'apparition",
         y="nombre d'OH",
         caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln() +
    theme(strip.text.y = element_text(size=10),
          axis.text=element_text(size=9),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}

####
#création d'un plot qui représente la part des apparitions par type de densité
#df = tableau OH_over_app 
#sous-titre = sous-titre
plot_part_densite <- function(df, sous_titre) {
  
  ggplot(df, aes(x=date_debut)) +
    geom_histogram(
      position="fill",
      binwidth = 100,
      alpha=0.4,
      aes( fill=densite)) +
    scale_fill_manual(values=c("3"="#016957", "2"="#3ab29d", "1"="#aaddd4", "0"="darkgrey"),
                      name="TDensité de la zone d'apparition",
                      labels=c("3"="forte","2"="moyenne","1"="faible","0"="non urbaine"))+
    scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
    scale_y_continuous(labels=scales::percent)+
    labs(title="Répartition des zones d'apparition des OH par type de densité",
         subtitle=sous_titre,
         x="année",
         y="part des apparitions dans \nchaque type d'occupation du sol",
         caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln() +
    theme(
      legend.position="bottom",
      strip.text.y = element_text(size=8),
      axis.text=element_text(size=8),
      axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
}

#--- résultats  ----
num <-4
sstitre <- "valeur urbaine 4 : édifices religieux"
wrap <- #a remplir si différent du wrap par défaut

plot_part_occupation(OH_over_app, "par portée") + facet_grid(PORTEE~.,labeller=labeller(PORTEE=facet_labels_portee))

plot_occupation(OH_over_app %>% filter(V_URB==5),"valeur urbaine 5 : lieux d'inhumation", wrap=occ1)
ggsave("plot_inhum.png", height =15, width = 24, units="cm")
plot_part_occupation(OH_over_app %>% filter(V_URB==5),"valeur urbaine 5 : lieux d'inhumation")
ggsave("plot_part_rel.png", height =9, width = 16, units="cm")

plot_occupation(OH_over_app %>% filter(V_URB == num),sstitre) 
plot_part_occupation(OH_over_app %>% filter(V_URB == num), sstitre)
plot_densite(OH_over_app %>% filter(V_USAGE == num), sstitre) 
plot_part_densite(OH_over_app %>% filter(V_USAGE == num), sstitre) + facet_grid(PORTEE~.,labeller=labeller(PORTEE=facet_labels_portee))

facet_grid(PORTEE~., labeller=labeller(PORTEE=facet_labels_portee))
plot_occupation(subset(OH_over_app, grepl("port ", OH_over_app$NOM)), "Ports")
ggsave(file="test.pdf", plot=plot_URB5, width=9.792, height = 5.833)
plot_densite(subset(OH_over_app, V_USAGE == 12 | V_USAGE == 16 | V_USAGE ==17 | grepl("port ", OH_over_app$NOM) ), "OH aménagements")

#comparaison des profils / part quand ça change
#choix automatique quand ça change ou pas : + ajouter exclusion des enceintes
usage_exclure <- repartition_urb %>% filter(intermediaire==100 | 'non urbaine'==100 | urbaine==100) %>% select(V_USAGE)
comparaison_app <- OH_over_app %>% filter(!(V_USAGE %in% usage_exclure$V_USAGE))
#liste perso : 
usage_inclure <- c(16,34,42,43,44,51,52,53,62,63)
comparaison_app <- OH_over_app %>% filter(V_USAGE %in% usage_inclure)
comparaison_app$categories <- comparaison_app$V_USAGE
comparaison_app[comparaison_app$V_URB==5,]$categories <- 50


plot_part_occupation(comparaison_app, "tous") + facet_grid(categories~.)
plot_part_densite(comparaison_app, "tous") + facet_grid(categories~.)

#-------------------------------------------------------------------------
#-------------- ANALYSE 2 : en cours d'existence  -------
#-------------------------------------------------------------------------


#---- A.stats changements ----
#nombre d'OH par nombre de transitions
OH_exi_resume <- OH_over_exi %>% 
  group_by(OH_NUM) %>% 
  summarize(n_diff_occ=max(n_diff_occ), n_diff_dens = max(n_diff_dens))
count(OH_exi_resume, n_diff_occ) %>% mutate(p=n/sum(n)*100)
count(OH_exi_resume, n_diff_dens)%>% mutate(p=n/sum(n)*100)


#transitions par catégories d'OH
OH_exi_resume_cat <- OH_over_exi %>% 
  group_by(OH_NUM) %>% 
  summarize(n_diff_occ=max(n_diff_occ), n_diff_dens = max(n_diff_dens)) %>% 
  left_join(OH_over_exi %>% select(OH_NUM,V_USAGE, V_URB, PORTEE), by=c(OH_NUM, OH_NUM))

count(OH_exi_resume, n_diff_occ) %>% mutate(p=n/sum(n)*100)
count(OH_exi_resume, n_diff_dens)%>% mutate(p=n/sum(n)*100)

#transitions par valeurs urbaines
OH_tab <- OH_exi_resume_cat %>% select(V_URB,n_diff_occ)
OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(V_URB) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(n_diff_occ,ptg, fill=0)%>% mutate(t=sum(`1`,`2`)) %>% View()

OH_tab <- OH_exi_resume_cat %>% select(V_URB,n_diff_dens)
OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(V_URB) %>% 
  mutate(ptg=prop.table(ptg*100)*100
         ) %>% 
  spread(n_diff_dens,ptg, fill=0)%>% mutate(t=sum(`1`,`2`,`3`))%>% View()


#transitions par portées
OH_tab <- OH_exi_resume_cat %>% select(PORTEE,n_diff_occ)
OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(PORTEE) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(n_diff_occ,ptg, fill=0) %>% mutate(t=sum(`1`,`2`))%>% View()
OH_tab <- OH_exi_resume_cat %>% select(PORTEE,n_diff_dens)
OH_tab %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(PORTEE) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(n_diff_dens,ptg, fill=0)%>% mutate(t=sum(`1`,`2`,`3`))%>% View()


#durée de vie * transitions
test_duree <- OH_over_exi %>% mutate(duree= date_fin_OH - date_debut_OH)


ggplot(test_duree, aes(x=as.factor(n_diff_occ), y=duree)) + 
  geom_boxplot(outlier.color="grey40", color="grey30")+
  stat_summary(fun.y=mean, geom="point", shape=1, size=3, color="indianred1")+ 
  theme_fivethirtyeight()+theme_ln()+
    labs(title="Durée d'existence des OH en fonction du nombre de \ntransitions de zones d'occupation du sol",
       x="nombre de transitions",
       y="durée d'existence",
       caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")
ggplot(test_duree, aes(x=as.factor(n_diff_dens), y=duree)) + 
  geom_boxplot(outlier.color="grey40", color="grey30")+
  stat_summary(fun.y=mean, geom="point", shape=1, size=3, color="indianred1")+ 
  scale_y_log10()+
  theme_fivethirtyeight()+theme_ln()+
labs(title="Durée d'existence des OH en fonction du nombre de \ntransitions de zones d'occupation du sol",
     x="nombre de transitions",
     y="durée d'existence",
     caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")

#portee* transitions
tab1 <- OH_over_exi %>% select(PORTEE,n_diff_occ) %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(PORTEE) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(n_diff_occ,ptg, fill=0)
tab2 <-OH_over_exi %>% select(PORTEE,n_diff_dens) %>% 
  na.omit() %>% 
  group_by_all() %>% 
  summarise(ptg=n()) %>% 
  group_by(PORTEE) %>% 
  mutate(ptg=prop.table(ptg*100)*100) %>% 
  spread(n_diff_dens,ptg, fill=0)




#ordre du tableau selon date debut/date_fin/autre pour affichage du plot
exi_plot <- OH_over_exi %>% mutate(OH_NUM2 = forcats::fct_reorder(.f = OH_NUM %>% as.factor(), .x = date_debut_OH))
#---- 1.Plots longitudinaux ----

###
#Création d'un plot qui représente les changements de zone d'occupation sur la durée de vie des OH
# df : tableau. Attention OHNUM2 = colonne réordonnée haut dessus
# sous_titre : sous-titre
plot_exi_occupation <- function(df, sous_titre) {
  ggplot(df)+
    geom_segment(
      aes(y=OH_NUM2, yend=OH_NUM2, x=date_debut, xend=date_fin+1, color=factor(occupation,levels=c("non urbaine","intermediaire","urbaine"))), 
      size=1.5)+
    scale_color_manual(values=c("urbaine"="#a05050", "intermediaire"="#c8ab37", "non urbaine"="darkgrey"),
                       name="Localisation de l'OH",
                       labels=c("en zone non urbaine","en zone intermédiaire", "en zone urbaine"))+
    scale_x_continuous(breaks = c(1,seq(100,2015,100)))+
    labs(title="Séquences de localisation des OH au cours de leur existence \nen fonction de l'occupation du sol",
         subtitle=sous_titre,
         x="années",
         y="OH \n(classés par date d'apparition)",
         caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(legend.position="bottom",
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_blank(),
          axis.ticks.y =element_blank(),
          axis.text.y=element_blank())
  
}
###
#Création d'un plot qui représente les changements de zone de densité sur la durée de vie des OH
# df : tableau. Attention OHNUM2 = colonne réordonnée haut dessus
# sous_titre : sous-titre
plot_exi_densite <- function(df, sous_titre) {
  ggplot(df)+
    geom_segment(
      aes(y=OH_NUM2, yend=OH_NUM2, x=date_debut, xend=date_fin+1, color=densite), 
      size=1.5)+
    scale_color_manual(values=c("3"="#016957", "2"="#3ab29d", "1"="#aaddd4", "0"="darkgrey"),
                       name="Localisation de l'OH",
                       labels=c("en zone de densité forte","en zone de densité moyenne", "en zone de densité faible", "en zone non urbaine"))+
    scale_x_continuous(breaks = c(1,seq(100,2015,100)))+
    labs(title="Séquences de localisation des OH au cours de leur existence \nen fonction de la densité densite",
         subtitle=sous_titre,
         x="années",
         y="OH \n(classés par date d'apparition)",
         caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(legend.position="bottom",
          legend.direction = "vertical",
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_blank(),
          axis.ticks.y =element_blank(),
          axis.text.y=element_blank())
}

plot_exi_occupation(exi_plot %>% filter(n_diff_occ==2), "tout")


#---- 2.plot transitions ----

df_transitions <- OH_over_exi %>% 
              select(date_debut, date_fin, date_fin_OH, occupation, densite, OH_NUM, V_USAGE, V_URB, PORTEE) %>% 
              mutate(date_transition=date_fin, lead_occ =lead(occupation), lead_dens= lead(densite)) %>% 
              mutate(trans_occ = paste(occupation,"\nà\n", lead_occ, sep=" ") %>% as.factor,trans_dens = paste(densite,"à", lead_dens, sep=" ") %>% as.factor()) %>% 
              filter(date_fin!=date_fin_OH) %>% 
              select(date_transition, occupation, lead_occ, trans_occ, densite, lead_dens, trans_dens,OH_NUM, V_USAGE, V_URB, PORTEE)


summary(df_transitions)

plot_transitions <- function(df, type_zone, sous_titre){
  if(type_zone=="occupation")
  {
    df <- df %>% filter(occupation!=lead_occ)
    titre <- "l'occupation du sol"
    wrap <- "trans_occ~."
    # couleurs <- c("urbaine"="#a05050", "intermediaire"="#c8ab37", "non urbaine"="darkgrey")
    }
  else if (type_zone=="densite"){
    df <- df %>% filter(densite!=lead_dens)
    titre <- "la densite du bâti"
    wrap <- "trans_dens~."
    # couleurs <- c("3"="#016957", "2"="#3ab29d", "1"="#aaddd4", "0"="darkgrey")
    
  }
  ggplot(data=df, aes(x=date_transition)) +
    geom_bar(stat = "count",
             width=20,
             fill="#747474")+
             # aes(fill=df[,type_zone]))+
    facet_grid(wrap, switch="y")+
    scale_y_continuous(position="right")+
    scale_x_continuous(breaks = c(1,seq(100,1900,100),2015))+
    # scale_fill_manual(values=couleurs)+
    labs(title=paste("Répartition des changements de contexte de localisation en fonction de",titre,sep=" "),
         subtitle=sous_titre,
         x="année",
         y="nombre de transitions",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln()
  
}


df_transitions %>% filter(V_USAGE==42) %>% select(trans_dens) %>% nrow()
df_transitions %>% filter(V_USAGE==42) %>% select(trans_occ) %>% summary(maxsum=20)


plot_transitions(df = df_transitions %>% filter(V_URB==5),
                 type_zone="densite",
                 sous_titre="lieux d'inhumation : valeur urbaine 5")

ggsave("transitions.svg", width = 13, height = 8, units="cm")
ggsave("transitions_42_urb.png")

