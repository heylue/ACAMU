###############################################
## Répartition des OH au cours du temps
## Traitements pour l'analyse temporelle, chapitre 3
## ACAMU : ANALYSES_TF
## L. Nahassia, 2019
## Zenodo. https://doi.org/10.5281/zenodo.3256673
##############################################

# # données : ToToPI_V3/OH_attributs
# # espace ? tous les OH même celles sans géométrie
# # fonctions ? V_USAGE 11 & >= 70)
# # temps ? -25 > 2015
# # autre : sans OH ajoutés par LN
# # compte : 1312 obs

library(ggplot2)
library(ggthemes)
library(scales)
library(gridExtra)
library(RColorBrewer)
library(tidyverse)
library(reshape2)
library(ggpubr)
library(cowplot)
source("3_themes_legendes_global.R")


#------------------------------------------ PREPARATION DONNEES ----

#Import sur selection temporelle et fonctionnelle
src <- src_sqlite("../ToToPI_V3.gpkg")
OH_source <- tbl(src, "OH_attributs") %>% 
  as_tibble() %>% 
  filter(V_USAGE != 11 & V_USAGE < 70 & DATE_FIN > -25 & (is.na(REFERENCE) | REFERENCE!="Nahassia 2019"))
OH <- OH_source


#mise en forme variables
OH$portee <- as.factor(OH$portee)
OH$v_urb <- cut (OH$V_USAGE,
                 breaks=c(0,20,30,40,50,60,70),
                 labels=c(1,2,3,4,5,6),
                 right=FALSE,
                 include.lowest = TRUE) %>% as.factor()


#------------------------------------------ CONSTRUCTION TABLEAUX ----

#----construction du tableau de contingence pour nombre ----
#---- 1.pour vurb ----
# structure du tableau
nb_vurb_an <- as.data.frame(matrix(nrow=max(OH$DATE_FIN)+26,ncol=6))
colnames(nb_vurb_an) <-c(1:6)
nb_vurb_an$annee <- as.numeric(rownames(nb_vurb_an))-26
# rownames(nb_vurb_an) <- as.numeric(rownames(nb_vurb_an))-1
#remplissage du tableau
for (n in 1:6){ # pour toutes les colonnes valeurs urbaines existantes
  for (i in 1:(nrow(nb_vurb_an)+1)) { #pour toutes les lignes
    annee <- i-26
    nb_vurb_an[i,n] <- sum (OH$v_urb==n & OH$DATE_DEB<=annee & OH$DATE_FIN>=annee)
  } # remplir cette colonne de la somme des individus ayant cette valeur urbaine et ayant existé dans les bornes données
}
nb_vurb_an <- nb_vurb_an %>%  filter(annee != 0)
nb_vurb_an$somme <- rowSums(nb_vurb_an)-nb_vurb_an$annee
# melt  pour plot
m_vurb_an <- nb_vurb_an %>% select(-somme) %>% melt(id.vars="annee")
# transformation n° des v_urb en titre complet
m_vurb_an$v_urb <- as.numeric(m_vurb_an$variable)
m_vurb_an$v_urb <- cut (m_vurb_an$v_urb,
                            breaks=c(1,2,3,4,5,6,7),
                            labels=c("1.aménagements",
                                     "2.structures défensives \n et militaires",
                                     "3.constructions civiles",
                                     "4.édifices religieux",
                                     "5.lieux d'inhumation",
                                     "6.lieux de commerce, \n artisanat, production"),
                            right=FALSE,
                            include.lowest = TRUE)

#---- 2. table % ----
pourc_vurb_an <- nb_vurb_an %>% mutate_at(vars(-somme, -annee), list(~./somme*100))
m_pourc_vurb_an <- pourc_vurb_an %>% select(-somme) %>% melt(id.vars="annee")
# transformation n° des v_urb en titre complet
m_pourc_vurb_an$v_urb <- as.numeric(m_pourc_vurb_an$variable)
m_pourc_vurb_an$v_urb <- cut (m_pourc_vurb_an$v_urb,
                                  breaks=c(1,2,3,4,5,6,7),
                                  labels=c("1.aménagements",
                                           "2.structures défensives \n et militaires",
                                           "3.constructions civiles",
                                           "4.édifices religieux",
                                           "5.lieux d'inhumation",
                                           "6.lieux de commerce, \n artisanat, production"),
                                  right=FALSE,
                                  include.lowest = TRUE)

#----3. par v_usage----
liste_vusage <- unique(OH %>% select(V_USAGE))
nb_usage_an <- as.data.frame(matrix(nrow=max(OH$DATE_FIN)+26,ncol=nrow(liste_vusage)))
colnames(nb_usage_an) <- sort(liste_vusage$V_USAGE)
nb_usage_an$annee <- as.numeric(rownames(nb_usage_an))-26
for (n in 1:nrow(liste_vusage)){
  for (i in 1:nrow(nb_usage_an)+1) {
    annee <- i-26
    v_usage <- colnames(nb_usage_an[n])
    nb_usage_an[i,n] <- sum (OH$V_USAGE==v_usage & OH$DATE_DEB<=annee & OH$DATE_FIN>=annee)
  }
}
nb_usage_an <- nb_usage_an %>%  filter(annee != 0)
nb_usage_an$somme <- rowSums(nb_usage_an)-nb_usage_an$annee
pourc_usage_an <- nb_usage_an %>% mutate_at(vars(-somme, -annee), list(~./somme*100)) #attention au nombre de colonnes

#melt
m_usage_an <- nb_usage_an %>% melt(id.vars="annee")


#melt > uniquement sur les v_usage qui nous intéresse, ici VURB==3 avec toutes les vus ou en 2 catégorie (habitat/pas habitat)
m_civil_an <- nb_usage_an %>% select(starts_with("3"),-somme,"annee") %>% melt(id.vars="annee")
m_pourc_civil_an <- pourc_usage_an %>% select(starts_with("3"),-somme,"annee") %>% melt(id.vars="annee")
m_civil_an$v_usage <- m_civil_an$variable
levels(m_civil_an$v_usage) <- c("Non spécifiées", #changer selon vusag
                                "Espaces\n Publics", 
                                "Pouvoir civil,\n justice", 
                                "Santé", 
                                "Spectacles,\n sports",
                                "Bains,\n thermes", 
                                "Habitat")
m_pourc_civil_an$v_usage <- m_pourc_civil_an$variable
levels(m_pourc_civil_an$v_usage) <- c("Non spécifiées", #changer selon vusag
                                      "Espaces\n Publics", 
                                      "Pouvoir civil,\n justice", 
                                      "Santé", 
                                      "Spectacles,\n sports",
                                      "Bains,\n thermes", 
                                      "Habitat")

somme_sans_hab <- nb_usage_an %>% select(grep("^3", colnames(nb_usage_an))) %>% select(-"37") %>% rowSums()
nb_civil_an_2c <- cbind(somme_sans_hab,nb_usage_an %>% select("37","annee"))
colnames(nb_civil_an_2c) <- c("autres constructions civiles (30-36)","habitat (37)", "annee")
somme_pourc_sans_hab <- pourc_usage_an%>% select(grep("^3", colnames(pourc_usage_an))) %>% select(-"37") %>% rowSums()
pourc_civil_an_2c <- cbind(somme_pourc_sans_hab, pourc_usage_an %>% select("37","annee"))
colnames(pourc_civil_an_2c) <- c("autres constructions civiles (30-36)","habitat (37)", "annee")
m_civil_an_2c <- melt(nb_civil_an_2c, id.vars="annee")
m_pourc_civil_an_2c <- melt(pourc_civil_an_2c, id.vars="annee")


#----4. par portées----
nb_portee_an <- as.data.frame(matrix(nrow=max(OH$DATE_FIN)+26,ncol=4))
colnames(nb_portee_an) <-c(1:4)
nb_portee_an$annee <- as.numeric(rownames(nb_portee_an))-26
for (n in 1:4){
  for (i in 1:(nrow(nb_portee_an+1))) {
    annee <- i-26
    nb_portee_an[i,n] <- sum (OH$PORTEE==n & OH$DATE_DEB<=annee & OH$DATE_FIN>=annee)
  } 
}
nb_portee_an <- nb_portee_an %>%  filter(annee != 0)
nb_portee_an$somme <- rowSums(nb_portee_an)-nb_portee_an$annee
m_portee_an <- nb_portee_an %>% select(-somme) %>% melt(id.vars="annee")
m_portee_an$portee <- m_portee_an$variable
levels(m_portee_an$portee) <- c("1 : portée locale",
                                "2 : portée moyenne ",
                                "3 : grande portée",
                                "4 : portée exceptionnelle")
pourc_portee_an <- nb_portee_an %>% mutate_at(vars(-somme, -annee), list(~./somme*100))
m_pourc_portee_an <- pourc_portee_an %>% select(-somme) %>% melt(id.vars="annee")
m_pourc_portee_an$portee <- m_pourc_portee_an$variable
levels(m_pourc_portee_an$portee) <- c("1 : portée locale",
                                      "2 : portée moyenne ",
                                      "3 : grande portée",
                                      "4 : portée exceptionnelle")


# #tables vurb+vusage+portee pour explOH
# nb_OH_annee <- left_join(
#   nb_vurb_an %>% select(annee, '1','2','3','4','5','6',somme) %>% rename_all(list(~paste0("vurb_",.))), 
#   nb_portee_an %>% rename_all(list(~paste0("portee_",.))), 
#   by=c("vurb_annee"="portee_annee")
# ) %>% 
#   left_join(., nb_usage_an %>% rename_all(list(~paste0("vusage_",.))),
#             by=c("vurb_annee"="vusage_annee")
#             ) %>% rename("annee"="vurb_annee")
# 
# pourc_OH_annee <- left_join(
#   pourc_vurb_an %>% select(annee, '1','2','3','4','5','6',somme) %>% rename_all(list(~paste0("vurb_",.))), 
#   pourc_portee_an %>% rename_all(list(~paste0("portee_",.))), 
#   by=c("vurb_annee"="portee_annee")
# ) %>% 
#   left_join(., pourc_usage_an %>% rename_all(list(~paste0("vusage_",.))),
#             by=c("vurb_annee"="vusage_annee")
#   ) %>% rename("annee"="vurb_annee")
# 
# write.csv(nb_OH_annee,"nb_OH_annee.csv")
# write.csv(pourc_OH_annee,"pourc_OH_annee.csv")


#------------------------------------------ PLOTS ----

#---- répartition oui/non dans le temps ----
AP_vurb_an <- nb_vurb_an %>% mutate('1'=ifelse(`1`==0, "0", "1.aménagements"),
                                            '2'=ifelse(`2`==0, "0", "2.structures défensives \n et militaires"),
                                            '3'=ifelse(`3`==0, "0", "3.constructions civiles"),
                                            '4'=ifelse(`4`==0, "0", "4.édifices religieux"),
                                            '5'=ifelse(`5`==0, "0", "5.lieux d'inhumation"),
                                            '6'=ifelse(`6`==0, "0", "6.lieux de commerce, \n artisanat, production")
                                            )
AP_vurb_m <- melt(AP_vurb_an %>% select(-somme), id="annee")
ggplot(AP_vurb_m %>% filter(value!="0"), aes(x=annee, y=value))+
  geom_point(aes(color=value))+
  scale_color_manual(values=colors_vu)+
  labs(
       x="année",
       y="valeur urbaines",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()
  


AP_usages_an <- nb_usage_an %>% mutate('12'=ifelse(`12`==0, "0","12" ),
                                        '13'=ifelse(`13`==0, "0","13" ),
                                        '14'=ifelse(`14`==0, "0","14" ),
                                        '15'=ifelse(`15`==0, "0","15" ),
                                        '16'=ifelse(`16`==0, "0","16" ),
                                        '17'=ifelse(`17`==0, "0","17" ),
                                        '18'=ifelse(`18`==0, "0","18" ),
                                        '19'=ifelse(`19`==0, "0","19" ),
                                        '21'=ifelse(`21`==0, "0","21" ),
                                        '22'=ifelse(`22`==0, "0","22" ),
                                        '30'=ifelse(`30`==0, "0","30" ),
                                        '31'=ifelse(`31`==0, "0","31" ),
                                        '32'=ifelse(`32`==0, "0","32" ),
                                        '34'=ifelse(`34`==0, "0","34" ),
                                        '35'=ifelse(`35`==0, "0","35" ),
                                        '36'=ifelse(`36`==0, "0","36" ),
                                        '37'=ifelse(`37`==0, "0","37" ),
                                        '41'=ifelse(`41`==0, "0","41" ),
                                        '42'=ifelse(`42`==0, "0","42" ),
                                        '43'=ifelse(`43`==0, "0","43" ),
                                        '44'=ifelse(`44`==0, "0","44" ),
                                        '51'=ifelse(`51`==0, "0","51" ),
                                        '52'=ifelse(`52`==0, "0","52" ),
                                        '53'=ifelse(`53`==0, "0","53" ),
                                        '61'=ifelse(`61`==0, "0","61" ),
                                        '62'=ifelse(`62`==0, "0","62" ),
                                        '63'=ifelse(`63`==0, "0","63" ),
                                        '65'=ifelse(`65`==0, "0","65" ))
AP_usages_m <- melt(AP_usages_an %>% select(-somme), id="annee")
ggplot(AP_usages_m %>% filter(value!="0"), aes(x=annee, y=value))+
  geom_point()+
  scale_color_manual(values=colors_vu)+
  labs(
    x="année",
    y="valeur urbaines",
    caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()


#----nombre d'OH total dans le temps ----
#rajouter un %>%  filter (variable==X) ou (année >)pour faire un zoom sur certaines activités/périodes
#absisse à ajuster en fonction du zoom temporel
ggplot(nb_vurb_an, aes(x=annee, y=somme))+
  geom_area(color=adjustcolor("grey20", alpha=0.5), fill=adjustcolor("grey20", alpha=0.2))+
  geom_hline(yintercept = 0, color ="grey75")+
  # geom_bar(stat = "identity",width=1, fill=adjustcolor("grey20", alpha=0.6))+
  labs(title="Nombre d'OH par année",
       x="année",
       y="nombre d'OH",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))+
  scale_x_continuous(breaks = c(1,seq(100,2000,100)))

ggsave("repartition_tout.svg")


#----nombre/% d'OH dans le temps tout superposé ----
plottout <- ggplot(m_vurb_an, aes(x=annee, y=value, color=rev(variable)))+
  geom_line()+
  geom_hline(yintercept = 0, color ="grey75")+
  scale_color_manual(labels=rev(unique(m_vurb_an$v_urb)), values=adjustcolor(rev(colors_vu), alpha=0.9))+
  labs(title="Par du nombre d'OH par année et par valeur urbaine",
       x="année",
       y="nombre d'OH",
       color="Valeurs urbaines",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    legend.position="bottom",
    axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))
ggsave("repartition_tout_vurb.png")


#----nombre d'OH dans le temps facettes vurbaines ----
plot5 <- ggplot(m_vurb_an, aes(x=annee, y=value))+
  geom_area(aes(fill=rev(m_vurb_an$variable)))+
  geom_line(aes(color=rev(m_vurb_an$variable)))+
  geom_hline(yintercept = 0, color ="grey75")+
  scale_color_manual(values=adjustcolor(rev(colors_vu), alpha=0.9))+
  scale_fill_manual(values=adjustcolor(rev(colors_vu), alpha=0.4))+
  facet_wrap(~ v_urb, nrow=2, scales = "free_x")+
  labs(title="Nombre d'OH par année et par valeur urbaine",
       x="année",
       # caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, ToToPI, CITERES-LAT", #pas de caption pour planche
       y="nombre d'OH")+
  scale_x_continuous(breaks = c(1,seq(200,2000,200)))+ #ajuster ici pour zoomdes constructions civiles
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1),
    plot.margin = unit(c(0.5,1,1.5,1), "lines"))
#----% d'OH dans le temps facettes urbaines ----
plot6 <- ggplot(m_pourc_vurb_an, aes(x=annee, y=value))+
  geom_area(aes(fill=rev(m_vurb_an$variable)))+
  geom_line(aes(color=rev(m_vurb_an$variable)))+
  geom_hline(yintercept = 0, color ="grey75")+
  scale_color_manual(values=adjustcolor(rev(colors_vu), alpha=0.9))+
  scale_fill_manual(values=adjustcolor(rev(colors_vu), alpha=0.4))+
  facet_wrap(~ v_urb, nrow=2, scales="free_x")+
  labs(title="Part de chaque type d'OH par rapport \nau nombre total d'OH par année et par valeur urbaine",
       x="année",
       y="% d'OH",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, ToToPI, CITERES-LAT")+
  scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
  scale_y_continuous(limits=c(0,100))+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))


#----planche nombre + % ----
repartition_planche <- arrangeGrob(plot5,plot6,nrow=2) %>% as_ggplot() +
  draw_plot_label(label=c("A","B"), size = 12, x=c(0.01,0.01), y=c(0.99,0.49))

png("repartition_planche.png",width=15.5, height =24, units="cm", res=300)
grid.arrange(repartition_planche)
dev.off()


#---- planche en n&b pour zoom ----
plot7 <- ggplot(m_vurb_an %>% filter(variable==6), aes(x=annee, y=value))+
  geom_area(fill=adjustcolor("grey60", alpha=0.4))+
  geom_line(color=adjustcolor("grey60", alpha=0.9))+
  geom_hline(yintercept = 0, color ="grey75")+
  facet_wrap(~ variable,scales = "free_x")+
  labs(title="Nombre d'OH par année et par valeur urbaine",
       # caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, ToToPI, CITERES-LAT", #pas de caption pour planche
       x="année",
       y="nombre d'OH")+
  scale_x_continuous(breaks = c(1,seq(200,2000,200)))+ #ajuster ici pour zoomdes constructions civiles
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1),
    plot.margin = unit(c(0.5,1,1.5,1), "lines"))

plot8 <- ggplot(m_pourc_vurb_an %>% filter(variable==6), aes(x=annee, y=value))+
  geom_area(fill=adjustcolor("grey60", alpha=0.4))+
  geom_line(color=adjustcolor("grey60", alpha=0.9))+
  geom_hline(yintercept = 0, color ="grey75")+
  facet_wrap(~ variable,scales="free_x")+
  labs(title="Part de chaque type d'OH par rapport \nau nombre total d'OH par année et par valeur urbaine",
       x="année",
       y="% d'OH",
       caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, ToToPI, CITERES-LAT")+
  scale_x_continuous(breaks = c(1,seq(200,2000,200)))+
  scale_y_continuous(limits=c(0,100))+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))

repartition_zoom <- arrangeGrob(plot7,plot8,nrow=2) %>% as_ggplot() +
  draw_plot_label(label=c("A","B"), size = 12, x=c(0.01,0.01), y=c(0.99,0.49))
png("repartition_zoom.png",width=14, height =15, units="cm", res=600)
grid.arrange(repartition_zoom)
dev.off()



#---- détail en valeur d'usage pour VURB==3 (vaut pour autres vurbs à condition de changer les labels)----
#histogramme pour VURB==3
plotA <- ggplot(data=OH %>% filter(v_urb==2), aes(x=as.factor(V_USAGE), fill=V_USAGE)) +
  geom_bar()+
  scale_y_continuous(breaks = seq(0, 400, 100))+
  # scale_x_discrete(labels=c("Non spécifiées", "Espaces\n Publics", "Pouvoir civil,\n justice", "Santé", "Spectacles,\n sports", "Bains,\n thermes", "Habitat"))+
  labs(title="Répartition des OH de type \"construction civile\" (valeur urbaine 3) \nen valeurs d'usages",
       x="Valeurs d'usage",
       y="nombre d'OH")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1),
    plot.margin = unit(c(0.5,1,1.5,1), "lines"))

#nombre d'OH habitat dans le temps 
plotB <- ggplot(m_civil_an %>% filter(v_usage=="Habitat"), aes(x=annee, y=value))+
  geom_bar(stat = "identity",width=1,fill=adjustcolor("#f4cc1a", alpha=0.8))+
  labs(title="Nombre d'OH type \"habitat\" \n(valeur d'usage 37) \npar année ",
       x="année",
       y="nombre d'habitat civil")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))+
  scale_x_continuous(breaks = c(1,seq(250,2000,250)))+
  scale_y_continuous(limits = c(NA,230), breaks=seq(0,300,50))

#Par d'OH habitat dans le temps 
plotC<- ggplot(m_pourc_civil_an %>% filter(v_usage=="Habitat"), aes(x=annee, y=value))+
  geom_bar(stat = "identity",width=1,fill=adjustcolor("#f4cc1a", alpha=0.8))+
  labs(title="Part d'OH type \"habitat\" \n(valeur d'usage 37) par rapport au \nnombre total d'OH par année",
       x="année",
       y="nombre d'habitat civil")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1),
    plot.title = element_text(size=9)
  )+
  scale_x_continuous(breaks = c(1,seq(250,2000,250)))


#planche OH VURB==3
arrangeGrob(plotA, plotB, plotC,
            nrow=2, 
            layout_matrix=rbind(c(1,1),
                                c(2,3))) %>% 
  as_ggplot() +
  draw_plot_label(label=c("A","B","C"), size = 12, x=c(0.01,0.01,0.52), y=c(0.99,0.49,0.49))+
  labs( caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")#marche pas ajouter caption


#---- pour les portées ----
# nombre
plot3  <- ggplot(m_portee_an, aes(x=annee, y=value, fill=rev(variable)))+
  geom_bar(stat = "identity",width=1)+
  scale_fill_manual(values=adjustcolor(rev(colors_p), alpha=0.8))+
  facet_wrap(~ portee, nrow=1, scales = "free_x")+
  labs(title="Nombre d'OH par année et par portée",
       x="année",
       y="nombre d'OH")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1),
        plot.margin = unit(c(0.5,1,1.5,1), "lines"))+
  scale_x_continuous(breaks = c(1,seq(250,2000,250)))

#%
plot4 <- ggplot(m_pourc_portee_an, aes(x=annee, y=value, fill=rev(variable)))+
  geom_bar(stat = "identity",width=1)+
  scale_fill_manual(values=adjustcolor(rev(colors_p), alpha=0.8))+
  facet_wrap(~ portee, nrow=1, scales = "free_x")+
  labs(title="Part de chaque type d'OH par rapport au nombre total d'OH par année et par portée",
       x="année",
       y="% d'OH",
       caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))+
  scale_x_continuous(breaks = c(1,seq(250,2000,250)))

#planche portées
arrangeGrob(plot3,plot4,nrow=2) %>% as_ggplot() +
  draw_plot_label(label=c("A","B"), size = 12, x=c(0.01,0.01), y=c(0.99,0.49))


