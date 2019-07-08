###############################################
## Exploration factorielle des données : AFC et CAH
## Traitements pour l'exploration temporelle, chapitre 4
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


#library
library(ade4)
library(RColorBrewer)
library(wesanderson)
library(dichromat)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(lemon)
library(extrafont)
library(reshape2)
library(grid)
library(stringr)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(explor)
library(ggdendro)
library(plotly)
source("4_global_fonctions_AFC_CAH.R", encoding="UTF8")
source("4_themes_legendes_gobal.R", encoding="UTF8")

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

#récupération liste vusage
EF <- dbGetQuery(db, "select * from EF")

liste_vusage <- EF[EF$Code_EF %in% unique(OH_source$V_USAGE), c("Code_EF", "V_usage")]#pour le moment car nom_usages pas bon pour 41 et 43 (?)
liste_vusage$nom_num <- paste(sub_vusage$Code_EF,
                            ".", 
                            sub_vusage$V_usage, #nom vurb avec capitale
                            sep="")
nom_vurb <- c("1.aménagement",
               "2.structures défensives et militaires",
               "3.constructions civiles",
               "4.édifices religieux",
               "5.lieux d'inhumation",
               "6.lieux de commerce, artisanat, production")
code_vurb <- c(1,2,3,4,5,6)
liste_vurb <- data.frame("nom_num"=nom_vurb, "Code_EF"=code_vurb)



#------------------------------------------ PREPARATION TABLEAUX DE CONTINGENCE-----

#sequences temporelles
seq25 <- c(seq(-25,2015,25), 2016) #borne supérieure exclue, 
seq25[seq25 == 0] <- 1 #0 --> 1
seq50 <- c(seq(-25,2015,50), 2016) #borne supérieure exclue
seq100 <-c(seq(-25,2015,100), 2016) #borne supérieure exclue
seqexp <- c(-25,250,471,750,903,1000,1100,1356,1622,1760,1840,1950,2016) #borne supérieure exclue

#creation tableaux (fonction dans 6_2_global_fonctions_afc)
tab25  <- creation_tab_cont(seq25)
tab50  <- creation_tab_cont(seq50)
tab100  <- creation_tab_cont(seq100)
tabexp  <- creation_tab_cont(seqexp)


#------------------------------------------ TEST KHI2-----
#creation des tableaux
tab_urb_25 <- tab25 %>% select(starts_with("v_urb"))
tab_urb_50 <- tab50 %>% select(starts_with("v_urb"))
tab_urb_100 <- tab100 %>% select(starts_with("v_urb"))
tab_urb_exp <- tabexp %>% select(starts_with("v_urb"))
tab_usage_25 <- tab25 %>% select(starts_with("v_usage"))
tab_usage_50 <- tab50 %>% select(starts_with("v_usage"))
tab_usage_100 <- tab100 %>% select(starts_with("v_usage"))
tab_usage_exp <- tabexp %>% select(starts_with("v_usage"))
tab_portee_25 <- tab25 %>% select(starts_with("portee"))
tab_portee_50 <- tab50 %>% select(starts_with("portee"))
tab_portee_100 <- tab100 %>% select(starts_with("portee"))
tab_portee_exp <- tabexp %>% select(starts_with("portee"))


#KHI2 | p <= 0.01 : rejet indépendance
liste_tabs <- list(tab_urb_25, tab_urb_50, tab_urb_100, tab_urb_exp, tab_usage_25, tab_usage_50, tab_usage_100, tab_usage_exp, tab_portee_25, tab_portee_50, tab_portee_100, tab_portee_exp)
val_khi2 <- sapply(liste_tabs, function(x) chisq.test(x)$statistic)
val_ddl <- sapply(liste_tabs, function(x) chisq.test(x)$parameter)
val_pvalue <- sapply(liste_tabs, function(x) chisq.test(x)$p.value)
test_khi2 <- as.character(val_pvalue<0.01)
tab_khi2 <- rbind(round(val_khi2), val_ddl, val_pvalue, test_khi2) %>% as.data.frame()
rownames(tab_khi2) <- c("khi-deux","degré de liberté","p.value","indépendance ?")
colnames(tab_khi2)<- c("tab_urb_25", "tab_urb_50", "tab_urb_100", "tab_urb_exp", "tab_usage_25", "tab_usage_50", "tab_usage_100", "tab_usage_exp", "tab_portee_25", "tab_portee_50", "tab_portee_100", "tab_portee_exp")
#hypothèse nulle rejetée pour toutes les combinaisons


#------------------------------------------ AFC CALCUL et CHOIX AXES -----
tab_AFC <- tab_portee_exp #insérer ici le tableau de contingence pour l'AFC
stitre <- "tablea L [portées * périodes expertes]"

AFC <- dudi.coa(tab_AFC, scannf=FALSE, nf=6)
# explor(AFC) # pour exploration interactive

#tab des valeurs propres
sumAFC_OH <- summary.variance.dudi(AFC)

# ---- graphique valeurs propres---- 
## Entrée : data = resultat dudi.coa,umdata = résultat summary.variance.dudi, titre=titre du graphique, soustitre = description type AFC (rien par défaut)
axes_barplot <- barplot.dudi.variance(data=AFC,
                                      sumdata=sumAFC_OH,
                                      titre="Variance expliquées par les axes",# à changer en fonction de l'AFC 
                                      soustitre=stitre
)+
  theme(legend.position ="none" )

#---- graphique valeurs propres toutes AFC confondues ----
#toutes les variances pour figure manuscrit
afc_tout <- rbind(
  dudi.coa(tab_usage_25, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_usage_50, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_usage_100, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_usage_exp, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_urb_25, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_urb_50, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_urb_100, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_urb_exp, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_portee_25, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_portee_50, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_portee_100, scannf=FALSE, nf=6) %>% summary.variance.dudi,
  dudi.coa(tab_portee_exp, scannf=FALSE, nf=6) %>% summary.variance.dudi
)


sum_tout <- rbind(  
  dudi.coa(tab_usage_25, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="valeur d'usage", periode="25 ans"),
  dudi.coa(tab_usage_50, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="valeur d'usage", periode="50 ans"),
  dudi.coa(tab_usage_100, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="valeur d'usage", periode="100 ans"),
  dudi.coa(tab_usage_exp, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="valeur d'usage", periode="périodes expertes"),
  dudi.coa(tab_urb_25, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="valeur urbaine", periode="25 ans"),
  dudi.coa(tab_urb_50, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="valeur urbaine", periode="50 ans"),
  dudi.coa(tab_urb_100, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="valeur urbaine", periode="100 ans"),
  dudi.coa(tab_urb_exp, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="valeur urbaine", periode="périodes expertes"),
  dudi.coa(tab_portee_25, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="portee", periode="25 ans"),
  dudi.coa(tab_portee_50, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="portee", periode="50 ans"),
  dudi.coa(tab_portee_100, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="portee", periode="100 ans"),
  dudi.coa(tab_portee_exp, scannf=FALSE, nf=6) %>% summary.variance.dudi %>% mutate(couleur=PCTVAR>100/max(dudi.coa(tab_usage_25, scannf=FALSE, nf=6)$rank),fonction="portee", periode="périodes expertes")
)

variance_tout <- ggplot(data=sum_tout, aes(x=COMP, y=PCTVAR, fill=couleur)) +
    geom_bar(stat="identity")+
    labs(x="Axes",
         y="Part de la variance expliquée",
         # title="Taux d'inertie des axes des AFC",
         # subtitle="pour les 12 tableaux de contingence",
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT",
         fill="Taux d'inertie des axes")+
    facet_wrap(fonction~periode,nrow=3, ncol=4,scales="free")+
    scale_fill_manual(values=alpha(c("grey60","darkslategray3"), 0.8), labels=c("Inférieur au taux d'inertie moyen (100/nombre d'axes)", "Supérieur au taux d'inertie moyen (100/nombre d'axes)")) +
    theme_fivethirtyeight()+
    theme_ln()+
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          strip.text = element_text(size=7, margin=margin(0.1, 0, 0.1, 0, "cm")))


png(filename = "variance_AFCs.png",
    unit="cm",
    width=15.5,
    height = 16,
    res=600)
variance_tout
dev.off()




### Résultats de l'AFC
# AFC_OH$li # coordonnées des lignes sur chaque axe
# AFC_OH$co # coordonnées des colonnes sur chaque axe
# AFC_OH$l1 # individu "normed score"
# AFC_OH$c1 # variable "normed score" = "les axes principaux" >> pour faire le graphique avec le cercle
# AFC_OH$eig # inertie des axes >> en % 100*AFC_OH$eig / sum(AFC_OH$eig)
# AFC_OH

#---------------------------------------------------------
#----------- AFC biplots & contributions  ----------------
#---------------------------------------------------------

#---- Choix Variable ----

### CHOIX COULEUR : CHOISIR LA BONNE VARIABLE EN FONCTION DU TABLEAU DE CONTINGENCE
couleurs_choix <- couleurs_vurb
couleurs_choix <- couleurs_vusage
couleurs_choix <- couleurs_portee

#### CHOIX NOM VARIABLES : CHOISIR LA BONNE VARIABLE EN FONCTION DU TABLEAU DE CONTINGENCE
liste_var <- sort(unique(OH$v_urb))
liste_var <- sort(unique(OH$V_USAGE))
liste_var <- sort(unique(OH$portee))

### CHOIX MARGE DU HAUT & REPULSION TEXT_REPEL: CHOIX EN FONCTION DU TABLEAU DE CONTINGENCE
marge <- NULL #portéees
marge <- 0.5
marge_texte <- 0.5
marge_texte <- 0.8


#---- BIPLOT ----
# png(filename="AFC_100a_vurbaine.png", width=1000, height=1000, res=150) #décommenter pour enregistrer en png
# Entrée : data = resultat dudi.coa + sumdata = résultat summary.variance.dudi

# 1. plot pour manuscrit----
# à changer en fonction de l'AFC
titre <- "Répartition des variables sur le premier axe de l'AFC"

#lignes seules----
plot_AFC <- biplot.periodes.AFC(data=AFC,
                                axe=list(1,2), # à changer en fonction de l'AFC
                                titre=NULL,
                                soustitre=stitre,
                                marge=marge,
                                position="haut"
)+labs(caption=NULL)

#lignes et colonnes----
plot_AFC <- biplot.AFC(data=AFC, 
                       axe=list(1,2), # à changer en fonction de l'AFC
                       variable_color=couleurs_choix, 
                       liste_variable=liste_var,
                       titre=titre,
                       soustitre=stitre,
                       marge=marge,
                       type="valeurs d'usage", # a changer en fonction de l'AFC
                       marge_texte = marge_texte
)

#pour figure manuscrit----
#plot_AFC + scale_y_reverse() # pour périodes historiques
png(filename = "biplot.png",
    type="cairo",
    units="cm",
    width=17,#18.26
    height = 17.5, #12.88 pour les planches de biplots
    pointsize = 10,
    res=300)
plot_AFC + 
  # scale_y_reverse()+ #pour 
  theme(axis.title=element_text(size=9),
        axis.text=element_text(size=9),
        plot.subtitle = element_text(size=10))
dev.off()


#2.plot pour exploration (proche exploR) ----
biplot_col <- setNames(AFC$co, names(AFC$li))
biplot_col$type_var <- "caractéristiques fonctionnelles"
biplot_col$type_rep <-rep("points")
biplot_col$color_var <- str_sub(row.names(biplot_col), start=-1)
biplot_li <- AFC$li
biplot_li$type_var <- "périodes temporelles"
biplot_li$type_rep <- "périodes temporelles"
biplot_li$color_var <- "temps"
tab_biplot <- rbind(biplot_col,biplot_li)


scatterD3(x=tab_biplot$Axis1, y=tab_biplot$Axis2,
          lab=row.names(tab_biplot),
          symbol_var = tab_biplot$type_var,
          # symbol_rep = tab_biplot$type_var,
          col_var = tab_biplot$color_var,
          colors=c("temps"="grey", "1"=colors_vu[1], "2"=colors_vu[2], "3"=colors_vu[3], "4"=colors_vu[4], "5"=colors_vu[5],"6"=colors_vu[6]),
          point_opacity = 0.9,
          hover_opacity = 1,
          hover_size = 2,
          xlab="Axe 1",
          ylab="Axe 2",
          symbol_lab="variables en lignes et colonnes",
          col_lab="type de variable"
)

# dev.off()#décommenter pour enregistrer en png

#---- CONTRIBUTIONS ----
contrib_AFC_OH <- inertia.dudi(AFC,row.inertia = TRUE, col.inertia = TRUE)
# contrib_AFC_OH
# contrib_AFC_OH$TOT #inertie des axes
# contrib_AFC_OH$row.abs #contribution des lignes à l'inertie des axes (part de la variance expliquée par l'individu), somme en colonne
# contrib_AFC_OH$col.abs #idem colonne
# contrib_AFC_OH$row.rel #contribution de l'axe à la variance de la ligne (cos²), qualité de représentation de l'individu, somme en ligne
# contrib_AFC_OHcol.rel #idem colonne => pour les deux à mettre en absolu

#sortie table avec couleurs :
#contribution lignes
CTR_li <- contrib_AFC_OH$row.abs
CTR_li <- CTR_li %>% select(c("Axis1(%)", "Axis2(%)")) #que les axes analysés > a changer selon
colnames(CTR_li) <- c("Axe 1", "Axe 2")
#contrribution colonnes
CTR_co <- contrib_AFC_OH$col.abs
CTR_co <- CTR_co %>% select(c("Axis1(%)", "Axis2(%)"))#que les axes analysés
colnames(CTR_co) <- c("Axe 1", "Axe 2")
#plot # Entrée data = tableau à préparer dessus, titre = titre, couleur = couleur, gestion date : si périodes tri dans l'ordre chronologique les vecteur
pligne <- heat.contribvar(data=CTR_li, 
                          titre="Contribution des périodes \n dans la variance des axes", 
                          soustitre=stitre,
                          couleur="mediumturquoise", 
                          gestion_dates=TRUE)+theme(legend.position = "none")
pcol <- heat.contribvar(CTR_co, 
                        titre="Contribution des valeurs urbaines \n dans la variance des axes", 
                        soustitre=stitre,
                        couleur="mediumturquoise", 
                        gestion_dates=FALSE)+theme(legend.position = "none")

#planche contributions----
arrangeGrob(
  pligne+
    labs(caption="", title=NULL, subtitle=NULL)+
    theme(legend.position = "none",
          plot.background = element_rect(fill="white"),
          plot.margin = unit(c(0,1,1,0),"lines")), 
  pcol+
    labs(caption="", title=NULL, subtitle=NULL)+
    theme(legend.position = "none",
          plot.background = element_rect(fill="white"),
          plot.margin = unit(c(0,1,1,1),"points")),
  ncol=2,
  top=textGrob("Contribution des variables à \n l'inertie des deux premiers axes ",
               gp=gpar(fontsize=9, fontface="bold"),
               hjust=0,
               x=0.1),
  # sub=stitre,
  layout_matrix=rbind(c(1,2), # à ajuster selon taille tableaux
                      # c(1,2),
                      # c(1,NA),
                      # c(1,NA),
                      # c(1,NA),
                      c(1,NA))
) %>% as_ggplot()


#planche tous résultats----
arrangeGrob(
  axes_barplot+labs(caption="")+theme(legend.position = "none"),
  pligne+labs(caption="")+theme(legend.position = "none"), 
  pcol+labs(caption=""),
  plot_AFC,
  nrow=3,
  layout_matrix=rbind(c(1,1,1,2),
                      c(4,4,4,2),
                      c(4,4,4,3))) %>%
  as_ggplot() +
  draw_plot_label(label=c("A","B","C","D"), size = 11, x=c(0.05,0.05,0.75,0.75), y=c(0.99,0.66,0.99,0.35))


#---- COS2 ligne (axe explique comment l'individu) ----
#préparation des données hors fonction
#suppression de la dernière colonne inutile
cos2_li <- subset(abs(contrib_AFC_OH$row.rel), select = - con.tra)
#mettre des noms corrects
colnames(cos2_li) <- c("Axe 1", "Axe 2", "Axe 3", "Axe 4", "Axe 5")
#COS2colonne (axe explique comment l'individu)
cos2_col <- subset(abs(contrib_AFC_OH$col.rel), select = - con.tra)
colnames(cos2_col) <- c("Axe 1", "Axe 2", "Axe 3", "Axe 4", "Axe 5")
rownames(cos2_col) <-  c("voiries, aménagements","structures défensives et militaires","constructions civiles","édifices religieux","lieux d'inhumation","lieux de commerce, artisanat, production")
heat.contribvar(cos2_col, "Part de la variance des variables-valeurs_urbaines exprimée par chaque axe", "turquoise")



#------------------------------------------
#--------------------- CAH ----------------
#------------------------------------------

#----- fonctions pour différents calculs >> code Gravier, Julie. (2018, April). AA2018 : analyser et regrouper des périodes chronologiques en archéologie avec R (Version 1.0). Zenodo. http://doi.org/10.5281/zenodo.1222190
source("6_2_Fonctions_VisuClasses.R")

CAH_AFC <- tab_AFC %>% CAH_DistKhi2()


#------ 1. choix du nombre de classes ----

#inertie
inertie_CAH <- sort(CAH_AFC$height, decreasing = TRUE)
inertie_CAH_part <- inertie_CAH/sum(inertie_CAH)*100
nombre_noeuds <- 20 #nombre de noeuds à représenter sur le diagramme
inertie_plot <- inertie_CAH_part[1:nombre_noeuds]

inert_CAH <- ggplot()+
  geom_bar(
    aes(x=c(1:nombre_noeuds),y=inertie_plot),
    stat="identity",
    fill="grey40",
    alpha=0.8)+
  scale_x_continuous(breaks=seq(1,nombre_noeuds,1), expand=expand_scale(add=0.5))+
  labs(
    title="Inertie relative intra-classe de la CAH pour les 20 premiers nœuds : \ndécoupage en 7 classes (nœud n°6) ",
    subtitle="sur AFC A [valeurs urbaines * 25 ans] - aggrégation selon le critère de Ward",
    caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES",
    x="noeuds",
    y="Part de l'inertie totale (%)")+
  theme_fivethirtyeight()+
  theme_ln()

#dendrogramme CAH
titre_dendro <-"Dendrogramme de la CAH : découpage en 7 classes (nœud n°6)"
trace("ggdendrogram", edit=TRUE) #modification expand dans le package > " expand=c(0.01,0.01) dans scale_y et expand=c(0,0)dans scale_x
dendro <- ggdendrogram(CAH_AFC)+
  coord_flip()+
    labs(
    title=titre_dendro,
    y="",
    x="",
    subtitle="sur AFC A [valeurs urbaines * 25 ans] - aggrégation selon le critère de Ward",
    caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    axis.text.y = element_text(size=6),
    panel.grid.major.y=element_blank(),
    panel.border = element_blank())

#les deux plots ensembles

compo_cah <- arrangeGrob(
  inert_CAH+labs(caption=""),
  dendro, 
  nrow=4,
  layout_matrix=rbind(c(1),
                      c(2),
                      c(2),
                      c(2))) %>%
  as_ggplot()

png(filename = "compo_CAH.png",
    unit="cm",
    width=16,
    height =25,
    res=600)
compo_cah
dev.off()

#pour rédaction ajouter rectangles

#---- 2. variabilité des classes ----

nombre_classes <- 7 #choix nombre classes


flip <- classes.periodes.cah(CAH=CAH_AFC, nb=nombre_classes)
classes_periodes <- flip$entier
dates_axe <- flip$axe

frise <- ggplot(classes_periodes) +
  geom_segment(aes(x=deb, xend=fin, y=0., yend=0., color=classes) , linetype=1, size=6) +
  # geom_text(data=dates_axe,
  #           aes(x=deb,
  #               y=0,
  #               label=classes),
  #           color="white",
  #           nudge_x = +15)+
  # scale_color_manual(values=brewer.pal(n=nrow(dates_axe),name="Set3"))+
  # scale_y_continuous(limits= c(0,0.5))+
  scale_color_manual(values=palette_CAH(nombre_classes))+
  scale_x_continuous(breaks=c(dates_axe$deb,2016),expand=expand_scale(add=0))+
  labs(colour=paste("classes de la CAH",stitre,sep="\n"))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major =  element_blank(),
        panel.border = element_blank(),
        text = element_text(colour="#3C3C3C"),
        axis.text.x = element_text(size=8, angle=90, vjust =0.6, hjust=1, colour="#3C3C3C"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(colour="grey40"),
        aspect.ratio =0.02,
        legend.position="top",
        legend.direction = "horizontal",
        legend.title= element_text(size=10)
  )+
  guides(colour=guide_legend(nrow=1))



#pour figure manuscrit---
svg(filename = "frise.svg",
    width=10,
    height = 6)
frise + theme(
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.title=element_blank(),
  legend.position="left",
  legend.justification = "right",
  legend.key.size = unit(3, "points"),
  legend.box.margin = margin(0,-10,0,0))+
  guides(colour=guide_legend(label.position="bottom", nrow=1))
dev.off()

#pour figure manuscrit 2 (sans légende) ---
png(filename = "frise.png",
    unit="cm",
    width=17,
    height = 2,
    res=600,
    bg="transparent")
frise + 
  theme(
  axis.text.x = element_text(size=6),
  legend.position = "none")
dev.off()


#---- 3. caractérisation des classes sur écarts à la moyenne standardisés----
#--- fonctions de cette partie : J. Gravier 2018 http://doi.org/10.5281/zenodo.1222190

nombre_classes <- 7  #choix nombre classes
tab <- tab_urb_25
tab_CAH_car <- tab %>% CAH_DistKhi2()

#tableau des écarts à l'indépendance des portées et valeurs urbaines pour chaque classe
# pas en vusage car trop de classes !
typochrono <- cutree(tab_CAH_car, k=nombre_classes) #classes des périodes > pour rédac ??
#caractérisation par valeur urbaine
ecarts_vurb <- tab %>% TabEcartPearsonResidus() %>%  mutate(Cluster = factor(typochrono, levels = 1:nombre_classes))
ecarts_norm_vurb  <- ecarts_vurb %>% group_by(Cluster) %>% summarise_all(funs(mean))
#caracterisation par portée
ecarts_portee <- tab_portee_25 %>% TabEcartPearsonResidus() %>%  mutate(Cluster = factor(typochrono, levels = 1:nombre_classes))
ecarts_norm_portee  <- ecarts_portee %>% group_by(Cluster) %>% summarise_all(funs(mean))
#les deux groupés en 1 tableau
tab_ecarts <- cbind(ecarts_norm_vurb,ecarts_norm_portee %>% select(-Cluster))

#----- plot pour * vurb ----
#preparation du tableau pour plot 
#format long
tab_plot_car2 <- tab_ecarts %>% gather(key=Fonction, value="data", v_urb.1 : portee.4)
#transparence
tab_plot_car2$transparence <- 0
tab_plot_car2[grepl("p",tab_plot_car2$Fonction),]$transparence <- 1
tab_plot_car2$fonction2 <- str_sub(tab_plot_car2$Fonction, start=-1)
liste_vurb$Code_EF <- as.character(liste_vurb$Code_EF)
tab_plot_car2 <- left_join(x=tab_plot_car2, y=liste_vurb, by=c("fonction2"="Code_EF")) %>% 
  select(Cluster, Fonction, nom_num, data, transparence, nom_num)
tab_plot_car2$nom_num <- as.character(tab_plot_car2$nom_num )
tab_plot_car2[grepl("p",tab_plot_car2$Fonction),]$nom_num <- tab_plot_car2[grepl("p",tab_plot_car2$Fonction),]$Fonction

plot_ecarts <- 
  ggplot(tab_plot_car2, aes(x=nom_num)) +
  geom_bar(aes(x=nom_num, y=data, fill=Cluster, alpha=transparence), stat= "identity")+
  scale_fill_manual(values=palette_CAH(nombre_classes))+
  scale_alpha(range=c(1,0.5))+
  scale_y_continuous(limits=c(-10, 15), expand=expand_scale(add=0), breaks=c(-10,0,10))+
  scale_x_discrete(position="top")+
  facet_wrap(~Cluster, ncol=3, scales="free_x")+
  coord_flip()+
  labs(
    y = "Moyenne par classe des résidus standardisés (résidus de Pearson)",
    x ="Valeurs urbaines",
    title= "B  Caractérisation des classes de périodes par portées et valeurs urbaines",
    subtitle="Classe de périodes: CAH sur AFC valeurs urbaines * 25 ans",
    caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, CITERES-LAT")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(plot.caption=element_text(hjust=0))


#----- plot pour * vusage ----
#preparation du tableau pour plot 
#format long
tab_plot_car2 <- tab_ecarts %>% gather(key=Fonction, value="data", v_usage.12 : portee.4)
#transparence
tab_plot_car2$transparence <- 0
tab_plot_car2[grepl("p",tab_plot_car$Fonction),]$transparence <- 1
tab_plot_car2$fonction2 <- str_sub(tab_plot_car2$Fonction, start=-2)
liste_vusage$Code_EF <- as.character(liste_vusage$Code_EF)
tab_plot_car2 <- left_join(x=tab_plot_car2, y=liste_vusage, by=c("fonction2"="Code_EF")) %>% 
  select(Cluster, Fonction, nom_num, data, transparence, nom_num)
tab_plot_car2[grepl("p",tab_plot_car$Fonction),]$nom_num <- tab_plot_car[grepl("p",tab_plot_car$Fonction),]$Fonction

plot_ecarts <- 
  ggplot(tab_plot_car2, aes(x=nom_num)) +
  annotate(xmin="21.système défensif urbain",xmax="22.structures fortifiées", 
           ymin=-Inf, ymax=Inf,geom="rect", alpha=0.15)+
  annotate(xmin="41.cultes païens",xmax="44.bâtiments ecclésistiques", 
           ymin=-Inf, ymax=Inf,geom="rect", alpha=0.15)+
  annotate(xmin="61.commerce, échanges, boutiques",xmax="65.extraction, carrières", 
           ymin=-Inf, ymax=Inf,geom="rect", alpha=0.15)+
  geom_bar(aes(x=nom_num, y=data, fill=Cluster, alpha=transparence), stat= "identity")+
  scale_fill_manual(values=palette_CAH(nombre_classes))+
  scale_alpha(range=c(1,0.5))+
  scale_y_continuous(limits=c(-10, 15), expand=expand_scale(add=0), breaks=c(-10,0,10))+
  facet_wrap(~Cluster, scales="free_x")+
  coord_flip()+
  labs(
    x = "Moyenne des écarts standardisés par classe (résidus de Pearson)",
    y="Valeurs urbaines",
    title= "Caractérisation des périodes urbaines par type de fonctions",
    subtitle="Classe de périodes: CAH sur AFC valeurs d'usage * 25 ans",
    caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
  theme_fivethirtyeight()+
  theme_ln()


png(filename = "plot_ecarts.png",
    unit="cm",
    width=25,
    height = 15,
    res=600)
plot_ecarts +
  theme(axis.title=element_text(size=9),
        axis.text=element_text(size=7.5),
        plot.subtitle = element_text(size=10))
dev.off()
#----

#frise et caractérisation ensemble pour manuscrit
frise2 <- frise + 
  # geom_text(data=dates_axe,
  #           aes(x=deb,
  #               y=0,
  #               label=classes),
  #           color="white",
  #           nudge_x = +30)+
  theme_ln()+
  theme(
    aspect.ratio =0.02,
    panel.background = element_blank(),
    # plot.background = element_blank(),
    legend.position="none",
    axis.text.x = element_text(size=8, angle=90, vjust =0.6, hjust=1, colour="grey40")) +
  labs(title="A  Répartition des 7 classes dans le temps",
       caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, CITERES-LAT")

composition <- arrangeGrob(
  frise2+labs(caption=""), 
  plot_ecarts,
  nrow=5,#8 pour vusage
  layout_matrix=rbind(c(1),
                      # c(2), #pour vusage
                      # c(2),
                      # c(2),
                      # c(2),
                      c(2),
                      c(2),
                      c(2),
                      c(2))) %>% as_ggplot()

png(filename = "composition_caracterisation.png",
    unit="cm",
    width=16, #17 pour vusage
    height = 17.5, #24 
    res=600)
composition
dev.off()
