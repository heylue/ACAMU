###############################################
## Exploration factorielle des données : fonctions globales (création de tableaux et de plots)
## Traitements pour l'exploration temporelle, chapitre 4
## ACAMU : ANALYSES_TF
## L. Nahassia, 2019
## Zenodo. https://doi.org/10.5281/zenodo.3256673
##############################################



#---- Palettes ----
#VURB
couleurs_vurb <- adjustcolor(colors_vu, alpha=0.7)
#PORTEE
couleurs_portee <- adjustcolor(colors_p, alpha=0.7)
#INIDIVIDUS TEMPs
color_ramp_ind <- colorRampPalette(c("grey10", "gray60"))
#V_USAGE # inséré directement sur le tableau de résultat > que si tableau de contingence sur vusage
# AFC$co$usages <- as.numeric(str_sub(rownames(AFC$co), start=-2))
# AFC$co$couleurs <- cut (AFC$co$usages,
#                         breaks=c(10,20,30,40,50,60,70),
#                         labels=c("#e72535", "#0a8eb1", "#f6b01a", "#2fc6a0", "#703ab9","#ff7919"),
#                         right=FALSE,
#                         include.lowest = TRUE)
# couleurs_vusage <- AFC$co$couleurs
#/// revoir ce qui est le mieux
usages <- as.numeric(str_sub(rownames(AFC$co), start=-2))
couleurs_vusage <- cut (usages,
                        breaks=c(10,20,30,40,50,60,70),
                        labels=c("#e72535", "#0a8eb1", "#f6b01a", "#2fc6a0", "#703ab9","#ff7919"),
                        right=FALSE,
                        include.lowest = TRUE)
names(couleurs_vusage) <- usages
couleurs_vusage <- adjustcolor(couleurs_vusage, alpha=0.7)
#CAH
palette_CAH  <- colorRampPalette(brewer.pal(7,"Set2"))
# palette_CAH  <- colorRampPalette(c("#399283","#a73358","#154e56","#dc8873","#42558d","#cca05e"))
# palette_CAH  <- colorRampPalette(c("#399283","#a73358","#154e56","#dc8873","#4c3e76","#c66b50"))
palette_CAH  <- colorRampPalette(c("#E8C95D","#eb8179","#85dbbd","#628aba","#a39e96"))

palette_CAH_usages <- colorRampPalette(c("grey80","grey40"))



#E8C95D
#D16B54
#A9D8C8
#433447
#BCB19D


# création tableaux de contingence---------------------------------------------------------------
creation_tab_cont <- function(sequence){
  bornes_deb <- head(sequence, -1)
  bornes_fin <- tail(sequence, -1)
  tabcont <- data.frame(deb=bornes_deb,fin=bornes_fin)
  tabcont$periode <-paste(tabcont$deb,tabcont$fin,sep="-")
  #compte des v_urb > avec boucle
  for (n in sort(unique(OHafc$v_urb))){ # pour toutes les valeurs urbaines
    nom_col <- paste("v_urb",n,sep=".") # creer une nouvelle colonne 
    for (i in 1:length(tabcont$deb)) { 
      tabcont[i,nom_col] <- sum (OHafc$v_urb==n & OHafc$DISPARITIO>=tabcont$deb[i] & OHafc$APPARITION<tabcont$fin[i])
    } # remplir cette colonne de la somme des individus ayant cette valeur urbaine et ayant existé dans les bornes données
  }
  #compte des v_usage
  for (n in sort(unique(OHafc$V_USAGE))){
    nom_col <- paste("v_usage",n,sep=".") # creer une nouvelle colonne 
    for (i in 1:length(tabcont$deb)) { 
      tabcont[i,nom_col] <- sum (OHafc$V_USAGE==n & OHafc$DISPARITIO>=tabcont$deb[i] & OHafc$APPARITION<tabcont$fin[i])
    }
  }
  #compte des portees
  for (n in sort(unique(OHafc$portee))){
    nom_col <- paste("portee",n,sep=".") # creer une nouvelle colonne 
    for (i in 1:length(tabcont$deb)) { 
      tabcont[i,nom_col] <- sum (OHafc$portee==n & OHafc$DISPARITIO>=tabcont$deb[i] & OHafc$APPARITION<tabcont$fin[i])
    }
  }
  row.names(tabcont)<-tabcont$periode
  return(tabcont)
}

# summary.variance.dudi -------------------------------------------------------------------------
# Crée un tableau pour résumer la variance expliquée par chaque axe + mise en forme pour plotter
# Entrée : data = resultat dudi.coa
# Sortie : data.frame avec {n° de composante, variance, % de la variance, variance résultante, factor pour couleur}

summary.variance.dudi <- function(data){
  expected_inertia <- 100/max(data$rank)
  summaryAFC <- data.frame(
    COMP = seq(1,max(data$rank),1), 
    EIG = data$eig,
    PCTVAR = 100*data$eig / sum(data$eig),
    CUMPCTVAR = cumsum(100*data$eig / sum(data$eig)))
  summaryAFC$CUTSCOLOR <- cut(summaryAFC$PCTVAR, breaks=c(0,expected_inertia,Inf))
  return(summaryAFC)
}

# barplot.dudi.variance -----------------------------------------------------------------
# Crée un graphique barplot de la variance expliquée par chaque axe d'un résultat d'AFC
# Entrée : data = resultat dudi.coa + sumdata = résultat summary.variance.dudi
# Sortie : un objet ggplot

#paste(as.character(substitute(data)), # ligne pour récupérer le nom de la variable

barplot.dudi.variance <- function(data, sumdata, titre, soustitre=""){
  expected_inertia <- 100/max(data$rank)
  plot <- ggplot(data=sumdata, aes(x=COMP, y=PCTVAR, fill=sumdata$CUTSCOLOR)) +
    geom_bar(stat="identity") +
    labs(x="Composantes",
         y="Part de la variance expliquée",
         title= titre,
         subtitle=soustitre,
         caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, CITERES-LAT")+
    #     geom_text(aes(label=paste(round(PCTVAR),"%",sep=""),color = sumdata$CUTSCOLOR), 
    #               vjust=1.2,
    #               hjust=0.4,
    #               fontface = "bold",
    #               size=10) +
    scale_fill_manual(values=alpha(c("grey60","darkslategray3"), 0.8), labels=c("Axe non conservé dans l'analyse", "Axe conservé dans l'analyse") ) +
    geom_hline(aes(yintercept=expected_inertia), colour="turquoise4", linetype="dashed")+
    geom_text(x=max(sumdata$COMP)+0.5, 
              y=expected_inertia+1,
              label=paste("inertie moyenne attendue par composante : ", round(expected_inertia, digits=2), "%", sep=""),
              vjust=0,
              hjust=1,
              colour="turquoise4",
              alpha=0.5,
              size=3)+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(legend.position = "bottom",
          legend.title = element_blank())
  return(plot)
}


# biplot.AFC -----------------------------------------------------------------
# Crée un graphique biplot des résultats de l'AFC sur les 2 premiers axes (variables+individus)
# Entrée : data = resultat dudi.coa  ; variable_color = palette pour les variables ; liste_variable = nom des variables ; 
#         axes = liste des 2 axes à ploter ; titre/soustitre ; marge = ajouter une marge en haut du plot ; 
#         type=variable fonctionnelle ; marge_texte = marge autour des text_repel
# Sortie : un objet ggplot

biplot.AFC <- function(data, axe, variable_color, liste_variable, titre, soustitre, marge, type, marge_texte){
  
  pctvar_A1 <- round(data$eig[axe[[1]]]*100/sum(data$eig))
  pctvar_A2 <- round(data$eig[axe[[2]]]*100/sum(data$eig))
  inertie_expl <- inertia.dudi(data,row.inertia = TRUE, col.inertia = TRUE)
  data$co$inertie1 <- inertie_expl$col.abs[,axe[[1]]]
  data$co$inertie2 <- inertie_expl$col.abs[,axe[[2]]]
  data$li$inertie1 <- inertie_expl$row.abs[,axe[[1]]]
  data$li$inertie2 <- inertie_expl$row.abs[,axe[[2]]]
  
  if(nrow(data$li) > 50){lab_periode <- data$li[append(seq(1,nrow(data$li),3),seq(2,nrow(data$li),3)),]} else {lab_periode <- data$li} #selection de 2 lignes sur 3 si nombre de périodes trop grand pour lisibilité
  
  if(position=="haut")#placement de la légende
  { position_legende <- 1
  } else if (position=="bas") 
  { position_legende <- 0}
  
  contrib_col <- (data$co$inertie1+data$co$inertie2)/2
  
  ggplot()+
    geom_hline(aes(yintercept=0), colour="gray25")+
    geom_vline(aes(xintercept=0), colour="gray25")+
    labs(title=titre,
         subtitle=soustitre,
         x=paste("Axe n°", axe[[1]], " (",pctvar_A1, "% de variance expliquée)", sep=""),
         y=paste("Axe n°", axe[[2]]," (",pctvar_A2, "% de variance expliquée)", sep=""),
         caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, CITERES-LAT",
         size=paste("Contribution moyenne des", type,"\nà l'inertie des deux axes")
    )+
    # élargissement des limites du plot
    expand_limits(x=c(min(data$li[,axe[[1]]])-0.1, max(data$li[,axe[[1]]])+0.1), y=c(min(data$li[,axe[[2]]])-0.1, max(data$li[,axe[[2]]])+marge))+ 
    # lignes reliant lignes (périodes) 
    geom_path(data=data$li, 
              mapping=aes(x=data$li[,axe[[1]]], y=data$li[,axe[[2]]]),
              linetype="dotted",
              colour= "gray18")+ #bcp trop compliqué de faire une line avec gradient
    #label des lignes (périodes)
    geom_text_repel(data=lab_periode,
                    mapping=aes(x=lab_periode[,axe[[1]]], y=lab_periode[,axe[[2]]],label=rownames(lab_periode)),
                    force=3,
                    # max.iter = 1000,
                    box.padding = unit(marge_texte, "lines"), 
                    size=3,
                    colour=color_ramp_ind(nrow(lab_periode)),
                    segment.color="grey60")+
    # points des colonnes (fonctions)  
    geom_point(data=data$co,
               mapping=aes(x=data$co[,axe[[1]]], y=data$co[,axe[[2]]], size=contrib_col), #choix des composantes i et j
               shape=16,
               colour=variable_color)+
    # #labels des colonnes (fonctions)  
    geom_text(data=data$co,
              mapping=aes(x=data$co[,axe[[1]]], y=data$co[,axe[[2]]],label=liste_variable),
              colour=variable_color,
              size=3,
              fontface="bold",
              hjust=-1, vjust=-1.5)+
    #points des lignes (périodes)
    geom_point(data=data$li, 
               mapping=aes(x=data$li[,axe[[1]]], y=data$li[,axe[[2]]]),
               shape=18,
               size=3,
               colour= color_ramp_ind(nrow(data$li)), 
               alpha=0.8)+
    scale_size_continuous(range=c(1,10), 
                          breaks=c(
                            round(min(contrib_col)+0.01,2), 
                            round((min(contrib_col)+max(contrib_col))/2,2),
                            round(max(contrib_col)-0.01,2)
                            )
                          )+
    theme_fivethirtyeight()+
    theme_ln()+
    theme( legend.position = "bottom",
           legend.justification = "left",
           # legend.background = element_rect(fill=adjustcolor("white",0.8), colour=NULL),
          legend.key = element_blank(),
          legend.direction = "horizontal")+
  guides(size=guide_legend(title.position = "top", label.position="bottom", label.hjust = 0.5, override.aes = list(shape=1)))
  
}
# biplot.tout.AFC -----------------------------------------------------------------
# Crée un graphique biplot des résultats de l'AFC sur les 2 premiers axes (variables+individus)
# Entrée : data = resultat dudi.coa  ; variable_color = palette pour les variables ; liste_variable = nom des variables ; 
#         axes = liste des 2 axes à ploter ; titre/soustitre ; marge = ajouter une marge en haut du plot ; 
#         position = position de la légende dans le plot ("bas" ou "haut" )
# Sortie : un objet ggplot


biplot.periodes.AFC <- function(data, axe, variable_color, liste_variable, titre, soustitre, marge, position="bas"){
  
  pctvar_A1 <- round(data$eig[axe[[1]]]*100/sum(data$eig))
  pctvar_A2 <- round(data$eig[axe[[2]]]*100/sum(data$eig))
  inertie_expl <- inertia.dudi(data,row.inertia = TRUE, col.inertia = TRUE)
  data$co$inertie1 <- inertie_expl$col.abs[,axe[[1]]]
  data$co$inertie2 <- inertie_expl$col.abs[,axe[[2]]]
  data$li$inertie1 <- inertie_expl$row.abs[,axe[[1]]]
  data$li$inertie2 <- inertie_expl$row.abs[,axe[[2]]]
  # lab_periode <- data$li
  if(nrow(data$li) > 50){lab_periode <- data$li[append(seq(1,nrow(data$li),3),seq(2,nrow(data$li),3)),]} else {lab_periode <- data$li} #selection de 2 lignes sur 3 si nombre de périodes trop grand pour lisibilité
  

  
  ggplot()+
    geom_hline(aes(yintercept=0), colour="gray25")+
    geom_vline(aes(xintercept=0), colour="gray25")+
    labs(title=titre,
         subtitle=soustitre,
         x=paste("Axe n°", axe[[1]], " (",pctvar_A1, "% de variance expliquée)", sep=""),
         y=paste("Axe n°", axe[[2]]," (",pctvar_A2, "% de variance expliquée)", sep=""),
         caption="L. Nahassia, Géographie-cité, 2018 | Sources : ToToPI, CITERES-LAT",
         size="Inertie moyenne \nexpliquée des variables \nsur les deux axes"
    )+
    # élargissement des limites du plot
    expand_limits(x=c(min(data$li[,axe[[1]]])-0.1, max(data$li[,axe[[1]]])+0.1), y=c(min(data$li[,axe[[2]]])-0.1, max(data$li[,axe[[2]]])+marge))+ 
    # lignes reliant lignes (périodes) 
    geom_path(data=data$li, 
              mapping=aes(x=data$li[,axe[[1]]], y=data$li[,axe[[2]]]),
              linetype="dotted",
              colour= "gray18")+ #bcp trop compliqué de faire une line avec gradient
    #label des lignes (périodes)
    geom_text_repel(data=lab_periode,
                    mapping=aes(x=lab_periode[,axe[[1]]], y=lab_periode[,axe[[2]]],label=rownames(lab_periode)),
                    force=1,
                    # max.iter = 1000,
                    box.padding = unit(0.4, "lines"), 
                    size=3,
                    colour=color_ramp_ind(nrow(lab_periode)),
                    segment.color="grey60")+
    #points des lignes (périodes)
    geom_point(data=data$li, 
               mapping=aes(x=data$li[,axe[[1]]], y=data$li[,axe[[2]]]),
               shape=18,
               size=5,
               colour= color_ramp_ind(nrow(data$li)), 
               alpha=0.8)+
    # scale_size_continuous(range = c(1, 10), breaks=c(1,5,20))+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(legend.justification = c(1, position_legende),
          legend.position = c(1, position_legende),
          legend.background = element_rect(fill=adjustcolor("white",0.8), colour=NULL),
          legend.key = element_blank())
}

# heat.contribvar-----------------------------------------------------------------
# Crée une heat map comme un tableau pour analyser les contributions et les cos²
# Entrée : data = tableau à préparer (nom des variables), à partir de inertia.dudi
# Sortie : affiche un objet ggdraw, retourne le ggplot sans axes inversés

heat.contribvar<- function(data, titre, soustitre="", couleur, gestion_dates=FALSE) {
  
  if (gestion_dates==TRUE){    #prepa données quand data = périodes (melt + tri)
    data$periodes <- row.names(data)
    data.m <- melt(data=data, id.vars = "periodes")
    data.m$periodes_sort <- word(data.m$periodes,1,sep = "\\-")#sélection des dates avant "-" /!\ remettre -25 qui disparaît forcément
    data.m[grep("^-25",data.m$periodes),]$periodes_sort <- -25 # -25 remis en premières date
    yvar <- as.factor(data.m$periodes_sort)
    ylabs <- data.m$periodes
  }
  else if(gestion_dates==FALSE) # prépa données quand data =! périodes
  {
    data$variables <- row.names(data)
    data.m <- melt(data=data, id.vars = "variables")
    yvar <- data.m$variables
    ylabs <- data.m$variables
  }
  
  if(nrow(data) > 50){taille_ticks_text <- 6} else {taille_ticks_text <- 7} #selection d'une ligne sur 2 si nombre de périodes trop grand pour lisibilité
  
  #data.m$periodes_sort <- data.m$periodes_sort %>% as.numeric()
  
  # #gestion transparence possible
  # geom_tile(color = "white", fill= "grey85")+
  #   geom_tile(aes(fill=value, alpha = value), color = "white")+
  #   scale_fill_gradient(low="white", high="red")+
  
  #plot
  ggplot(data.m, aes(x=variable, y=yvar))+ #y pour axe dans l'ordre
    geom_tile(aes(fill=value), color = "grey87")+
    geom_text(aes(label = round(value, digits=2)), size=2) +
    scale_y_discrete(
      expand=c(0,0),
      labels=ylabs #labels périodes entières
    ) +
    scale_x_discrete(
      expand = c(0, 0), 
      position = "top"
    )+
    scale_fill_gradient(low="white", high=couleur)+
    labs(
      title =titre,
      subtitle =soustitre,
      caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, CITERES-LAT",
      x="",
      y="",
      fill= "%")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme( 
      panel.grid = element_blank(),
      panel.background	= element_blank(),
      axis.ticks = element_blank(), #pour ne pas voir les ticks et title
      axis.text.y= element_text( size=taille_ticks_text, margin=margin(t=-4, unit="pt")),
      legend.position = "right",
      legend.direction = "vertical",
      legend.background = element_blank())
}


# classes.periodes.cah -------------------------------------------------------------------
# Crée le tableaux recençant les appartenances de chaque périodes aux classes de la CAH
# Entrée : CAH = résultats CAH, nb= nombre de classe
# Sortie : $entier = tableau dans son intégralité, $axe = sous-ensembles pour légendes

classes.periodes.cah <- function(CAH, nb){
  
  typochrono <- cutree(CAH, k=nb) 
  classes_periodes <- typochrono %>% as.data.frame()
  classes_periodes$periode <- row.names(classes_periodes) 
  classes_periodes$deb <- word(row.names(classes_periodes),1,sep = "\\-")%>% as.numeric()#sélection des dates avant "-" /!\ remettre -25 qui disparaît forcément
  classes_periodes[grep("^-25",row.names(classes_periodes)),]$deb <- -25 # -25 remis en premières date de début
  classes_periodes$fin <- word(row.names(classes_periodes),2,sep = "\\-")%>% as.numeric()#sélection des dates après "-" /!\ erreur sur fin de la période commençant par -25
  classes_periodes[grep("^-25",row.names(classes_periodes)),]$fin <- word(row.names(classes_periodes),2,sep = "\\-25-")[1] %>% as.numeric() # première date de fin ajustée
  row.names(classes_periodes) <- NULL
  colnames(classes_periodes) <- c("classes","periode","deb","fin")
  classes_periodes$classes <- as.factor(classes_periodes$classes)
  
  dates_axe <- NULL
  
  for (i in 1:nombre_classes)
  {
    tab <- classes_periodes %>% filter(classes==i)
    tab$deb <- as.character(tab$deb)
    tab$fin <- as.character(tab$fin)
    tab <- tab %>% filter(!(deb %in% fin))
    dates_axe <- rbind(dates_axe,tab)
    
  }
  
  dates_axe$deb <- as.numeric(dates_axe$deb)
  dates_axe$fin <- as.numeric(dates_axe$fin)
  
  liste_tab <- list("entier"=classes_periodes, "axe"=dates_axe)
  
  return(liste_tab)
  
}
