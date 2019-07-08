###############################################
## Analyse des activités en fonction de l'environnement : traitements supplémentaires
## Traitements pour l'analyse spatiale verticale, chapitre 6
## Analyses_FET
## L. Nahassia, 2019
## Zenodo. https://doi.org/10.5281/zenodo.3256673
##############################################

#apparition et disparition des lieux d'inhumation
inhum_old <- OH_p %>% filter(V_USAGE == 42) 


plot_annees <-
 ggplot(inhum_old, 
    aes(x=DATE_FIN,y=DATE_DEB))+
  geom_point(aes(colour= ifelse(inhum_old$DATE_FIN<1800,"A","B")))+
  scale_colour_manual(name=NULL,values =c("A"="grey50","B"="indianred1"),
                      labels=c("A"="disparition avant 1800","B"="disparition en 1800 ou après (12 OH)"))+
  scale_x_continuous(limits=c(-25, 2015), breaks = c(1,seq(100,2000,100)))+
  scale_y_continuous(limits=c(-25, 2015), breaks = c(1,seq(100,2000,100)))+
  coord_fixed()+
  labs(
    title="Répartition des lieux d'inhumation en fonction de \nleur date d'apparition et de disparition",
       x="année de disparition",
       y="année d'apparition",
       caption="L. Nahassia, Géographie-cités, 2019 | Sources : ToToPI, LAT, CITERES")+
  theme_fivethirtyeight()+
  theme_ln()+
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_line(linetype ="dotted", color ="grey75"),
    plot.background = element_rect(fill="white"))
  

ggMarginal(
  p = plot_annees,
  type = 'histogram',
  margins = 'both',
  size = 5,
  colour = 'black',
  fill="white"
)


#plot pour durée de vie des OH gris quand apparition avant annee x ?
plot_duree <- function(df, wrap="~V_URB") {
  ggplot(df)+
    geom_vline(xintercept = 1800,
               color="black",
               size=0.6,
               linetype="dotted")+
    geom_segment(
      aes(y=OH_NUM2, yend=OH_NUM2, x=date_debut, xend=date_fin+1, 
          color= ifelse(date_debut_OH<1700 | date_debut > 1800, "A", ifelse(date_fin_OH>1800,"C","B"))),
      size=1.2)+
    scale_colour_manual(name=NULL,
                        values =c("A"="grey70","B"="indianred1", "C"="darkturquoise"),
                        labels=c("A"="Apparition avant 1700 ou après 1800",
                                 "B"="Apparition après 1700 et disparition avant 1800",
                                 "C"="Apparition après 1700 et disparition après 1800")) +
    scale_x_continuous(breaks = c(1,seq(100,2015,100)))+
    labs(title="Durée de vie des OH lieux d'inhumation (valeur urbaine 5)",
         x="années",
         y="OH",
         caption="L. Nahassia, Géographie-cités, 2018 | Sources : ToToPI, LAT, CITERES")+
    theme_fivethirtyeight()+
    theme_ln()+
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_blank(),
          axis.ticks.y =element_blank(),
          axis.text.x= element_text(angle=90),
          axis.text.y=element_blank())
}
plot_duree(exi_plot %>% filter(V_URB == 5, date_fin_OH >= 1700))
