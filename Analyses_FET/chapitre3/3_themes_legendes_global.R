################################
## Apparition et disparition d'OH : thèmes ggplot et palettes
## Traitements pour l'analyse du rythme du changement, chapitre 3
## ACAMU : ANALYSES_TF
## L. Nahassia, 2019
## Zenodo. https://doi.org/10.5281/zenodo.3256673
###############################


##thème
 theme_ln <- function() 
  {theme(legend.position = "none",
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        panel.grid.major.x = element_line(line=0.01,linetype ="dotted", color ="grey75"),
        panel.grid.minor.y = element_blank(), #ajouter les ticks secondaires
        panel.background = element_rect(fill="white"),
        plot.background = element_rect(fill="grey87"),
        plot.title = element_text(size=10, face="bold"),
        plot.subtitle = element_text(size=8, face="plain"),
        plot.caption = element_text(size=7),
        plot.margin = unit(c(0.5,1,0.3,1), "lines"),
        axis.ticks =  element_line(colour = "grey75"),
        axis.title=element_text(size=8),
        axis.text=element_text(size=7),
        strip.background = element_rect(fill = "grey80"),
        strip.text = element_text(size=8))}

## palettes
colors_vu <-c("#e72535", "#0a8eb1", "#f6b01a", "#2fc6a0", "#703ab9","#ff7919")
colors_p <-c("#febd2b", "#fe892f", "#ff5733", "#ff2b37")
colors_civil <- c("#f4cc1a","#e6b313","#dba10f","#d2930b","#c98508","#be7504","#af6000")
test <- c("#e72535", "#0a8eb1", "#f6b01a","#703ab9")

