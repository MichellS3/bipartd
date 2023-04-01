library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(flextable)
library(grid)
library(gridExtra)
library(cowplot)

long_df <- function(table){
  a <- table %>% pivot_longer(!c(Year,Proporcion), names_to= "Parameter", values_to ="Value")
  a
}

redesC<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Robustez.csv")
red_uniqueC<-redesC %>% filter(Year==2021)


#redC2021<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes Coast/Red_2021.csv")
redC2021<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes Amazonia/Red_2021.csv")
b<-redC2021
source<-unique(b$Source)
target<-unique(b$Target)
nodes<-c(source,target)
total<-length(unique(nodes))

b<-redC2021
initial_nodes<- 15
p_nodes_eliminados<-c(0)
nodes_de_red<-c(1)
order_elimination_costa<-red_uniqueC[,2]  #CAMBIAR
order_elimination_amazonia<-red_uniqueC[1:10,3] #CAMBIAR

for (n in 1:(length(order_elimination_amazonia))){  #CAMBIARred_uniqueC$Costa
  for (i in 1:(length(b$Source))){
    for (j in (1:2)){
      cell<- b[i,j]
      node_elimination<-order_elimination_amazonia[n]
      if (cell==node_elimination){
        for (k in 1:3) {
          b[i,k] = 0
        }
      }
    }
  }
  source<-unique(b$Source)
  target<-unique(b$Target)
  nodes<-c(source,target)
  total<-((length(unique(nodes)))-1)
  proportion<- n/initial_nodes
  proportion2<-total/initial_nodes
  p_nodes_eliminados<-c(p_nodes_eliminados,proportion)
  nodes_de_red<-c(nodes_de_red,proportion2)
}

for (i in (length(p_nodes_eliminados)+1):initial_nodes){
  proportion<-i/initial_nodes
  p_nodes_eliminados<-c(p_nodes_eliminados,proportion)
  nodes_de_red<-c(nodes_de_red,0)
}

costa<- data.frame('Proporcion'=p_nodes_eliminados,
                   'Coast'=nodes_de_red)
amazonia<- data.frame('Proporcion'=p_nodes_eliminados, #Con nodos menores a 10 cambiar
                      'Amazonia'=nodes_de_red) #Con nodos menores a 10 cambiar

#amazonia<-amazonia[-c(12), ]

red_amazonia_2021<- data.frame('Year'=rep(2021,length(nodes_de_red)), #Con nodos menores a 10 cambiar
                            'Proporcion'=p_nodes_eliminados, #Con nodos menores a 10 cambiar
                            #'Coast'=costa[2])#,
                            'Amazonia'=amazonia[2])

Table_join<-long_df(red_amazonia_2021)

#
#todo_cya<-Table_join
todo_cya<-rbind(todo_cya,Table_join)
#todo_cya<-todo_cya[-c(254), ]

Table_anex<-data.frame('Region'=c('Amazonia','Coast'),
                       'Nodes'=c(14,51),
                       'Edges'=c(115,2039))

mytheme <- gridExtra::ttheme_minimal(
  core = list(bg_params = list(fill = c("#66C2A5","#FC8D62"), col=NA),
              fg_params=list(fontface=3, cex = 0.5)),
  colhead = list(fg_params=list(fontface=4L, cex = 0.5)),
  rowhead = list(fg_params=list(fontface=3L, cex = 0.5)))

myt <- gridExtra::tableGrob(Table_anex, theme = mytheme)


#Fig4<-
  ggplot(todo_cya, aes(x = Proporcion, y = Value, color = Parameter)) +
  geom_point(alpha = .5, size = 0.5) +
  theme_minimal() +
  labs(x = expression(f), y = expression(P[infinity])) +
  theme(panel.grid.minor = element_blank()) +
  geom_smooth(aes(group = Parameter),linewidth = 1,method = "gam",linetype=1, fill = NA) +
  scale_color_brewer(palette = 'Set2') +
  theme(legend.title = element_text(size=6, face = "bold"),
        axis.text =element_text(size=6),
        axis.title = element_text(size=8),
        legend.position = "none",
        legend.text=element_text(size=6),
        legend.key.width= unit(2.0, 'mm'),
        legend.spacing.y = unit(0.5, 'mm'),
        legend.spacing.x = unit(0.7, 'mm'),
        plot.margin = margin(t=0.5, l=0.5, r=0.5, b=0.5, unit = "mm")) +
  xlim(0,1)+ylim(0,1) +
  annotation_custom(myt, xmin=0.70, xmax=0.75, ymin=0.75, ymax=1)

ggsave(filename = "C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Robustez.png", 
       width = 5, height = 4, dpi = 300, units = "in", device='png')
