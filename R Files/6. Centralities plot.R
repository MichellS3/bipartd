library(ggplot2)
library(dplyr)
library(ggpubr)

#INTERMEDIACION

Table_inter<- read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Betweenness.csv")

gl<- c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
       0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
       0.25, 0.25, 0.25, 0.25, 0.25, 1, 1, 1, 1, 1, 1, 1, 0.25, 0.25, 0.25,
       0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
       1, 1, 1, 1, 1, 1, 1, 0.25, 1, 1, 1, 1, 1, 1, 1,
       0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
       0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 1, 1, 1, 1,
       1, 1, 1, 0.25, 0.25, 0.25,0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
       0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 1, 1, 1, 1, 1, 1, 1, 0.25, 0.25,
       0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 0.25,0.25, 0.25)

Fig1<-
  ggplot(data = Table_inter, aes(x = Year, y = Ranking, group = Canton)) +
  geom_line(aes(color = Canton), linewidth = gl) +
  geom_point(aes(color = Canton), size = 2) +
  scale_y_reverse(breaks = 1:nrow(Table_inter)) +
  theme_minimal() +
  theme(legend.title = element_text(size=6, face = "bold"),
        axis.text =element_text(size=6),
        axis.title = element_text(size=6),
        legend.text=element_text(size=6),
        legend.key.width= unit(2.0, 'mm'),
        legend.spacing.y = unit(0.3, 'mm'),
        legend.spacing.x = unit(0.7, 'mm'),
        plot.margin = margin(t=2, l=2, r=2, b=2, unit = "mm"),
        legend.position = "bottom") +
  guides(color = guide_legend(title = "Cantons",nrow = 2, byrow = TRUE),
         fill = guide_legend(byrow = TRUE))

#CERCANIA

Table_cer<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Closeness.csv")

glc<- c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 1, 1, 1, 1, 1, 1, 1, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
        1, 1, 1, 1, 1, 1, 1, 0.25, 1, 1, 1, 1, 1, 1, 1,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 1, 1, 1, 1,
        1, 1, 1, 0.25, 0.25, 0.25,0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 1, 1, 1, 1, 1, 1, 1, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25)

Fig2<-
  ggplot(data = Table_cer, aes(x = Year, y = Ranking, group = Canton)) +
  geom_line(aes(color = Canton), linewidth = glc) +
  geom_point(aes(color = Canton), size = 2) +
  scale_y_reverse(breaks = 1:nrow(Table_cer)) +
  theme_minimal() +
  theme(legend.title = element_text(size=6, face = "bold"),
        axis.text =element_text(size=6),
        axis.title = element_text(size=6),
        legend.text=element_text(size=6),
        legend.key.width= unit(2.0, 'mm'),
        legend.spacing.y = unit(0.3, 'mm'),
        legend.spacing.x = unit(0.7, 'mm'),
        plot.margin = margin(t=2, l=2, r=2, b=2, unit = "mm"),
        legend.position = "bottom") +
  guides(color = guide_legend(title = "Cantons",nrow =2 , byrow = TRUE),
         fill = guide_legend(byrow = TRUE))

ggarrange(Fig1,Fig2, 
          labels = c("A) Betweeness centrality ", "B) Closeness centrality"),
          ncol = 2, nrow = 1, common.legend = TRUE, legend="bottom",
          font.label = list(size=12, face="bold"))

ggsave(filename = "C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Closeness and Betweeness centrality.png", 
       width = 10, height = 6, dpi = 300, units = "in", device='png')

#DEGREE

Table_deg<- read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Degree.csv")

gld<- c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 1, 1, 1, 1, 1, 1, 1, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 1, 1, 1, 1, 1, 1, 1, 0.25, 0.25, 0.25,
        0.25, 1, 1, 1, 1, 1, 1, 1, 0.25, 1, 1, 1, 1, 1, 1,
        1, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 1, 1, 1,
        1, 1, 1, 1, 0.25, 0.25,0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 1, 1, 1, 1, 1, 1, 1, 0.25,
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25,0.25, 0.25, 0.25)

Fig3<-
ggplot(data = Table_deg, aes(x = Year, y = Ranking, group = Canton)) +
  geom_line(aes(color = Canton), linewidth = gld) +
  geom_point(aes(color = Canton), size = 2) +
  scale_y_reverse(breaks = 1:nrow(Table_deg)) +
  theme_minimal() +
  theme(legend.title = element_text(size=6, face = "bold"),
        axis.text =element_text(size=6),
        axis.title = element_text(size=6),
        legend.text=element_text(size=6),
        legend.key.width= unit(2.0, 'mm'),
        legend.spacing.y = unit(0.3, 'mm'),
        legend.spacing.x = unit(0.7, 'mm'),
        plot.margin = margin(t=2, l=2, r=2, b=2, unit = "mm"),
        legend.position = "bottom") +
  guides(color = guide_legend(title = "Cantons",nrow = 2, byrow = TRUE),
         fill = guide_legend(byrow = TRUE)) +
  labs(title = "Dengue transmission Control nodes in Ecuador",
       subtitle = "Cantons ranked by Degree Centrality")

ggsave(filename = "C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Degree centrality.png", 
       width = 10, height = 6, dpi = 300, units = "in", device='png')  
