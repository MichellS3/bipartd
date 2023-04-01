library(ggplot2)
library(ggpubr)
library(png)

img1 <- readPNG("C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Red 2014.png")
img2 <- readPNG("C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Red 2015.png")
img3 <- readPNG("C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Red 2016.png")
img4 <- readPNG("C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Red 2017.png")
img5 <- readPNG("C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Red 2018.png")
img6 <- readPNG("C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Red 2019.png")
img7 <- readPNG("C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Red 2020.png")
img8 <- readPNG("C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Red 2021.png")

im_A <- ggplot() + 
  background_image(img1) +
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "mm"))
im_B <- ggplot() + 
  background_image(img2) +
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "mm"))
im_C <- ggplot() + 
  background_image(img3) +
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "mm"))
im_D <- ggplot() + 
  background_image(img4) +
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "mm"))
im_E <- ggplot() + 
  background_image(img5) +
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "mm"))
im_F <- ggplot() + 
  background_image(img6) +
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "mm"))
im_G <- ggplot() + 
  background_image(img7) +
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "mm"))
im_H <- ggplot() + 
  background_image(img8) +
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "mm"))

ggarrange(im_A, im_B, im_C, im_D, im_E, im_F, im_G, im_H, 
          labels = c("2014", "2015", "2016", "2017", "2018","2019","2020","2021"),
          ncol = 4, nrow = 2,font.label = list(size=12, face="bold"))

ggsave(filename = "C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Inicial_Networks.png", 
       width = 10, height = 6, dpi = 300, units = "in", device='png')
