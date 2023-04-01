library(ggplot2)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(geodata)
library(terra)
library(colorspace)

#Grafica de tasa de hospitalizacion por dengue en Ecuador

cant_with_cod<-data.frame('Canton'=ca_cant$CANTON, 
                          'codes'=c('2202', '1801', '1002', '1503', '0702', '0806', '0301', '1202', '1201', '0904', '1302', '1210', '1102', '0202', '1902', '1303', '0603','0923', 
                                    '0101', '0906', '0907', '1304', '1504', '0706', '0909', '0802', '0908', '0801', '1106', '1305', '0102', '0103', '1402', '0201', '0901','0707', 
                                    '1001', '0928', '1306', '0808', '2402', '0502', '0304', '2101', '0501', '1403', '1101', '0924', '1108', '0701', '1308', '1703', '0910','1212',
                                    '1401', '0803', '0911', '0912', '2201', '1310', '1109', '0503', '0709', '1601', '0105', '0914', '1708', '1311', '0710', '0921', '1301',
                                    '1204', '0504', '1110', '1205', '1507', '0804', '1213', '1701', '0601', '1312', '1705', '0505', '2403', '0919', '0916', '2001', '0920', '0805',
                                    '0205', '1807', '1322', '2003', '2401', '0108', '0918', '0712', '1405', '2301', '1111', '2104', '0109', '1314', '1406', '1409', '1501', '0401',
                                    '1206', '1207', '1208', '1905', '1901', '0713'))

ca_cant<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Cases_Cant_Year.csv")
value_control<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Control Nodes.csv")
cs<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Coordenadas y Codigos/Log_y_Lat_Cantons.csv")
pob_cant<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Adelantos Tesis/Análisis Cantón/Análisis exploratorio/proyeccion_cantonal_total_2010-2021.csv")

value_control<-merge(value_control, ca_cant %>% dplyr::select(CANTON), 
                     by.x=c("CANTON"), by.y="CANTON", all = TRUE)

values_control<-value_control$VALUE
for (i in (1:length(value_control$CANTON))){
  num_c<-value_control$VALUE
  c1<-num_c[i]
  if (is.na(c1)) {
    values_control[i]<-1
  }
}

value_control$VALUE<-values_control


value_control<-merge(value_control, cs %>% dplyr::select(DPA_CANTON, DPA_DESCAN), 
               by.x=c("CANTON"), by.y="DPA_DESCAN")

value_control<-value_control[-c(11), ] #Eliminar la repeticion de Bolivar

##DESDE AQUI

#ec_pob_cases_cantones <- merge(value_control,pob_cant, by.x="DPA_CANTON", by.y="Código")

ec_pob_cases_cantones <- merge(value_control, cant_with_cod %>% dplyr::select(Canton, codes), 
                               by.x=c("CANTON"), by.y="Canton")

ec_cantones <- getData("GADM", country="ECU", level=2)

ec_pob_cases_cantones <- merge(ec_cantones, ec_pob_cases_cantones %>% dplyr::select(codes,VALUE ), 
                               by.x=c("CC_2"), by.y="codes")

ec_pob_cases_cantones@data %>% head()

values_control<-ec_pob_cases_cantones$VALUE
for (i in (1:length(ec_pob_cases_cantones$VALUE))){
  num_c<-ec_pob_cases_cantones$VALUE
  c1<-num_c[i]
  if (is.na(c1)) {
    values_control[i]<-0
  }
}

ec_pob_cases_cantones$VALUE<-values_control

factors_control<-ec_pob_cases_cantones$VALUE
for (i in (1:length(ec_pob_cases_cantones$VALUE))){
  num_c<-ec_pob_cases_cantones$VALUE
  c1<-num_c[i]
  if (c1==0) {
    factors_control[i]<-Never
  }
  if (c1==0) {
    factors_control[i]<-Never
  }
  if (c1==0) {
    factors_control[i]<-Never
  }
  if (c1==0) {
    factors_control[i]<-Never
  }
  if (c1==0) {
    factors_control[i]<-Never
  }
}

"#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598"
"#ABDDA4" "#66C2A5" "#3288BD" "#5E4FA2"


#P_2021<-
ec_pob_cases_cantones %>% 
  st_as_sf() %>% 
  mutate(Frequency = VALUE) %>% 
  ggplot()+
  geom_sf(aes(fill=Frequency))+
  scale_fill_fermenter(n.breaks = 5, palette = "Blues", na.value = NA, direction = 1, breaks=c(4,8,12,16),labels=c("Occasionally", "Sometimes", "Often", "Always"), limits=c(1,16))+
  #scale_fill_gradient(low = "grey", high = "brown")
  #scale_fill_manual(values = c("yellow","gold","lightblue","royalblue"))
  #viridis::scale_fill_viridis(discrete = TRUE,direction=-1)+
  #scale_fill_gradientn(n.breaks = 5, colours=c("#EDEDED","#FFFFBF","#ABDDA4","#3288BD"), na.value = NA, breaks=c(0,4,8,12,16),labels=c("Never","Occasionally", "Sometimes", "Often", "Always"), limits=c(1,16))+
  #scale_fill_distiller( palette = "Spectral", na.value = NA)+
  coord_sf(xlim = c(-81.00, -75.00), ylim = c(-5.00, 1.30))+  
  theme_minimal() +
  theme(legend.title = element_text(size=6, face = "bold"),
        axis.text =element_text(size=6),
        axis.title = element_text(size=6),
        legend.text=element_text(size=6),
        legend.key.width= unit(2.0, 'mm'),
        legend.spacing.y = unit(2.0, 'mm'),
        legend.spacing.x = unit(0.7, 'mm'),
        plot.margin = margin(t=2, l=2, r=2, b=2, unit = "mm"))

ggsave(filename = "C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Frequency of control nodes.png", 
       width = 10, height = 6, dpi = 300, units = "in", device='png')
