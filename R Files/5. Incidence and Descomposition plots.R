library(ggplot2)
library(tseries)
library(forecast)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(geodata)
library(terra)
library(colorspace)
library(ggpubr)

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
cs<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Coordenadas y Codigos/Log_y_Lat_Cantons.csv")
pob_cant<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Adelantos Tesis/Análisis Cantón/Análisis exploratorio/proyeccion_cantonal_total_2010-2021.csv")

ca_cant<-merge(ca_cant, cs %>% dplyr::select(DPA_CANTON, DPA_DESCAN), 
                    by.x=c("CANTON"), by.y="DPA_DESCAN")

ca_cant<-ca_cant[-c(11), ] #Eliminar la repeticion de Bolivar

##DESDE AQUI

ec_pob_cases_cantones <- merge(ca_cant,pob_cant, by.x=c("DPA_CANTON"), by.y="Código")

ec_pob_cases_cantones <- merge(ec_pob_cases_cantones, cant_with_cod %>% dplyr::select(Canton, codes), 
                               by.x=c("CANTON"), by.y="Canton")

ec_cantones <- getData("GADM", country="ECU", level=2)

ec_pob_cases_cantones <- merge(ec_cantones, ec_pob_cases_cantones %>% dplyr::select(codes,Num..CASOS.2021,X2021 ), 
                               by.x=c("CC_2"), by.y="codes")

ec_pob_cases_cantones@data %>% head()

cases_year<-ec_pob_cases_cantones$Num..CASOS.2021
pob_year<-as.numeric(ec_pob_cases_cantones$X2021)
for (i in (1:length(ec_pob_cases_cantones$Num..CASOS.2021))){
  num_c<-ec_pob_cases_cantones$Num..CASOS.2021
  c1<-num_c[i]
  if (is.na(c1)) {
    cases_year[i]<-0
    pob_year[i]<-0
  }
}

ec_pob_cases_cantones$Num..CASOS.2021<-cases_year
ec_pob_cases_cantones$X2021<-pob_year

#P_2021<-
  ec_pob_cases_cantones %>% 
  st_as_sf() %>% 
  mutate(Incidence = (Num..CASOS.2021/X2021)*100000) %>% 
  ggplot()+
  geom_sf(aes(fill=Incidence))+
  scale_fill_gradientn(n.breaks = 5, colours=c('Yellow','Orange','Red'), na.value = NA, limits=c(1,490))+
  coord_sf(xlim = c(-81.00, -75.00), ylim = c(-5.00, 1.30))+  
  theme_minimal() +
  theme(legend.title = element_text(size=6, face = "bold"),
        axis.text =element_text(size=6),
        axis.title = element_text(size=6),
        legend.text=element_text(size=6),
        legend.key.width= unit(2.0, 'mm'),
        legend.spacing.y = unit(2.0, 'mm'),
        legend.spacing.x = unit(0.7, 'mm'),
        plot.margin = margin(t=2, l=2, r=2, b=2, unit = "mm")) +
  scale_x_continuous(breaks = seq(-81, -75, by = 2))+
  scale_y_continuous(breaks = seq(-5, 1.30, by = 2))+
  guides(color = guide_legend(title = "Dengue hospitalizations"))

ggarrange(P_2014,P_2015,P_2016,P_2017,P_2018,P_2019,P_2020,P_2021, 
          labels = c("2014", "2015", "2016", "2017","2018", "2019", "2020", "2021"),
          ncol = 4, nrow = 2, common.legend = TRUE, legend="right",
          font.label = list(size=12, face="bold"))

ggsave(filename = "C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Incidence_hospitalizacion_dengue_year.png", 
       width = 10, height = 6, dpi = 300, units = "in", device='png')

#Table_incidence <- data.frame('Codes'=ec_pob_cases_cantones$CC_2,
#                              'Cantons'=ec_pob_cases_cantones$NAME_2)

Tasa_2021 <- (ec_pob_cases_cantones$Num..CASOS.2021/ec_pob_cases_cantones$X2021)*100000

for (i in (1:length(Tasa_2021))){
  c1<-Tasa_2021[i]
  if (is.na(c1)) {Tasa_2021[i]<-0}}

Table_incidence<-cbind(Table_incidence,Tasa_2021)

write.csv(Table_incidence,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Dengue hospitalizations Rate.csv")


#Graficas de descomposicion

a<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Descom_data.csv")
tsdata<- ts(a$num..cases, start=2014, frequency=52)
Fig1<-stl (tsdata, s.window = 'periodic')%>% autoplot()+
  theme(plot.margin = margin(t=8, l=2, r=2, b=2, unit = "mm"))

b<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Descom_Coast.csv")
tsdata<- ts(b$num..cases, start=2014, frequency=52)
Fig2<-stl (tsdata, s.window = 'periodic')%>% autoplot()+
  theme(plot.margin = margin(t=8, l=2, r=2, b=2, unit = "mm"))


c<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Descom_Amazonia.csv")
tsdata<- ts(c$num..cases, start=2014, frequency=52)
Fig3<-stl (tsdata, s.window = 'periodic')%>% autoplot()+
  theme(plot.margin = margin(t=8, l=2, r=2, b=2, unit = "mm"))


ggarrange(Fig1,Fig2,Fig3,
          labels = c('A) Countrywide', 'B) Coast', 'C) Amazonia'),
          ncol = 3, nrow = 1,
          font.label = list(size=12, face="bold"))

ggsave(filename = "C:/Users/miche/Desktop/Michell Universidad/Tesis/Results/Grafica_de_descomposicion.png", 
       width = 12, height = 4, dpi = 300, units = "in", device='png')
