library(dplyr)

Table_test06<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2021.csv")
Labels<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes por canton/ID_Label 2021.csv")
cs<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Coordenadas y Codigos/Log_y_Lat_Cantons.csv")

Table_test06$CANTON <- str_replace (Table_test06$CANTON, "2302", "2301")
Table_test06$ID_cant <- str_replace (Table_test06$ID_cant, "2302", "2301")

Table_test06<-merge(Table_test06, cs %>% dplyr::select(DPA_CANTON, DPA_DESCAN), 
                    by.x=c("ID_cant"), by.y="DPA_CANTON")
Table_test06$ID_cant<-Table_test06$DPA_DESCAN
Table_test06$CANTON<-Table_test06$DPA_DESCAN

Table_test <- data.frame("AÑO"=rep(2021,length(Table_test06$AÑO)),
                         "PROVINCIA"= Table_test06$PROVINCIA,
                         "CANTON"= Table_test06$CANTON,
                         "ID_cant"= Table_test06$ID_cant,
                         "PARROQUIA"= Table_test06$PARROQUIA,
                         "ENFERMEDAD"= Table_test06$ENFERMEDAD,
                         "SE"=Table_test06$SE,
                         "NUM. CASOS"=Table_test06$NUM..CASOS)

write.csv(Table_test,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2021.csv")


#CAMBIAR ID-LABELS
2021 2021 corregir las tildes y poner en mayusculas
2021 2021 2021 2021 2021 no cambiar

cant<-toupper(Labels$CANTON)

Labels$CANTON<-cant

Table_ID<-data.frame('Id'=Labels$ID,
                     'Label'=Labels$CANTON)

write.csv(Table_ID,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes por canton/ID_Label 2021.csv")



#Numero de casos por canton
canton <- unique(Table_test06$CANTON)
ID_cant <- (1:length(canton))

for (i in 1:length(canton)) {
  name<- canton[i]
  replace<- ID_cant[i]
  Table_test06$ID_cant <- sub(name,replace,Table_test06$ID_cant)
}

Cases_province<-function(ID_canton){
  Table_cant01 <- Table_test06 %>% filter (ID_cant==ID_canton)
  cases_acum<-0
  for (i in 1:length(Table_cant01$CANTON)){
    cases_acum<-cases_acum+Table_cant01[i,8]
  }
  cases_acum
}

Num_cases_cant <- lapply(ID_cant, Cases_province)
cases<-numeric()
for (i in 1:length(Num_cases_cant)){
  a<-Num_cases_cant[[i]]
  cases<-c(cases,a)
}

Table_Cases_province<- data.frame('CANTON'=canton,
                                  'Num. CASOS 2021'=cases)

b<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Cases_Cant_Year.csv")
Table_Prov <- merge(b,Table_Cases_province, all=TRUE)
write.csv(Table_Prov,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Cases_Cant_Year.csv")



#Numero de casos por semana
SE <- sort(unique(Table_test06$SE))

Cases_week<-function(SEe){
  Table_cant01 <- Table_test06 %>% filter (SE==SEe)
  cases_acum<-0
  for (i in 1:length(Table_cant01$CANTON)){
    cases_acum<-cases_acum+Table_cant01[i,8]
  }
  cases_acum
}

Num_cases_week <- lapply(SE, Cases_week)

cases<-numeric()
for (i in 1:length(Num_cases_week)){
  a<-Num_cases_week[[i]]
  cases<-c(cases,a)
}

Table_Cases_week<- data.frame('SE'=SE,
                              'Num. CASOS 2021'=cases)

d<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Cases_Week_Year.csv")
Table_Week <- merge(d,Table_Cases_week, all=TRUE)
write.csv(Table_Week,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Cases_Week_Year.csv")


#Casos por semana en la region Costa
Table_cases<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2021.csv")
Labels<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes por canton/ID_Label 2021.csv")

canton <- unique(Table_cases$CANTON)
ID_cant <- (1:length(canton))

for (i in 1:length(canton)) {
  name<- canton[i]
  replace<- ID_cant[i]
  Table_cases$ID_cant <- sub(name,replace,Table_cases$ID_cant)
}

Table_cases<-merge(Table_cases, Labels %>% dplyr::select(Id, STATUS), 
                   by.x=c("ID_cant"), by.y="Id")

Table_cases_R<- Table_cases %>% filter(STATUS=='COAST')

SE <- sort(unique(Table_cases_R$SE))

Cases_week_r<-function(SEe){
  Table_cant01 <- Table_cases_R %>% filter (SE==SEe)
  cases_acum<-0
  for (i in 1:length(Table_cant01$CANTON)){
    cases_acum<-cases_acum+Table_cant01[i,8]
  }
  cases_acum
}

Num_cases_week <- lapply(SE, Cases_week_r)

cases<-numeric()
for (i in 1:length(Num_cases_week)){
  a<-Num_cases_week[[i]]
  cases<-c(cases,a)
}

Table_Cases_week<- data.frame('SE'=SE,
                              'Num. CASOS 2021'=cases)

d<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Coast_Week_Year.csv")
Table_Week <- merge(d,Table_Cases_week, all=TRUE)
write.csv(Table_Week,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Coast_Week_Year.csv")

#write.csv(Table_Cases_week,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Coast_Week_Year.csv")



#Casos por semana en la region Amazonia

Table_cases_R<- Table_cases %>% filter(STATUS=='AMAZONIA')

SE <- sort(unique(Table_cases_R$SE))

Cases_week_r<-function(SEe){
  Table_cant01 <- Table_cases_R %>% filter (SE==SEe)
  cases_acum<-0
  for (i in 1:length(Table_cant01$CANTON)){
    cases_acum<-cases_acum+Table_cant01[i,8]
  }
  cases_acum
}

Num_cases_week <- lapply(SE, Cases_week_r)

cases<-numeric()
for (i in 1:length(Num_cases_week)){
  a<-Num_cases_week[[i]]
  cases<-c(cases,a)
}

Table_Cases_week<- data.frame('SE'=SE,
                              'Num. CASOS 2021'=cases)

d<- read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Amazonia_Week_Year.csv")
Table_Week <- merge(d,Table_Cases_week, all=TRUE)
write.csv(Table_Week,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Amazonia_Week_Year.csv")

#write.csv(Table_Cases_week,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Analisis Exploratorio/Amazonia_Week_Year.csv")

