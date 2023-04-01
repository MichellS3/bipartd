library(dplyr)
library(tidyverse)

Table_test<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2019.csv")

canton <- unique(Table_test$CANTON)
ID_cant <- (1:length(canton))

Table_ID <- data.frame('ID'=ID_cant,
                       'CANTON'=canton)
write.csv(Table_ID,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes por canton/ID_Label 2019.csv")

for (i in 1:length(canton)) {
  name<- canton[i]
  replace<- ID_cant[i]
  Table_test$ID_cant <- sub(name,replace,Table_test$ID_cant)
}

Table_test01<-Table_test

provT<-character()
cantT<-character()
numCT<-numeric()
weekT<-numeric()

Division_cant<-function(cod_cant){
  Table_div_01<-Table_test01 %>% filter(ID_cant==cod_cant)
  print(length(Table_div_01$CANTON))
  if (length(Table_div_01$CANTON)>1){
    for (i in 1:((length(Table_div_01$CANTON))-1)) {
      sum<-0
      numC01<-Table_div_01[i,8] #Numero de casos
      sum<-sum+numC01
      for (j in (i+1):(length(Table_div_01$CANTON))) {
        sem01<-Table_div_01[i,7] #Semana epidemiologica 01
        sem02<-Table_div_01[j,7] #Semana epidemiologica 02
        if (sem01 == sem02) {
          provNom<-Table_div_01[i,2] #provincia
          cantNom<-Table_div_01[i,3] #Canton
          numC02<-Table_div_01[j,8]
          sum<-sum+numC02
          for (k in 1:8) {
            Table_div_01[j,k] = 0
          }
        } 
      }
      if (numC01!=sum){
        provT<-c(provT,provNom)
        cantT<-c(cantT,cantNom)
        weekT<-c(weekT,sem01)
        numCT<-c(numCT,sum)
      }
    }
  } else{
    provT<-Table_div_01[1,2]
    cantT<-Table_div_01[1,3]
    weekT<-Table_div_01[1,7]
    numCT<-Table_div_01[1,8]
  }
  Table_div_02 <- data.frame("PROVINCIA"= provT,
                             "CANTON"= cantT,
                             "SE"=weekT,
                             "NÚM. CASOS"=numCT)
  Lis_data<-c(Table_div_01,Table_div_02)
} 

a<-lapply(ID_cant, Division_cant)

#Segunda Parte
Table_test<- data.frame("AÑO"=numeric(),
                        "PROVINCIA"= character(),
                        "CANTON"=character(),
                        "ID_cant"= numeric(),
                        "PARROQUIA"=character(),
                        "ENFERMEDAD"=character(),
                        "SE"=numeric(),
                        "NÚM. CASOS"=numeric())
Table_test02 <- data.frame("PROVINCIA"= character(),
                           "CANTON"= character(),
                           "SE"=numeric(),
                           "NÚM. CASOS"=numeric())
for (i in 1:(length(a))){
  Table_div_03 <- data.frame("AÑO"=a[[i]][[1]],
                             "PROVINCIA"= a[[i]][[2]],
                             "CANTON"=a[[i]][[3]],
                             "ID_cant"= a[[i]][[4]],
                             "PARROQUIA"=a[[i]][[5]],
                             "ENFERMEDAD"=a[[i]][[6]],
                             "SE"=a[[i]][[7]],
                             "NÚM. CASOS"=a[[i]][[8]])
  Table_test <- rbind(Table_test,Table_div_03)
  Table_div_04 <- data.frame("PROVINCIA"= a[[i]][[9]],
                             "CANTON"=a[[i]][[10]],
                             "SE"=a[[i]][[11]],
                             "NÚM. CASOS"=a[[i]][[12]])
  Table_test02 <- rbind(Table_test02,Table_div_04)
}

Table_test02<- Table_test02 %>% drop_na(CANTON)

#Borramos los repetidos con la Tabla 02

acum<-0
for (i in 1:(length(Table_test02$CANTON))) {
  for (j in 1:(length(Table_test$CANTON))) {
    cant11<- Table_test02[i,2]
    cant12<-Table_test[j,3]
    sem11<-Table_test02[i,3]
    sem12<-Table_test[j,7]
    if (cant11==cant12) {
      if (sem11==sem12) {
        for (k in 1:8) {
          Table_test[j,k] = 0
        }
        acum<-acum+1
      }
    }
  }
}

#Eliminar las filas con 0 

Table_test03<-Table_test

for (i in 1:(length(Table_test$CANTON))){
  cer<-Table_test[i,4]
  if(cer==0) {
    for (k in 1:8) {
      Table_test[i,k] = NA
    }
  }
}

#Eliminar las filas NA

Table_test04<- Table_test %>% drop_na(CANTON)

#Tabla de 3 columnas

Table_test05 <- data.frame("PROVINCIA"= Table_test04$PROVINCIA,
                           "CANTON"=Table_test04$CANTON,
                           "SE"=Table_test04$SE,
                           "NÚM. CASOS"=Table_test04$NÚM..CASOS)

#Join la tabla 2 y 5

Table_test06 <- rbind(Table_test02,Table_test05) #AQUI

for (i in 1:length(canton)) {
  name<- canton[i]
  replace<- ID_cant[i]
  Table_test06$CANTON <- sub(name,replace,Table_test06$CANTON)
}

write.csv(Table_test06,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes por canton/Bipartite 2019.csv")

