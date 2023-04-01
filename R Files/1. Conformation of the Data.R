library(dplyr)
library(stringr)

c_2014<-read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Egresos Hospitalarios/egresos_hospitalarios_2014.csv")
FC<-c_2014 %>% filter(cau_cie10=='Fiebre del dengue (dengue cl\xe1sico)')
FH<-c_2014 %>% filter(cau_cie10=='Fiebre del dengue hemorr\xe1gico')

c_total<-rbind(FC,FH)

SE<-numeric()

for (i in 1:length(c_total$cod_est)){
  date<-strsplit(c_total[i,21], "/")
  date<-unlist(date)
  year<-as.numeric(date[3])
  mes<-as.numeric(date[1])
  dia<-as.numeric(date[2])
  if (year==2013){
    SE<-c(SE,1)
  }
  if (year==2014){
    if (mes==1){
      if (dia<=5){
        SE<-c(SE,1)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,2)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,3)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,4)
      }
      if (dia>26){
        SE<-c(SE,5)
      }
    }
    if (mes==2){
      if (dia<=2){
        SE<-c(SE,5)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,6)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,7)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,8)
      }
      if (dia>23){
        SE<-c(SE,9)
      }
    }
    if (mes==3){
      if (dia<=2){
        SE<-c(SE,9)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,10)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,11)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,12)
      }
      if (dia>23 & dia<=30){
        SE<-c(SE,13)
      }
      if (dia>30){
        SE<-c(SE,14)
      } 
    }
    if (mes==4){
      if (dia<=6){
        SE<-c(SE,14)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,15)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,16)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,17)
      }
      if (dia>27){
        SE<-c(SE,18)
      } 
    }
    if (mes==5){
      if (dia<=4){
        SE<-c(SE,18)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,19)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,20)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,21)
      }
      if (dia>25){
        SE<-c(SE,22)
      }
    }
    if (mes==6){
      if (dia<=1){
        SE<-c(SE,22)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,23)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,24)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,25)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,26)
      }
      if (dia>29){
        SE<-c(SE,27)
      }
    }
    if (mes==7){
      if (dia<=6){
        SE<-c(SE,27)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,28)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,29)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,30)
      }
      if (dia>27){
        SE<-c(SE,31)
      }
    }
    if (mes==8){
      if (dia<=3){
        SE<-c(SE,31)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,32)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,33)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,34)
      }
      if (dia>24){
        SE<-c(SE,35)
      }
    }
    if (mes==9){
      if (dia<=7){
        SE<-c(SE,36)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,37)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,38)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,39)
      }
      if (dia>28){
        SE<-c(SE,40)
      }
    }
    if (mes==10){
      if (dia<=5){
        SE<-c(SE,40)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,41)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,42)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,43)
      }
      if (dia>26){
        SE<-c(SE,44)
      }
    }
    if (mes==11){
      if (dia<=2){
        SE<-c(SE,44)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,45)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,46)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,47)
      }
      if (dia>23){
        SE<-c(SE,48)
      }
    }
    if (mes==12){
      if (dia<=7){
        SE<-c(SE,49)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,50)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,51)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,52)
      }
      if (dia>28){
        SE<-c(SE,52)
      }
    }
  }
}

Table_test <- data.frame("AÑO"=rep(2014,length(c_total$cod_est)),
                         "PROVINCIA"= c_total$prov_ubi,
                         "CANTON"= c_total$cant_ubi,
                         "ID_cant"= c_total$cant_ubi,
                         "PARROQUIA"= c_total$parr_ubi,
                         "ENFERMEDAD"= c_total$cau_cie10,
                         "SE"=SE,
                         "NUM. CASOS"=rep(1, length(c_total$cod_est)))

write.csv(Table_test,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2014.csv")



c_2015<-read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Egresos Hospitalarios/egresos_hospitalarios_2015.csv")
FC<-c_2015 %>% filter(cau_cie10=='Fiebre del dengue [dengue cl\xe1sico]')
FH<-c_2015 %>% filter(cau_cie10=='Fiebre del dengue hemorr\xe1gico')

c_total<-rbind(FC,FH)

SE<-numeric()

for (i in 1:length(c_total$prov_ubi)){
  date<-strsplit(c_total[i,24], "/")
  date<-unlist(date)
  year<-as.numeric(date[3])
  mes<-as.numeric(date[1])
  dia<-as.numeric(date[2])
  if (year==2014){
    SE<-c(SE,1)
  }
  if (year==2015){
    if (mes==1){
      if (dia<=4){
        SE<-c(SE,1)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,2)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,3)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,4)
      }
      if (dia>25){
        SE<-c(SE,5)
      }
    }
    if (mes==2){
      if (dia<=1){
        SE<-c(SE,5)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,6)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,7)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,8)
      }
      if (dia>22){
        SE<-c(SE,9)
      }
    }
    if (mes==3){
      if (dia<=1){
        SE<-c(SE,9)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,10)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,11)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,12)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,13)
      }
      if (dia>29){
        SE<-c(SE,14)
      } 
    }
    if (mes==4){
      if (dia<=5){
        SE<-c(SE,14)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,15)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,16)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,17)
      }
      if (dia>26){
        SE<-c(SE,18)
      } 
    }
    if (mes==5){
      if (dia<=3){
        SE<-c(SE,18)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,19)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,20)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,21)
      }
      if (dia>24){
        SE<-c(SE,22)
      }
    }
    if (mes==6){
      if (dia<=7){
        SE<-c(SE,23)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,24)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,25)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,26)
      }
      if (dia>28){
        SE<-c(SE,27)
      }
    }
    if (mes==7){
      if (dia<=5){
        SE<-c(SE,27)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,28)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,29)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,30)
      }
      if (dia>26){
        SE<-c(SE,31)
      }
    }
    if (mes==8){
      if (dia<=2){
        SE<-c(SE,31)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,32)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,33)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,34)
      }
      if (dia>23 & dia<=30){
        SE<-c(SE,35)
      }
      if (dia>30){
        SE<-c(SE,36)
      }
    }
    if (mes==9){
      if (dia<=6){
        SE<-c(SE,36)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,37)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,38)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,39)
      }
      if (dia>27){
        SE<-c(SE,40)
      }
    }
    if (mes==10){
      if (dia<=4){
        SE<-c(SE,40)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,41)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,42)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,43)
      }
      if (dia>25){
        SE<-c(SE,44)
      }
    }
    if (mes==11){
      if (dia<=1){
        SE<-c(SE,44)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,45)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,46)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,47)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,48)
      }
      if (dia>29){
        SE<-c(SE,49)
      }
    }
    if (mes==12){
      if (dia<=6){
        SE<-c(SE,49)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,50)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,51)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,52)
      }
      if (dia>27){
        SE<-c(SE,52)
      }
    }
  }
}

Table_test <- data.frame("AÑO"=rep(2015,length(c_total$prov_ubi)),
                         "PROVINCIA"= c_total$prov_ubi,
                         "CANTON"= c_total$cant_ubi,
                         "ID_cant"= c_total$cant_ubi,
                         "PARROQUIA"= c_total$parr_ubi,
                         "ENFERMEDAD"= c_total$cau_cie10,
                         "SE"=SE,
                         "NUM. CASOS"=rep(1, length(c_total$prov_ubi)))

write.csv(Table_test,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2015.csv")



c_2016<-read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Egresos Hospitalarios/egresos_hospitalarios_2016.csv")
c_A<- c_2016 %>% filter(cau_cie10=='A90')
c_A1<- c_2016 %>% filter(cau_cie10=='A91')

c_total<-rbind(c_A,c_A1)

SE<-numeric()

for (i in 1:length(c_total$prov_ubi)){
  date<-strsplit(c_total[i,24], "/")
  date<-unlist(date)
  year<-as.numeric(date[3])
  mes<-as.numeric(date[1])
  dia<-as.numeric(date[2])
  if (year==2015){
    SE<-c(SE,1)
  }
  if (year==2016){
    if (mes==1){
      if (dia<=3){
        SE<-c(SE,1)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,2)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,3)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,4)
      }
      if (dia>24){
        SE<-c(SE,5)
      }
    }
    if (mes==2){
      if (dia<=7){
        SE<-c(SE,6)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,7)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,8)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,9)
      }
      if (dia>28){
        SE<-c(SE,10)
      }
    }
    if (mes==3){
      if (dia<=6){
        SE<-c(SE,10)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,11)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,12)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,13)
      }
      if (dia>27){
        SE<-c(SE,14)
      } 
    }
    if (mes==4){
      if (dia<=3){
        SE<-c(SE,14)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,15)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,16)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,17)
      }
      if (dia>24){
        SE<-c(SE,18)
      } 
    }
    if (mes==5){
      if (dia<=1){
        SE<-c(SE,18)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,19)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,20)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,21)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,22)
      }
      if (dia>29){
        SE<-c(SE,23)
      }
    }
    if (mes==6){
      if (dia<=5){
        SE<-c(SE,23)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,24)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,25)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,26)
      }
      if (dia>26){
        SE<-c(SE,27)
      }
    }
    if (mes==7){
      if (dia<=3){
        SE<-c(SE,27)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,28)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,29)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,30)
      }
      if (dia>24){
        SE<-c(SE,31)
      }
    }
    if (mes==8){
      if (dia<=7){
        SE<-c(SE,32)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,33)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,34)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,35)
      }
      if (dia>28){
        SE<-c(SE,36)
      }
    }
    if (mes==9){
      if (dia<=4){
        SE<-c(SE,36)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,37)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,38)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,39)
      }
      if (dia>25){
        SE<-c(SE,40)
      }
    }
    if (mes==10){
      if (dia<=2){
        SE<-c(SE,40)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,41)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,42)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,43)
      }
      if (dia>23 & dia<=30){
        SE<-c(SE,44)
      }
      if (dia>30){
        SE<-c(SE,45)
      }
    }
    if (mes==11){
      if (dia<=6){
        SE<-c(SE,45)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,46)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,47)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,48)
      }
      if (dia>27){
        SE<-c(SE,49)
      }
    }
    if (mes==12){
      if (dia<=4){
        SE<-c(SE,49)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,50)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,51)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,52)
      }
      if (dia>25){
        SE<-c(SE,52)
      }
    }
  }
}

Table_test <- data.frame("AÑO"=rep(2016,length(c_total$prov_ubi)),
                         "PROVINCIA"= c_total$prov_ubi,
                         "CANTON"= c_total$cant_ubi,
                         "ID_cant"= c_total$cant_ubi,
                         "PARROQUIA"= c_total$parr_ubi,
                         "ENFERMEDAD"= c_total$cau_cie10,
                         "SE"=SE,
                         "NUM. CASOS"=rep(1, length(c_total$prov_ubi)))

write.csv(Table_test,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2016.csv")



c_2017<-read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Egresos Hospitalarios/egresos_hospitalarios_2017.csv")
FC<-c_2017 %>% filter(cau_cie10=='Fiebre del dengue [dengue cl\xe1sico]')
FH<-c_2017 %>% filter(cau_cie10=='Fiebre del dengue hemorr\xe1gico')

c_total<-rbind(FC,FH)

SE<-numeric()

for (i in 1:length(c_total$prov_ubi)){
  date<-strsplit(c_total[i,24], "/")
  date<-unlist(date)
  year<-as.numeric(date[3])
  mes<-as.numeric(date[1])
  dia<-as.numeric(date[2])
  if (year==2016){
    SE<-c(SE,1)
  }
  if (year==2017){
    if (mes==1){
      if (dia<=8){
        SE<-c(SE,1)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,2)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,3)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,4)
      }
      if (dia>29){
        SE<-c(SE,5)
      }
    }
    if (mes==2){
      if (dia<=5){
        SE<-c(SE,5)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,6)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,7)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,8)
      }
      if (dia>26){
        SE<-c(SE,9)
      }
    }
    if (mes==3){
      if (dia<=5){
        SE<-c(SE,9)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,10)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,11)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,12)
      }
      if (dia>26){
        SE<-c(SE,13)
      } 
    }
    if (mes==4){
      if (dia<=2){
        SE<-c(SE,13)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,14)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,15)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,16)
      }
      if (dia>23){
        SE<-c(SE,17)
      } 
    }
    if (mes==5){
      if (dia<=7){
        SE<-c(SE,18)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,19)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,20)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,21)
      }
      if (dia>28){
        SE<-c(SE,22)
      }
    }
    if (mes==6){
      if (dia<=4){
        SE<-c(SE,22)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,23)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,24)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,25)
      }
      if (dia>25){
        SE<-c(SE,26)
      }
    }
    if (mes==7){
      if (dia<=2){
        SE<-c(SE,26)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,27)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,28)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,29)
      }
      if (dia>23 & dia<=30){
        SE<-c(SE,30)
      }
      if (dia>30){
        SE<-c(SE,31)
      }
    }
    if (mes==8){
      if (dia<=6){
        SE<-c(SE,31)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,32)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,33)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,34)
      }
      if (dia>27){
        SE<-c(SE,35)
      }
    }
    if (mes==9){
      if (dia<=3){
        SE<-c(SE,35)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,36)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,37)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,38)
      }
      if (dia>24){
        SE<-c(SE,39)
      }
    }
    if (mes==10){
      if (dia<=1){
        SE<-c(SE,39)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,40)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,41)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,42)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,43)
      }
      if (dia>29){
        SE<-c(SE,44)
      }
    }
    if (mes==11){
      if (dia<=5){
        SE<-c(SE,44)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,45)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,46)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,47)
      }
      if (dia>26){
        SE<-c(SE,48)
      }
    }
    if (mes==12){
      if (dia<=3){
        SE<-c(SE,48)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,49)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,50)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,51)
      }
      if (dia>24){
        SE<-c(SE,52)
      }
    }
  }
}

Table_test <- data.frame("AÑO"=rep(2017,length(c_total$prov_ubi)),
                         "PROVINCIA"= c_total$prov_ubi,
                         "CANTON"= c_total$cant_ubi,
                         "ID_cant"= c_total$cant_ubi,
                         "PARROQUIA"= c_total$parr_ubi,
                         "ENFERMEDAD"= c_total$cau_cie10,
                         "SE"=SE,
                         "NUM. CASOS"=rep(1, length(c_total$prov_ubi)))

write.csv(Table_test,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2017.csv")



c_2018<-read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Egresos Hospitalarios/egresos_hospitalarios_2018.csv")
c_A<- c_2018 %>% filter(cau_cie10=='A90')
c_A1<- c_2018 %>% filter(cau_cie10=='A91')

c_total<-rbind(c_A,c_A1)

SE<-numeric()

for (i in 1:length(c_total$prov_ubi)){
  date<-strsplit(c_total[i,24], "/")
  date<-unlist(date)
  year<-as.numeric(date[3])
  mes<-as.numeric(date[2])
  dia<-as.numeric(date[1])
  if (year==2017){
    SE<-c(SE,1)
  }
  if (year==2018){
    if (mes==1){
      if (dia<=7){
        SE<-c(SE,1)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,2)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,3)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,4)
      }
      if (dia>28){
        SE<-c(SE,5)
      }
    }
    if (mes==2){
      if (dia<=4){
        SE<-c(SE,5)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,6)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,7)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,8)
      }
      if (dia>25){
        SE<-c(SE,9)
      }
    }
    if (mes==3){
      if (dia<=4){
        SE<-c(SE,9)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,10)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,11)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,12)
      }
      if (dia>25){
        SE<-c(SE,13)
      } 
    }
    if (mes==4){
      if (dia<=1){
        SE<-c(SE,13)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,14)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,15)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,16)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,17)
      }
      if (dia>29){
        SE<-c(SE,18)
      } 
    }
    if (mes==5){
      if (dia<=6){
        SE<-c(SE,18)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,19)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,20)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,21)
      }
      if (dia>27){
        SE<-c(SE,22)
      }
    }
    if (mes==6){
      if (dia<=3){
        SE<-c(SE,22)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,23)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,24)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,25)
      }
      if (dia>24){
        SE<-c(SE,26)
      }
    }
    if (mes==7){
      if (dia<=1){
        SE<-c(SE,26)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,27)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,28)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,29)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,30)
      }
      if (dia>29){
        SE<-c(SE,31)
      }
    }
    if (mes==8){
      if (dia<=5){
        SE<-c(SE,31)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,32)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,33)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,34)
      }
      if (dia>26){
        SE<-c(SE,35)
      }
    }
    if (mes==9){
      if (dia<=2){
        SE<-c(SE,35)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,36)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,37)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,38)
      }
      if (dia>23){
        SE<-c(SE,39)
      }
    }
    if (mes==10){
      if (dia<=7){
        SE<-c(SE,40)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,41)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,42)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,43)
      }
      if (dia>28){
        SE<-c(SE,44)
      }
    }
    if (mes==11){
      if (dia<=4){
        SE<-c(SE,44)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,45)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,46)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,47)
      }
      if (dia>25){
        SE<-c(SE,48)
      }
    }
    if (mes==12){
      if (dia<=2){
        SE<-c(SE,48)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,49)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,50)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,51)
      }
      if (dia>23){
        SE<-c(SE,52)
      }
    }
  }
}

Table_test <- data.frame("AÑO"=rep(2018,length(c_total$prov_ubi)),
                         "PROVINCIA"= c_total$prov_ubi,
                         "CANTON"= c_total$cant_ubi,
                         "ID_cant"= c_total$cant_ubi,
                         "PARROQUIA"= c_total$parr_ubi,
                         "ENFERMEDAD"= c_total$cau_cie10,
                         "SE"=SE,
                         "NUM. CASOS"=rep(1, length(c_total$prov_ubi)))

write.csv(Table_test,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2018.csv")



c_2019<-read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Egresos Hospitalarios/egresos_hospitalarios_2019.csv")
c_A<- c_2019 %>% filter(cau_cie10=='A90')
c_A1<- c_2019 %>% filter(cau_cie10=='A91')

c_total<-rbind(c_A,c_A1)

SE<-numeric()

for (i in 1:length(c_total$prov_ubi)){
  date<-strsplit(c_total[i,24], "/")
  date<-unlist(date)
  year<-as.numeric(date[3])
  mes<-as.numeric(date[2])
  dia<-as.numeric(date[1])
  if (year==2018){
    SE<-c(SE,1)
  }
  if (year==2019){
    if (mes==1){
      if (dia<=6){
        SE<-c(SE,1)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,2)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,3)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,4)
      }
      if (dia>27){
        SE<-c(SE,5)
      }
    }
    if (mes==2){
      if (dia<=3){
        SE<-c(SE,5)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,6)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,7)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,8)
      }
      if (dia>24){
        SE<-c(SE,9)
      }
    }
    if (mes==3){
      if (dia<=3){
        SE<-c(SE,9)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,10)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,11)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,12)
      }
      if (dia>24){
        SE<-c(SE,13)
      } 
    }
    if (mes==4){
      if (dia<=7){
        SE<-c(SE,14)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,15)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,16)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,17)
      }
      if (dia>28){
        SE<-c(SE,18)
      } 
    }
    if (mes==5){
      if (dia<=5){
        SE<-c(SE,18)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,19)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,20)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,21)
      }
      if (dia>26){
        SE<-c(SE,22)
      }
    }
    if (mes==6){
      if (dia<=2){
        SE<-c(SE,22)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,23)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,24)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,25)
      }
      if (dia>23){
        SE<-c(SE,26)
      }
    }
    if (mes==7){
      if (dia<=7){
        SE<-c(SE,27)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,28)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,29)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,30)
      }
      if (dia>28){
        SE<-c(SE,31)
      }
    }
    if (mes==8){
      if (dia<=4){
        SE<-c(SE,31)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,32)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,33)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,34)
      }
      if (dia>25){
        SE<-c(SE,35)
      }
    }
    if (mes==9){
      if (dia<=1){
        SE<-c(SE,35)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,36)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,37)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,38)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,39)
      }
      if (dia>29){
        SE<-c(SE,40)
      }
    }
    if (mes==10){
      if (dia<=6){
        SE<-c(SE,40)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,41)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,42)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,43)
      }
      if (dia>27){
        SE<-c(SE,44)
      }
    }
    if (mes==11){
      if (dia<=3){
        SE<-c(SE,44)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,45)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,46)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,47)
      }
      if (dia>24){
        SE<-c(SE,48)
      }
    }
    if (mes==12){
      if (dia<=1){
        SE<-c(SE,48)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,49)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,50)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,51)
      }
      if (dia>22){
        SE<-c(SE,52)
      }
    }
  }
}

Table_test <- data.frame("AÑO"=rep(2019,length(c_total$prov_ubi)),
                         "PROVINCIA"= c_total$prov_ubi,
                         "CANTON"= c_total$cant_ubi,
                         "ID_cant"= c_total$cant_ubi,
                         "PARROQUIA"= c_total$parr_ubi,
                         "ENFERMEDAD"= c_total$cau_cie10,
                         "SE"=SE,
                         "NUM. CASOS"=rep(1, length(c_total$prov_ubi)))

write.csv(Table_test,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2019.csv")



c_2020<-read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Egresos Hospitalarios/egresos_hospitalarios_2020.csv")
c_A<- c_2020 %>% filter(cau_cie10=='A970')
c_A1<- c_2020 %>% filter(cau_cie10=='A971')
c_A2<- c_2020 %>% filter(cau_cie10=='A972')
c_A9<- c_2020 %>% filter(cau_cie10=='A979')

c_total<-rbind(c_total,c_A9)

SE<-numeric()

for (i in 1:length(c_total$prov_ubi)){
  year<-c_total[i,23]
  mes<-c_total[i,24]
  dia<-c_total[i,25]
  if (year==2019){
    SE<-c(SE,1)
  }
  if (year==2020){
    if (mes==1){
      if (dia<=5){
        SE<-c(SE,1)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,2)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,3)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,4)
      }
      if (dia>26){
        SE<-c(SE,5)
      }
    }
    if (mes==2){
      if (dia<=2){
        SE<-c(SE,5)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,6)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,7)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,8)
      }
      if (dia>23){
        SE<-c(SE,9)
      }
    }
    if (mes==3){
      if (dia<=1){
        SE<-c(SE,9)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,10)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,11)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,12)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,13)
      }
      if (dia>29){
        SE<-c(SE,14)
      } 
    }
    if (mes==4){
      if (dia<=5){
        SE<-c(SE,14)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,15)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,16)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,17)
      }
      if (dia>26){
        SE<-c(SE,18)
      } 
    }
    if (mes==5){
      if (dia<=3){
        SE<-c(SE,18)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,19)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,20)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,21)
      }
      if (dia>24){
        SE<-c(SE,22)
      }
    }
    if (mes==6){
      if (dia<=7){
        SE<-c(SE,23)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,24)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,25)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,26)
      }
      if (dia>28){
        SE<-c(SE,27)
      }
    }
    if (mes==7){
      if (dia<=5){
        SE<-c(SE,27)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,28)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,29)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,30)
      }
      if (dia>26){
        SE<-c(SE,31)
      }
    }
    if (mes==8){
      if (dia<=2){
        SE<-c(SE,31)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,32)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,33)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,34)
      }
      if (dia>23 & dia<=30){
        SE<-c(SE,35)
      }
      if (dia>30){
        SE<-c(SE,36)
      }
    }
    if (mes==9){
      if (dia<=6){
        SE<-c(SE,36)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,37)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,38)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,39)
      }
      if (dia>27){
        SE<-c(SE,40)
      }
    }
    if (mes==10){
      if (dia<=4){
        SE<-c(SE,40)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,41)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,42)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,43)
      }
      if (dia>25){
        SE<-c(SE,44)
      }
    }
    if (mes==11){
      if (dia<=1){
        SE<-c(SE,44)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,45)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,46)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,47)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,48)
      }
      if (dia>29){
        SE<-c(SE,49)
      }
    }
    if (mes==12){
      if (dia<=6){
        SE<-c(SE,49)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,50)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,51)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,52)
      }
      if (dia>27){
        SE<-c(SE,52)
      }
    }
  }
}

Table_test <- data.frame("AÑO"=rep(2020,length(c_total$prov_ubi)),
                         "PROVINCIA"= c_total$prov_ubi,
                         "CANTON"= c_total$cant_ubi,
                         "ID_cant"= c_total$cant_ubi,
                         "PARROQUIA"= c_total$parr_ubi,
                         "ENFERMEDAD"= c_total$cau_cie10,
                         "SE"=SE,
                         "NUM. CASOS"=rep(1, length(c_total$prov_ubi)))

write.csv(Table_test,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2020.csv")



c_2021<-read.csv2("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Egresos Hospitalarios/egresos_hospitalarios_2021.csv")
c_A<- c_2021 %>% filter(cau_cie10=='Dengue con signos de alarma')
c_A1<- c_2021 %>% filter(cau_cie10=='Dengue no especificado')
c_A2<- c_2021 %>% filter(cau_cie10=='Dengue severo')
c_A9<- c_2021 %>% filter(cau_cie10=='Dengue sin signos de alarma')

c_total<-rbind(c_total,c_A9)

SE<-numeric()

for (i in 1:length(c_total$prov_ubi)){
  date<-strsplit(c_total[i,24], "/")
  date<-unlist(date)
  day<-strsplit(date[3], " ")
  day<-unlist(day)
  year<-as.numeric(date[1])
  mes<-as.numeric(date[2])
  dia<-as.numeric(day[1])
  if (year==2020){
    SE<-c(SE,1)
  }
  if (year==2021){
    if (mes==1){
      if (dia<=3){
        SE<-c(SE,1)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,2)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,3)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,4)
      }
      if (dia>24){
        SE<-c(SE,5)
      }
    }
    if (mes==2){
      if (dia<=7){
        SE<-c(SE,6)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,7)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,8)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,9)
      }
      if (dia>28){
        SE<-c(SE,10)
      }
    }
    if (mes==3){
      if (dia<=7){
        SE<-c(SE,10)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,11)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,12)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,13)
      }
      if (dia>28){
        SE<-c(SE,14)
      } 
    }
    if (mes==4){
      if (dia<=4){
        SE<-c(SE,14)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,15)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,16)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,17)
      }
      if (dia>25){
        SE<-c(SE,18)
      } 
    }
    if (mes==5){
      if (dia<=2){
        SE<-c(SE,18)
      }
      if (dia>2 & dia<=9){
        SE<-c(SE,19)
      }
      if (dia>9 & dia<=16){
        SE<-c(SE,20)
      }
      if (dia>16 & dia<=23){
        SE<-c(SE,21)
      }
      if (dia>23 & dia<=30){
        SE<-c(SE,22)
      }
      if (dia>30){
        SE<-c(SE,23)
      }
    }
    if (mes==6){
      if (dia<=6){
        SE<-c(SE,23)
      }
      if (dia>6 & dia<=13){
        SE<-c(SE,24)
      }
      if (dia>13 & dia<=20){
        SE<-c(SE,25)
      }
      if (dia>20 & dia<=27){
        SE<-c(SE,26)
      }
      if (dia>27){
        SE<-c(SE,27)
      }
    }
    if (mes==7){
      if (dia<=4){
        SE<-c(SE,27)
      }
      if (dia>4 & dia<=11){
        SE<-c(SE,28)
      }
      if (dia>11 & dia<=18){
        SE<-c(SE,29)
      }
      if (dia>18 & dia<=25){
        SE<-c(SE,30)
      }
      if (dia>25){
        SE<-c(SE,31)
      }
    }
    if (mes==8){
      if (dia<=1){
        SE<-c(SE,31)
      }
      if (dia>1 & dia<=8){
        SE<-c(SE,33)
      }
      if (dia>8 & dia<=15){
        SE<-c(SE,34)
      }
      if (dia>15 & dia<=22){
        SE<-c(SE,35)
      }
      if (dia>22 & dia<=29){
        SE<-c(SE,35)
      }
      if (dia>29){
        SE<-c(SE,36)
      }
    }
    if (mes==9){
      if (dia<=5){
        SE<-c(SE,36)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,37)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,38)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,39)
      }
      if (dia>26){
        SE<-c(SE,40)
      }
    }
    if (mes==10){
      if (dia<=3){
        SE<-c(SE,40)
      }
      if (dia>3 & dia<=10){
        SE<-c(SE,41)
      }
      if (dia>10 & dia<=17){
        SE<-c(SE,42)
      }
      if (dia>17 & dia<=24){
        SE<-c(SE,43)
      }
      if (dia>24){
        SE<-c(SE,44)
      }
    }
    if (mes==11){
      if (dia<=7){
        SE<-c(SE,45)
      }
      if (dia>7 & dia<=14){
        SE<-c(SE,46)
      }
      if (dia>14 & dia<=21){
        SE<-c(SE,47)
      }
      if (dia>21 & dia<=28){
        SE<-c(SE,48)
      }
      if (dia>28){
        SE<-c(SE,49)
      }
    }
    if (mes==12){
      if (dia<=5){
        SE<-c(SE,49)
      }
      if (dia>5 & dia<=12){
        SE<-c(SE,50)
      }
      if (dia>12 & dia<=19){
        SE<-c(SE,51)
      }
      if (dia>19 & dia<=26){
        SE<-c(SE,52)
      }
      if (dia>26){
        SE<-c(SE,52)
      }
    }
  }
}

Table_test <- data.frame("AÑO"=rep(2021,length(c_total$prov_ubi)),
                         "PROVINCIA"= c_total$prov_ubi,
                         "CANTON"= c_total$cant_ubi,
                         "ID_cant"= c_total$cant_ubi,
                         "PARROQUIA"= c_total$parr_ubi,
                         "ENFERMEDAD"= c_total$cau_cie10,
                         "SE"=SE,
                         "NUM. CASOS"=rep(1, length(c_total$prov_ubi)))

write.csv(Table_test,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Annual Cases Dengue/Cases_2021.csv")
