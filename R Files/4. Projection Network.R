library(tnet)

Table_test06<-read.csv("C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes por canton/Bipartite 2021.csv")

canton <- unique(Table_test06$CANTON)
ID_cant <- (1:length(canton))

Table_ID <- data.frame('ID'=ID_cant,
                       'CANTON'=canton)
#write.csv(Table_ID,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes por canton/ID_Label 2019.csv")

for (i in 1:length(canton)) {
  name<- canton[i]
  replace<- ID_cant[i]
  Table_test06$CANTON <- sub(name,replace,Table_test06$CANTON)
}

net.w <- cbind(
  i=c(as.numeric(Table_test06$CANTON)),
  p=c(Table_test06$SE),
  w=c(Table_test06$NÚM..CASOS))

Table_finally <- projecting_tm(net.w, method="Newman")

write.csv(Table_finally,"C:/Users/miche/Desktop/Michell Universidad/Tesis/Data/Redes por canton/Network 2021.csv")
