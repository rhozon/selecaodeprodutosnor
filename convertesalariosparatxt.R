jan2020<-read.csv(file="C:/Users/rodri/Desktop/Bases CAGED/CAGED Novo/CAGEDMOV202001.txt",head=TRUE,sep=";")
str(jan2020)

saljan2020<-jan2020$salÃ.rio
library(readr)
write.table(saljan2020, file = "saljan2020.txt", sep = "\t",
            row.names = FALSE, col.names = "salario")


plot(saljan2020)

fev2020<-read.csv(file="C:/Users/rodri/Desktop/Bases CAGED/CAGED Novo/CAGEDMOV202002.txt",head=TRUE,sep=";")
str(fev2020)
salfev2020<-fev2020$salÃ.rio
write.table(saljan2020, file = "salfev2020.txt", sep = "\t",
            row.names = FALSE, col.names = "salario")


mar2020<-read.csv(file="C:/Users/rodri/Desktop/Bases CAGED/CAGED Novo/CAGEDMOV202003.txt",head=TRUE,sep=";")
str(mar2020)
salmar2020<-mar2020$salÃ.rio
write.table(salmar2020, file = "salmar2020.txt", sep = "\t",
            row.names = FALSE, col.names = "salario")

abr2020<-read.csv(file="C:/Users/rodri/Desktop/Bases CAGED/CAGED Novo/CAGEDMOV202004.txt",head=TRUE,sep=";")
str(abr2020)

mai2020<-read.csv(file="C:/Users/rodri/Desktop/Bases CAGED/CAGED Novo/CAGEDMOV202005.txt",head=TRUE,sep=";")
str(mai2020)
salmai2020<-abr2020$salÃ.rio
write.table(salabr2020, file = "salmai2020.txt", sep = "\t",
            row.names = FALSE, col.names = "salario")

jun2020<-read.csv(file="C:/Users/rodri/Desktop/Bases CAGED/CAGED Novo/CAGEDMOV202006.txt",head=TRUE,sep=";")
str(jun2020)
saljun2020<-jun2020$salÃ.rio
write.table(saljun2020, file = "saljun2020.txt", sep = "\t",
            row.names = FALSE, col.names = "salario")

jul2020<-read.csv(file="C:/Users/rodri/Desktop/Bases CAGED/CAGED Novo/CAGEDMOV202007.txt",head=TRUE,sep=";")
str(jul2020)
saljul2020<-jul2020$salÃ.rio
write.table(saljul2020, file = "saljul2020.txt", sep = "\t",
            row.names = FALSE, col.names = "salario")

ago2020<-read.csv(file="C:/Users/rodri/Desktop/Bases CAGED/CAGED Novo/CAGEDMOV202008.txt",head=TRUE,sep=";")
str(ago2020)
salago2020<-ago2020$salÃ.rio
write.table(salago2020, file = "salago2020.txt", sep = "\t",
            row.names = FALSE, col.names = "salario")

salario<-rbind(saljan2020,salfev2020,salmar2020,salabr2020,salmai2020,saljun2020,saljul2020,salago2020)

library(dplyr)
saljan2020<-saljan2020%>%
  mutate(Mes="Jan/2020")
str(saljan2020)
library(readr)
write.table(saljan2020, file = "saljan2020.txt", sep = "\t",
            row.names = FALSE)
