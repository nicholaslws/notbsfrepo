getwd()
setwd("C:\\Users\\Nick\\Desktop\\DinoLite\\measurements\\10phorid")

library(readxl)
library(ggplot2)
data1 <- read_excel("phorids.xlsx")

part_order<-c("Head","Thorax","Abdomen")
p_len<-ggplot(data=data1, aes(x=Part, y=Length, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC)+
  theme_gray()+ scale_x_discrete(limits = part_order)
p_len


p_vol<-ggplot(data=data1, aes(x=Part, y=Volume, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC)+
  theme_gray()+ scale_x_discrete(limits = part_order)
p_vol

=======
BSF data

setwd("C:\\Users\\Nick\\Desktop\\BSF")
getwd()
library(readxl)
df<-read_excel("BSF data.xlsx",  range = cell_cols("A:G"))
library(dplyr)
df %>% 
  group_by(Line, `Line ID`) %>% 
  summarise(Total = sum(Egg_mass, na.rm = TRUE))

aggregate(df$Egg_mass, by=list(df$Line, df$`Line ID`), sum)
=======
  
Phorid compairson
data_cecid <- read_excel("phorids.xlsx", sheet="cecids")

part_order_cecid<-c("Head","Thorax","Abdomen","Ovipositor")
p_len_cecid<-ggplot(data=data_cecid, aes(x=Part, y=Length, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC, ncol = 5)+
  theme_gray()+ scale_x_discrete(limits = part_order_cecid)
p_len_cecid


p_vol_cecid<-ggplot(data=data_cecid, aes(x=Part, y=Volume, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC, ncol = 5,scales = "free")+
  theme_gray()+ scale_x_discrete(limits = part_order_cecid)
p_vol_cecid

=========
chiro comprison

data_chiro <- read_excel("phorids.xlsx", sheet="chiro")

part_order_chiro<-c("Head","Thorax","Abdomen")
p_len_chiro<-ggplot(data=data_chiro, aes(x=Part, y=Length, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC, ncol = 5, scales="free")+
  theme_gray()+ scale_x_discrete(limits = part_order_chiro)
p_len_chiro


p_vol_chiro<-ggplot(data=data_chiro, aes(x=Part, y=Volume, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC, ncol = 5,scales = "free")+
  theme_gray()+ scale_x_discrete(limits = part_order_chiro)
p_vol_chiro

data_chiro

