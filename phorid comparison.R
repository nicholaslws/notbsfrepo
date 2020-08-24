getwd()

library(ggplot2)
library(stringr)


#phorid comparison
data_phorid <- read.csv("phorid_data.csv")
part_order<-c("Head","Thorax","Abdomen")
p_len<-ggplot(data=data_phorid, aes(x=Part, y=Length, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC)+
  theme_gray()+ scale_x_discrete(limits = part_order)
p_len


p_vol<-ggplot(data=data_phorid, aes(x=Part, y=Volume, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC)+
  theme_gray()+ scale_x_discrete(limits = part_order)
p_vol

=======
#cecid compairson
data_cecid <- read.csv("cecid_data.csv")

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

#chiro comprison
data_chiro <- read.csv("chiro_data.csv")
data_chiro <- data_chiro[c(1:5)]

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

=====
#All together

#Add taxa to the 3 DFs
data_phorid$taxa<-rep("phorid",nrow(data_phorid))
data_cecid$taxa<-rep("cecid",nrow(data_cecid))
data_chiro$taxa<-rep("chiro",nrow(data_chiro))

combined_df<-rbind(data_cecid,data_phorid,data_chiro)

#simple regression?
m1<-lm(Volume~Length, data=combined_df)
summary(m1)

m2<-lm(Volume~Length+taxa, data=combined_df)
summary(m2)

aov1<-aov(Volume~taxa+ Measurer, data=combined_df)
summary(aov1)
aovplot<-ggplot(data=combined_df, aes(x=Measurer, y = Volume, fill= Measurer))+
  geom_boxplot()
aovplot


#remove outlier
plot(aov1) #104 is outlier
combined_df2<-combined_df[-104,]
combined_df2<-combined_df2[!combined_df$Part=="Full",] #cuts off summed length/vol
aovplot2<-ggplot(data=combined_df2, aes(x=Measurer, y = Volume, fill= Measurer))+
  geom_boxplot()+
  facet_wrap(~taxa+Part, scales="free")
aovplot2   #cuts off some parts haha

