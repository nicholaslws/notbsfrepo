getwd()

#Load libraries
library(ggplot2)
library(stringr)

#In the following sections, I want to see if there is difference in body part sizes (length/vol) between measurers.
#I will just be plotting some graphs for a quick visualisation.

################
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

################
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

################
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

################
#pyscho comprison
data_psycho <- read.csv("psycho_data.csv")

part_order_psycho<-c("Head","Thorax","Abdomen")
p_len_psycho<-ggplot(data=data_psycho, aes(x=Part, y=Length, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC, ncol = 5, scales="free")+
  theme_gray()+ scale_x_discrete(limits = part_order_psycho)
p_len_psycho


p_vol_psycho<-ggplot(data=data_psycho, aes(x=Part, y=Volume, fill=Measurer)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ZRC, ncol = 5,scales = "free")+
  theme_gray()+ scale_x_discrete(limits = part_order_psycho)
p_vol_psycho


################
#All together

#Add taxa to the DFs
data_phorid$taxa<-rep("phorid",nrow(data_phorid))
data_cecid$taxa<-rep("cecid",nrow(data_cecid))
data_chiro$taxa<-rep("chiro",nrow(data_chiro))
data_psycho$taxa<-rep("psycho",nrow(data_psycho))

#Row bind the DFs to analyse them together.
combined_df<-rbind(data_cecid,data_phorid,data_chiro, data_psycho)

#simple, nonsense regression?
m1<-lm(Volume~Length, data=combined_df)
summary(m1)

m2<-lm(Volume~Length+taxa, data=combined_df)
summary(m2)

aov1<-aov(Volume~taxa+ Measurer, data=combined_df)
summary(aov1)
aovplot<-ggplot(data=combined_df, aes(x=Measurer, y = Volume, fill= Measurer))+
  geom_boxplot()
aovplot
#One big outlier. Try to remove it

#remove outlier
plot(aov1) #104 is clearly the outlier
combined_df2<-combined_df[-104,]
combined_df2<-combined_df2[!combined_df$Part=="Full",] #cuts off summed length/vol
aovplot2<-ggplot(data=combined_df2, aes(x=Measurer, y = Volume, fill= Measurer))+
  geom_boxplot()+
  facet_wrap(~taxa+Part, scales="free")
aovplot2   #cuts off some parts haha

#Ok, add some random models guys.
m3<-lm(Volume~Length, data=combined_df2)
summary(m3)
