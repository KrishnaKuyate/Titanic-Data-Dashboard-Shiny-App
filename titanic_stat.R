###Titanic stat:
library(dplyr)
library(knitr)
library(kableExtra)

View(Titanic)

###Table###
##as datafram:
titanic_df<-as.data.frame(Titanic)

##Total passangers:
sum(titanic_df$Freq)

#Main Table:
kable(titanic_df)%>%kable_styling(font_size = 15)%>%column_spec(1:5,color ="blue" ,background = "yellow")%>% 
  scroll_box(height = "600px")

#Class and no.of passanger:
aggregate(titanic_df$Freq,by=list(titanic_df$Class),FUN=sum)->Class_df
as.data.frame(Class_df)->Class_df
colnames(Class_df)<-c("Class","No. of Passengers")
kable(Class_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
  scroll_box(height = "600px")

#Male-Female and no. of passanger
aggregate(titanic_df$Freq,by=list(titanic_df$Sex),FUN=sum)->Sex_df
as.data.frame(Sex_df)->Sex_df
colnames(Sex_df)<-c("Sex","No. of Passangers")
kable(Sex_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
  scroll_box(height = "600px")


#Age and No. of passangers
aggregate(titanic_df$Freq,by=list(titanic_df$Age),FUN=sum)->Age_df
as.data.frame(Age_df)->Age_df
colnames(Age_df)<-c("Age","No. of Passangers")
kable(Age_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
  scroll_box(height = "600px")

#Servied and died
aggregate(titanic_df$Freq,by=list(titanic_df$Survived),FUN=sum)->server_df
as.data.frame(server_df)->server_df
colnames(server_df)<-c("Servied","No.of passangers")
kable(server_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
  scroll_box(height = "600px")


##Class-Sex-Freq
aggregate(titanic_df$Freq,by=list(titanic_df$Class,titanic_df$Sex),FUN=sum)->cla_Se_Fre
as.data.frame(cla_Se_Fre)->cla_Se_Fre
colnames(cla_Se_Fre)<-c("Class","Sex","Frequency")
kable(cla_Se_Fre)%>%kable_styling(font_size = 15)%>%column_spec(1:3,color ="blue" ,background = "yellow")%>% 
  scroll_box(height = "600px")


##class-Age-Fre
aggregate(titanic_df$Freq,by=list(titanic_df$Class,titanic_df$Age),FUN=sum)->cla_age_Fre
as.data.frame(cla_age_Fre)->cla_age_Fre
colnames(cla_age_Fre)<-c("Class","Age","Frequnecy")
kable(cla_age_Fre)%>%kable_styling(font_size = 15)%>%column_spec(1:3,color ="blue" ,background = "yellow")%>% 
  scroll_box(height = "600px")


#Class-Servived-Fre
aggregate(titanic_df$Freq,by=list(titanic_df$Class,titanic_df$Survived),FUN=sum)->cla_se_fr
as.data.frame(cla_se_fr)->cla_se_fr
colnames(cla_se_fr)<-c("Class","Servived","Frequency")
kable(cla_se_fr)%>%kable_styling(font_size = 15)%>%column_spec(1:3,color ="blue" ,background = "yellow")%>% 
  scroll_box(height = "600px")
