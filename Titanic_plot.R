###Graph:
library(ggplot2)
library(latticeExtra)
library(plotly)
#Class and no.of passanger:
ggplot(Class_df,aes(x=Class,y=`No. of Passengers`,fill=Class))+geom_bar(stat = "identity",position = "dodge")+
  ggtitle("Class and no.of passanger")

#Male-Female and no. of passanger
ggplot(Sex_df,aes(x=Sex,y=`No. of Passengers`,fill=Sex))+
  geom_point(stat = "identity",position = "identity",aes(size=`No. of Passengers`,color=Sex))
+ggtitle("Male-Female and no. of passanger")

#Age and No. of passangers
ggplot(Age_df,aes(x=Age,y=`No. of Passangers`,fill=Age))+geom_bar(stat = "identity",position = "dodge")+
  ggtitle("Age and No. of passangers")


##Servied and died
ggplot(server_df,aes(x=Servied,y=`No.of passangers`))+geom_boxplot(stat ="boxplot" ,aes(size=`No.of passangers`,color=Servied))+
  ggtitle("Servied ")

##Class-Sex-Freq
ggplot(cla_Se_Fre,aes(x=Sex,y=Frequency))+geom_point(aes(size=Frequency,color=Class))+ggtitle("Class contain no. male & female")

##class-Age-Fre
cla_age_Fre
plot_ly(x=cla_age_Fre$Class, y=cla_age_Fre$Age, z=cla_age_Fre$Frequnecy, type="scatter3d", mode="markers",color =cla_age_Fre$Class)%>%
  
  layout(
    
    title = "Class -Age - No. of Passangers",
    
    scene = list(
      
      xaxis = list(title = "Class"),
      
      yaxis = list(title = "Age"),
      
      zaxis = list(title = "No. Of Passangers")
      
    ))