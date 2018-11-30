server<-shinyServer(
  
  
  function(input,output,session)
  {
    
    #####Plot:
    #Class and no.of passanger:
    output$Class_N_passanger<-renderPlot({ggplot(Class_df,aes(x=Class,y=`No. of Passengers`,fill=Class))+geom_bar(stat = "identity",position = "dodge")+ggtitle("Class and no.of passanger")})
    
    #Male-Female and no. of passanger:
    output$male_female<-renderPlotly({ggplot(Sex_df,aes(x=Sex,y=`No. of Passengers`,fill=Sex))+geom_point(stat = "identity",position = "identity",aes(size=`No. of Passengers`,color=Sex))+ggtitle("Male-Female and no. of passanger")})
    
    #Age and No. of passangers:
    output$age_no_pass<-renderPlotly({ggplot(Age_df,aes(x=Age,y=`No. of Passangers`,fill=Age))+geom_bar(stat = "identity",position = "dodge")+
        ggtitle("Age and No. of passangers")})
    
    #Servied and died:
    output$servied_died<-renderPlotly({ggplot(server_df,aes(x=Servied,y=`No.of passangers`))+geom_boxplot(stat ="boxplot" ,aes(size=`No.of passangers`,color=Servied))+
        ggtitle("Servied ")})
    
    ##Class-Sex-Freq
    output$class_sex_freq<-renderPlotly({ggplot(cla_Se_Fre,aes(x=Sex,y=Frequency))+geom_point(aes(size=Frequency,color=Class))+ggtitle("Class contain no. male & female")})
    
    ##class-Age-Fre
    output$class_age_fre<-renderPlotly({plot_ly(x=cla_age_Fre$Class, y=cla_age_Fre$Age, z=cla_age_Fre$Frequnecy, type="scatter3d", mode="markers",color =cla_age_Fre$Class)%>%
        
        layout(
          
          title = "Class -Age - No. of Passangers",
          
          scene = list(
            
            xaxis = list(title = "Class"),
            
            yaxis = list(title = "Age"),
            
            zaxis = list(title = "No. Of Passangers")
            
          ))})
    
    ##########Table:
    #Main Table:
    output$main_table<-renderTable({kable(titanic_df)%>%kable_styling(font_size = 15)%>%column_spec(1:5,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    
    #Class and no.of passanger:
    output$Class_nopass_t<-renderTable({aggregate(titanic_df$Freq,by=list(titanic_df$Class),FUN=sum)->Class_df
      as.data.frame(Class_df)->Class_df
      colnames(Class_df)<-c("Class","No. of Passengers")
      kable(Class_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    #Male-Female and no. of passanger:
    output$maleFemale_nopass_t<-renderTable({aggregate(titanic_df$Freq,by=list(titanic_df$Sex),FUN=sum)->Sex_df
      as.data.frame(Sex_df)->Sex_df
      colnames(Sex_df)<-c("Sex","No. of Passangers")
      kable(Sex_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    #Age and No. of passangers:
    output$age_no_pass_t<-renderTable({aggregate(titanic_df$Freq,by=list(titanic_df$Age),FUN=sum)->Age_df
      as.data.frame(Age_df)->Age_df
      colnames(Age_df)<-c("Age","No. of Passangers")
      kable(Age_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    #Servied and died
    
    output$servied_t<-renderTable({aggregate(titanic_df$Freq,by=list(titanic_df$Survived),FUN=sum)->server_df
      as.data.frame(server_df)->server_df
      colnames(server_df)<-c("Servied","No.of passangers")
      kable(server_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    
    ##Class-Sex-Freq
    
    output$class_sex_fre_t<-renderTable({aggregate(titanic_df$Freq,by=list(titanic_df$Class,titanic_df$Sex),FUN=sum)->cla_Se_Fre
      as.data.frame(cla_Se_Fre)->cla_Se_Fre
      colnames(cla_Se_Fre)<-c("Class","Sex","Frequency")
      kable(cla_Se_Fre)%>%kable_styling(font_size = 15)%>%column_spec(1:3,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    ##class-Age-Fre
    
    output$class_age_fre_t<-renderTable({aggregate(titanic_df$Freq,by=list(titanic_df$Class,titanic_df$Age),FUN=sum)->cla_age_Fre
      as.data.frame(cla_age_Fre)->cla_age_Fre
      colnames(cla_age_Fre)<-c("Class","Age","Frequnecy")
      kable(cla_age_Fre)%>%kable_styling(font_size = 15)%>%column_spec(1:3,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    #Class-Servived-Fre
    
    output$class_servived_fre_t<-renderTable({aggregate(titanic_df$Freq,by=list(titanic_df$Class,titanic_df$Survived),FUN=sum)->cla_se_fr
      as.data.frame(cla_se_fr)->cla_se_fr
      colnames(cla_se_fr)<-c("Class","Servived","Frequency")
      kable(cla_se_fr)%>%kable_styling(font_size = 15)%>%column_spec(1:3,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})

    ############About####################
    ##Content:
    output$about_text<-renderText({
      rawText <- readLines('about.txt')
      return(rawText)
    })
    
  }
)
  






renderDataTable()













  