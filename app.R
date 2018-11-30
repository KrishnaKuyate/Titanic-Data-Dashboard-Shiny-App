
library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)
library(magick)
library(utils)


ui<-shinyUI(
  
  dashboardPage(
                 dashboardHeader(title = "Titanic"),
                 dashboardSidebar(
                   sidebarMenu(
                     menuItem(text = "About",tabName = "About",icon = icon("adn")),
                     menuItem(text = "History",tabName = "History",icon = icon("dharmachakra")),
                     ##Submenu:
                     menuItem(text = "Data",tabName = "Data",icon = icon("database"),
                              menuSubItem(text = "Statistics Table",tabName = "Statistics Table",icon = icon("table")),
                              menuSubItem(text = "Chart",tabName = "Charts",icon = icon("chart-area"))),
                     menuItem(text = "Titanic Movie",tabName = "Titanic Movie",icon = icon("ship")),
                     menuItem(text = "Gallary",tabName = "Gallary",icon = icon("camera-retro"))
                     
                     )
                  
                 ),
                 
                 
                 dashboardBody(
                   
                   tabItems(
                     ##Tab1:About
                     tabItem(tabName = "About",fluidRow(textOutput('about_txt'),tags$head(tags$style("#about_txt{color: black;font-size: 20px;font-style: italic;}"))),
                             fluidRow(imageOutput("about_img1"))
                             ),
                     ##tab2
                     tabItem(tabName ="History",
                             fluidRow(textOutput('H_P1'),tags$head(tags$style("#H_P1{color: black;font-size: 20px;font-style: italic;}"))),
                             fluidRow(textOutput('H_P2'),tags$head(tags$style("#H_P2{color: black;font-size: 20px;font-style: italic;}"))),
                             fluidRow(textOutput('H_P3'),tags$head(tags$style("#H_P3{color: black;font-size: 20px;font-style: italic;}"))),
                             fluidRow(textOutput('H_P4'),tags$head(tags$style("#H_P4{color: black;font-size: 20px;font-style: italic;}"))),
                             fluidRow(textOutput('H_P5'),tags$head(tags$style("#H_P5{color: black;font-size: 20px;font-style: italic;}"))),
                             fluidRow(textOutput('H_P6'),tags$head(tags$style("#H_P6{color: black;font-size: 20px;font-style: italic;}")))
                             
                             
                             ),
                     ##Subtab
                     
                     
                       tabItem(tabName ="Statistics Table",
                               fluidRow(box(title = NULL,dataTableOutput("main_table")),box(title = NULL,dataTableOutput("Class_nopass_t"))),
                               fluidRow(box(title = NULL,dataTableOutput("maleFemale_nopass_t")),box(title = NULL,dataTableOutput("age_no_pass_t"))),
                               fluidRow(box(title = NULL,dataTableOutput("servied_t")),box(title = NULL,dataTableOutput("class_sex_fre_t"))),
                               fluidRow(box(title = NULL,dataTableOutput),box(title = NULL,dataTableOutput("class_servived_fre_t")))
                               
                               
                               ),
                       tabItem(tabName ="Charts",
                               
                               fluidRow(box(title = NULL,plotOutput("Class_N_passanger")),box(title = NULL,plotlyOutput("male_female"))),
                               fluidRow(box(title = NULL,plotlyOutput("age_no_pass")),box(title = NULL,plotlyOutput("servied_died"))),
                               fluidRow(box(title = NULL,plotlyOutput("class_sex_freq")),box(title = NULL,plotlyOutput("class_age_fre")))
                             
                               ),
                    
                     
                     tabItem(tabName = "Gallary",p("Gall")),
                     tabItem(tabName = "Titanic Movie",p("mov"))
                     
                     
                   )
                   
                   
                 )
    
    
    

    
    
  )
  
)

#########Server call
server<-shinyServer(
  
  
  function(input,output,session)
  {
    
    ###About:
    ##About Text
    output$about_txt<-renderText({
      fileName <- "about.txt"
      conn <- file(fileName,open="r")
      linn <-readLines(conn)
      for (i in 1:length(linn)){
        print(linn[i])
      }
      return(linn )
      close(conn)
    })
    ##About Text end
    ##About Image
    output$about_img1<-renderImage({
      
      return(list(
        src = "about.jpg",
        filetype = "image/jpeg",
        alt = "This is a chainring"
      ))
      
    })
    ##About Image End
    ###About End
    
    ##History 
    #P1
    output$H_P1<-renderText({
      fileName1 <- "H_P1.txt"
      conn1 <- file(fileName1,open="r")
      linn1 <-readLines(conn1)
      for (i in 1:length(linn1)){
        print(linn1[i])
      }
      return(linn1 )
      close(conn1)
    })
    #end p1
    #P2
    output$H_P2<-renderText({
      fileName2 <- "H_P2.txt"
      conn2 <- file(fileName2,open="r")
      linn2 <-readLines(conn2)
      for (i in 1:length(linn2)){
        print(linn2[i])
      }
      return(linn2 )
      close(conn2)
    })
    #EndP2
    #p3
    output$H_P3<-renderText({
      fileName2 <- "H_P3.txt"
      conn2 <- file(fileName2,open="r")
      linn2 <-readLines(conn2)
      for (i in 1:length(linn2)){
        print(linn2[i])
      }
      return(linn2 )
      close(conn2)
    })
    #end p3
    #P4
    output$H_P4<-renderText({
      fileName2 <- "H_P4.txt"
      conn2 <- file(fileName2,open="r")
      linn2 <-readLines(conn2)
      for (i in 1:length(linn2)){
        print(linn2[i])
      }
      return(linn2 )
      close(conn2)
    })
    
    #End P4
    #p5
    output$H_P5<-renderText({
      fileName2 <- "H_P5.txt"
      conn2 <- file(fileName2,open="r")
      linn2 <-readLines(conn2)
      for (i in 1:length(linn2)){
        print(linn2[i])
      }
      return(linn2 )
      close(conn2)
    })
    
    #End p5
    #P6
    output$H_P6<-renderText({
      fileName2 <- "H_P6.txt"
      conn2 <- file(fileName2,open="r")
      linn2 <-readLines(conn2)
      for (i in 1:length(linn2)){
        print(linn2[i])
      }
      return(linn2 )
      close(conn2)
    })
    #End P6
    ###History end
    ####Data
    ##Statistics
    #Main Table:
    output$main_table<-renderDataTable({kable(titanic_df)%>%kable_styling(font_size = 15)%>%column_spec(1:5,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    
    
    #Class and no.of passanger:
    output$Class_nopass_t<-renderDataTable({aggregate(titanic_df$Freq,by=list(titanic_df$Class),FUN=sum)->Class_df
      as.data.frame(Class_df)->Class_df
      colnames(Class_df)<-c("Class","No. of Passengers")
      kable(Class_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    #Male-Female and no. of passanger:
    output$maleFemale_nopass_t<-renderDataTable({aggregate(titanic_df$Freq,by=list(titanic_df$Sex),FUN=sum)->Sex_df
      as.data.frame(Sex_df)->Sex_df
      colnames(Sex_df)<-c("Sex","No. of Passangers")
      kable(Sex_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    #Age and No. of passangers:
    output$age_no_pass_t<-renderDataTable({aggregate(titanic_df$Freq,by=list(titanic_df$Age),FUN=sum)->Age_df
      as.data.frame(Age_df)->Age_df
      colnames(Age_df)<-c("Age","No. of Passangers")
      kable(Age_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    #Servied and died
    
    output$servied_t<-renderDataTable({aggregate(titanic_df$Freq,by=list(titanic_df$Survived),FUN=sum)->server_df
      as.data.frame(server_df)->server_df
      colnames(server_df)<-c("Servied","No.of passangers")
      kable(server_df)%>%kable_styling(font_size = 15)%>%column_spec(1:2,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    
    ##Class-Sex-Freq
    
    output$class_sex_fre_t<-renderDataTable({aggregate(titanic_df$Freq,by=list(titanic_df$Class,titanic_df$Sex),FUN=sum)->cla_Se_Fre
      as.data.frame(cla_Se_Fre)->cla_Se_Fre
      colnames(cla_Se_Fre)<-c("Class","Sex","Frequency")
      kable(cla_Se_Fre)%>%kable_styling(font_size = 15)%>%column_spec(1:3,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    ##class-Age-Fre
    
    output$class_age_fre_t<-renderDataTable({aggregate(titanic_df$Freq,by=list(titanic_df$Class,titanic_df$Age),FUN=sum)->cla_age_Fre
      as.data.frame(cla_age_Fre)->cla_age_Fre
      colnames(cla_age_Fre)<-c("Class","Age","Frequnecy")
      kable(cla_age_Fre)%>%kable_styling(font_size = 15)%>%column_spec(1:3,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    #Class-Servived-Fre
    
    output$class_servived_fre_t<-renderDataTable({aggregate(titanic_df$Freq,by=list(titanic_df$Class,titanic_df$Survived),FUN=sum)->cla_se_fr
      as.data.frame(cla_se_fr)->cla_se_fr
      colnames(cla_se_fr)<-c("Class","Servived","Frequency")
      kable(cla_se_fr)%>%kable_styling(font_size = 15)%>%column_spec(1:3,color ="blue" ,background = "yellow")%>% 
        scroll_box(height = "600px")})
    ##Statistics End
    ##Chart
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
    
    ##Chart End
    
    ####Data End
  }
)

##shiny app

shinyApp(ui = ui, server = server)

