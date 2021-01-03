library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(tiger, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(rlang, warn.conflicts = FALSE)
library(klaR, warn.conflicts = FALSE)
library(leaflet)
#library(sf)
#library(rgdal)
library(tidyverse)
library(DBI)
library(odbc)


con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "ODBC Driver 17 for SQL Server",
                      Server = "tcp:cmserv.database.windows.net",
                      Database = "cm",
                      UID = "utkuco",
                      #PWD = rstudioapi::askForPassword("Database password"),
                      PWD = "Utku-coskun1",
                      Port = 1433)

moov<- dbGetQuery(con, "SELECT * FROM datatbl")




ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Moov by Garenta"),
                    
                    dashboardSidebar( sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
                      
                    )), #Sidebar sonu
                    
                    
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = "dashboard",
                                    fluidRow(
                                        column(2,
                                               textInput(inputId = "tarih11",label =  "Başlangıç Tarih Saat:", value = "2019-11-01 00:00:00" ),
                                               textInput(inputId = "tarih12",label =  "Bitiş Tarih Saat:", value = "2019-11-08 00:00:00" )
                                        ),
                                        column(2,
                                               textInput(inputId = "tarih21",label =  "Kıyaslanacak Başlangıç Tarih Saat:", value = "2019-11-08 00:00:00" ),
                                               textInput(inputId = "tarih22",label =  "Kıyaslanacak Bitiş Tarih Saat:", value = "2019-11-15 00:00:00" )
                                        ),
                                        column(2,
                                               tags$br(),
                                               tags$br(),
                                               tags$br(),
                                               actionButton("do", "Go go go!")
                                        )
                                    ),#rowsonu
                                    
                                    fluidRow(
                                        column(4,
                                               h4("Ana Parametreler"),
                                               tags$br(),
                                               #DT::dataTableOutput("searchtablo"),
                                               
                                               h4("Performans KPI'ları Özet Tablo Tablo"),
                                               tags$br(),
                                               DT::dataTableOutput("ozettablo")
                                        ),
                                        
                                        column(4,
                                               #h3("Performans KPI'ları Özet Tablo Tablo"),
                                               selectInput(inputId = "plotchoose", label = "KPI Seç", choices =c("Kirasayi"="ks","Ciro"="ci","Musterisayisi"="mu","Kampanya"="ka")),
                                               tags$br(),
                                               plotOutput("dashplot")
                                        ),
                                        column(4,
                                               #h3("Performans KPI'ları Özet Tablo Tablo"),
                                               tags$br(),
                                               tags$br(),
                                               tags$br(),
                                               tags$br(),
                                               tags$br(),
                                               plotOutput("dashplot2")
                                        )
                                    ),#rowsonu
                                    
                                    fluidRow(
                                        column(7,
                                               h3("Performans KPI'ları Ayrıntılı Tablo"),
                                               h4("Günler"),
                                               tags$br(),
                                               DT::dataTableOutput("mytable")
                                        )
                                    ), #rowsonu
                                    
                                    fluidRow(
                                        column(4,
                                               h4("Bölgeler"),
                                               tags$br(),
                                               DT::dataTableOutput("dashtablobolgeler")
                                        )
                                    ), #rowsonu
                                    
                                    fluidRow(
                                        column(4,
                                               h4("Yaş Grubu"),
                                               tags$br(),
                                               DT::dataTableOutput("dashtabloyasgrubu")
                                        )
                                    ) #rowsonu
                                    
                                    
                                 
                                    
                                    
                            )
                           
                                    
                                    
                            )
                        ) #tabitemsonu
                        
                    )#Tabitems sonu


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$do, {
        
        output$ozettablo = DT::renderDataTable({
            ozet1<- moov %>% filter(kirasaat1>=as.POSIXct( input$tarih11 , origin = "1899-12-30", tz="GMT")  & kirasaat1<= as.POSIXct( input$tarih12 , origin = "1899-12-30", tz="GMT") ) %>% 
                summarise(Kiralamasayisi=n(),
                          Ciro=sum(kampanyasiztutar,na.rm = TRUE),
                          Musterisayisi=n_distinct(Customer_ID),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kampanya=length(Kampanya[Kampanya!="-"]),
                          Paketli=length(Paket_adi[Paket_adi!="-"]),
                          İlkkiralama=length(İlk.Kiralama.Durumu[İlk.Kiralama.Durumu=="True"])
                )
            
            ozet2<- moov %>% filter(kirasaat1>= as.POSIXct( input$tarih21 , origin = "1899-12-30", tz="GMT")  & kirasaat1<= as.POSIXct( input$tarih22 , origin = "1899-12-30", tz="GMT") )  %>% 
                summarise(Kiralamasayisi2=n(),
                          Ciro2=sum(kampanyasiztutar,na.rm = TRUE),
                          Musterisayisi2=n_distinct(Customer_ID),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kampanya2=length(Kampanya[Kampanya!="-"]),
                          Paketli=length(Paket_adi[Paket_adi!="-"]),
                          İlkkiralama=length(İlk.Kiralama.Durumu[İlk.Kiralama.Durumu=="True"])
                )
            
            ozet1<- as.data.frame(t(ozet1)) 
            ozet2<- as.data.frame(t(ozet2)) 
            ozet3<- cbind(ozet1,ozet2)
            names(ozet3)[1]<-"Tarih1"
            names(ozet3)[2]<-"Tarih2"
            
            ozet3$Degisim<- percent((ozet3$Tarih2/ozet3$Tarih1)-1) 
            ozet3$degisim<- (ozet3$Tarih2/ozet3$Tarih1)-1
            
            brks<- quantile(ozet3$degisim, probs = seq(.05, .95, .05), na.rm = TRUE)
            clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
            clrs<- rev(clrs)
            
            ozet3<- datatable(ozet3,options=list(columnDefs = list(list(visible=FALSE, targets=4)))) %>% formatStyle(columns = names(ozet3)[3], valueColumns =names(ozet3)[4]  , backgroundColor = styleInterval(brks, clrs)) 
            
            
        })
        
        output$mytable = DT::renderDataTable({
            kiraadet<- moov %>% filter(kirasaat1>=as.POSIXct( input$tarih11 , origin = "1899-12-30", tz="GMT")  & kirasaat1<= as.POSIXct( input$tarih12 , origin = "1899-12-30", tz="GMT"))  %>% group_by(kiratarih1) %>% 
                summarise(Gun1=last(gunler),
                          Kirasayi1=n(),
                          Ciro1=sum(kampanyasiztutar,na.rm = TRUE),
                          Mussayi1=n_distinct(tc),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kamp1=length(Kampanya[Kampanya!="-"])
                          #Paketli1=length(Paket_adi[Paket_adi!="-"]),
                          #İlkkira1=length(İlk.Kiralama.Durumu[İlk.Kiralama.Durumu=="True"])
                )
            names(kiraadet)[1]<-"KiraTarih1"
            
            kiraadet2<- moov %>% filter(kirasaat1>= as.POSIXct( input$tarih21 , origin = "1899-12-30", tz="GMT") & kirasaat1<= as.POSIXct( input$tarih22 , origin = "1899-12-30", tz="GMT")  )     %>%
                group_by(kiratarih1) %>% 
                summarise(Gun2=last(gunler),
                          Kirasayi2=n(),
                          Ciro2=sum(kampanyasiztutar,na.rm = TRUE),
                          Mussayi2=n_distinct(tc),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kamp2=length(Kampanya[Kampanya!="-"])
                          #Paketli2=length(Paket_adi[Paket_adi!="-"]),
                          #İlkkira2=length(İlk.Kiralama.Durumu[İlk.Kiralama.Durumu=="True"])
                )
            names(kiraadet2)[1]<-"KiraTarih2"
            
            kiraadet3<- cbind(kiraadet,kiraadet2,deparse.level = 0)
            kiraadet3$KiraTarih1<- substr(kiraadet3$KiraTarih1,start = 1,stop = 10)
            kiraadet3$KiraTarih2<- substr(kiraadet3$KiraTarih2,start = 1,stop = 10)
            
            kiraadet3$Kiradeg<- percent((kiraadet3$Kirasayi2/kiraadet3$Kirasayi1)-1) 
            kiraadet3$Cirodeg<- percent((kiraadet3$Ciro2/kiraadet3$Ciro1)-1)
            kiraadet3$Musdeg<- percent((kiraadet3$Mussayi2/kiraadet3$Mussayi1)-1)
            kiraadet3$Kampdeg<- percent((kiraadet3$Kamp2/kiraadet3$Kamp1)-1)
            
            #kiraadet3$Paketdeg<- percent((kiraadet3$Paketli2/kiraadet3$Paketli1)-1)
            #kiraadet3$Ilkkiradeg<- percent((kiraadet3$İlkkira2/kiraadet3$İlkkira1)-1)
            
            kiraadet3$kiradeg<- (kiraadet3$Kirasayi2/kiraadet3$Kirasayi1)-1
            kiraadet3$cirodeg<- (kiraadet3$Ciro2/kiraadet3$Ciro1)-1
            kiraadet3$musdeg<- (kiraadet3$Mussayi2/kiraadet3$Mussayi1)-1
            kiraadet3$kampdeg<- (kiraadet3$Kamp2/kiraadet3$Kamp1)-1
            
            #kiraadet3$paketdeg<- (kiraadet3$Paketli2/kiraadet3$Paketli1)-1
            #kiraadet3$ilkkiradeg<- (kiraadet3$Paketli2/kiraadet3$Paketli1)-1
            
            kiraadet3<- kiraadet3[,c(1,7,2,8,3,9,13,4,10,14,5,11,15,6,12,16,17,18,19,20)]
            
            brks<- quantile(kiraadet3$kiradeg, probs = seq(.05, .95, .05), na.rm = TRUE)
            clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
            clrs<- rev(clrs)
            
            kiraadet3<- datatable(kiraadet3,options=list(columnDefs = list(list(visible=FALSE, targets=c(17:20))))) %>% formatStyle(columns = names(kiraadet3)[c(7,10,13,16)], valueColumns =names(kiraadet3)[c(17:20)]  , backgroundColor = styleInterval(brks, clrs)) 
            kiraadet3
            
            
            
            
        })
        
        output$mytable2 = DT::renderDataTable({
            kiraadet<- moov %>% filter(kirasaat1>=as.POSIXct( input$tarih11 , origin = "1899-12-30", tz="GMT") & kirasaat1<= as.POSIXct( input$tarih12 , origin = "1899-12-30", tz="GMT"))  %>% group_by(kiratarih1) %>% 
                summarise(Gun1=last(gunler),
                          Kiralamasayisi1=n(),
                          Ciro1=sum(kampanyasiztutar,na.rm = TRUE),
                          Musterisayisi1=n_distinct(tc),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kampanya1=length(Kampanya[Kampanya!="-"])
                )
            names(kiraadet)[1]<-"KiraTarih1"
            
            kiraadet2<- moov %>% filter(kirasaat1>= as.POSIXct( input$tarih21 , origin = "1899-12-30", tz="GMT") & kirasaat1<= as.POSIXct( input$tarih22 , origin = "1899-12-30", tz="GMT")  )     %>%
                group_by(kiratarih1) %>% 
                summarise(Gun2=last(gunler),
                          Kiralamasayisi2=n(),
                          Ciro2=sum(kampanyasiztutar,na.rm = TRUE),
                          Musterisayisi2=n_distinct(tc),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kampanya2=length(Kampanya[Kampanya!="-"])
                )
            names(kiraadet2)[1]<-"KiraTarih2"
            
            kiraadet3<- cbind(kiraadet,kiraadet2)
            kiraadet3$KiraTarih1<- substr(kiraadet3$KiraTarih1,start = 1,stop = 10)
            kiraadet3$KiraTarih2<- substr(kiraadet3$KiraTarih2,start = 1,stop = 10)
            #Buradan sonra değişim başlıyor
            kiraadet4$DegisimKiraSayi<- round(kiraadet3$Kiralamasayisi1/kiraadet3$Kiralamasayisi2,2)  
            kiraadet4$DegisimCiro <- round(kiraadet3$Ciro1/kiraadet3$Ciro2,2)  
            kiraadet4$DegisimMusteri<-round(kiraadet3$Musterisayisi1/kiraadet3$Musterisayisi2,2)
            kiraadet4$DegisimKampanya<-round(kiraadet3$Kampanya1/kiraadet3$Kampanya2,2)
            kiraadet4<- as.data.frame( kiraadet4)
            kiraadet4 <- kiraadet4[ ,2:ncol(kiraadet4)]
            kiraadet4
            
        })
        
        #Plot için geçerli
        df <- reactive({ 
            df<- moov %>%filter(kirasaat1>=as.POSIXct( input$tarih11 , origin = "1899-12-30", tz="GMT")  & kirasaat1<= as.POSIXct( input$tarih12 , origin = "1899-12-30", tz="GMT")) %>%   
                group_by(kiratarih1) %>% summarise(ks=n(), 
                                                   ci=sum(kampanyasiztutar,na.rm = TRUE),    
                                                   mu=n_distinct(tc), 
                                                   ka=length(Kampanya[Kampanya!="-"])) %>% dplyr::select(kiratarih1,input$plotchoose )
        })
        
        output$dashplot <- renderPlot({
            
            dfs<-df()
            
            #ploting<- plot(unlist(dfs[,2])~unlist(dfs[,1]),type="l",col="red", lwd=5, xlab="Tarih", ylab="Değişken", main="Karşılaştırma Grafiği",xaxt = "n")+
            # axis(1,as.Date(dfs$kiratarih1),format(dfs$kiratarih1, "%b %d")) 
            #ploting
            
            dfs$kiratarih1 <- as.Date(dfs$kiratarih1, "%m/%d/%Y")
            data <- data.frame(x = unlist(dfs[,1]),y=unlist(dfs[,2]))
            data$x<-as.Date(data$x, origin = "1970-01-01", tz="GMT")
            p <- ggplot(data, aes(x=x, y=y)) +
                geom_line(colour="red",size=1.5) +
                xlab("Tarih1")+
                ylab("Değişken")
            p
            
        })
        
        #Plot için lazım
        df2 <- reactive({ 
            df2<- moov %>%filter(kirasaat1>=as.POSIXct( input$tarih21 , origin = "1899-12-30", tz="GMT")  & kirasaat1<= as.POSIXct( input$tarih22 , origin = "1899-12-30", tz="GMT")) %>%   
                group_by(kiratarih1) %>% summarise(ks=n(), 
                                                   ci=sum(kampanyasiztutar,na.rm = TRUE),    
                                                   mu=n_distinct(tc), 
                                                   ka=length(Kampanya[Kampanya!="-"])) %>% dplyr::select(kiratarih1,input$plotchoose )
        })
        
        output$dashplot2 <- renderPlot({
            
            dfs<-df2()
            
            #ploting<- plot(unlist(dfs[,2])~unlist(dfs[,1]),type="l",col="red", lwd=5, xlab="Tarih", ylab="Değişken", main="Karşılaştırma Grafiği",xaxt = "n")+
            # axis(1,as.Date(dfs$kiratarih1),format(dfs$kiratarih1, "%b %d")) 
            #ploting
            
            dfs$kiratarih1 <- as.Date(dfs$kiratarih1, "%m/%d/%Y")
            data <- data.frame(x = unlist(dfs[,1]),y=unlist(dfs[,2]))
            data$x<-as.Date(data$x, origin = "1970-01-01", tz="GMT")
            p <- ggplot(data, aes(x=x, y=y)) +
                geom_line(colour="red",size=1.5) +
                xlab("Tarih2")+
                ylab("Değişken")
            p
            
        })
        
        #dash bolgeler
        output$dashtablobolgeler = DT::renderDataTable({
            ozet1<- moov %>% filter(kirasaat1>=as.POSIXct( input$tarih11 , origin = "1899-12-30", tz="GMT")  & kirasaat1<= as.POSIXct( input$tarih12 , origin = "1899-12-30", tz="GMT")) %>% group_by(startloc) %>%
                summarise(Kirasayi1=n(),
                          Ciro1=sum(kampanyasiztutar,na.rm = TRUE),
                          Mussayi1=n_distinct(Customer_ID),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kamp1=length(Kampanya[Kampanya!="-"])
                )
            
            
            ozet2<- moov %>% filter(kirasaat1>= as.POSIXct( input$tarih21 , origin = "1899-12-30", tz="GMT") & kirasaat1<= as.POSIXct( input$tarih22 , origin = "1899-12-30", tz="GMT"))  %>% group_by(startloc) %>%
                summarise(Kirasayi2=n(),
                          Ciro2=sum(kampanyasiztutar,na.rm = TRUE),
                          Mussayi2=n_distinct(Customer_ID),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kamp2=length(Kampanya[Kampanya!="-"])
                )
            ozet3<- merge(ozet1,ozet2)
            
            ozet3$Kiradeg<- percent((ozet3$Kirasayi2/ozet3$Kirasayi1)-1) 
            ozet3$Cirodeg<- percent((ozet3$Ciro2/ozet3$Ciro1)-1)
            ozet3$Musdeg<- percent((ozet3$Mussayi2/ozet3$Mussayi1)-1)
            ozet3$Kampdeg<- percent((ozet3$Kamp2/ozet3$Kamp1)-1)
            ozet3$kiradeg<- (ozet3$Kirasayi2/ozet3$Kirasayi1)-1
            ozet3$cirodeg<- (ozet3$Ciro2/ozet3$Ciro1)-1
            ozet3$musdeg<- (ozet3$Mussayi2/ozet3$Mussayi1)-1
            ozet3$kampdeg<- (ozet3$Kamp2/ozet3$Kamp1)-1
            
            ozet3<- ozet3[,c(1,2,6,10,3,7,11,4,8,12,5,9,13,14,15,16,17)]
            
            brks<- quantile(ozet3$kiradeg, probs = seq(.05, .95, .05), na.rm = TRUE)
            clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
            clrs<- rev(clrs)
            
            ozet3<- datatable(ozet3,options=list(columnDefs = list(list(visible=FALSE, targets=c(14:17))))) %>% formatStyle(columns = names(ozet3)[c(4,7,10,13)], valueColumns =names(ozet3)[c(14:17)]  , backgroundColor = styleInterval(brks, clrs)) 
            ozet3
            
            
        })
        
        #dash yas grubu
        output$dashtabloyasgrubu = DT::renderDataTable({
            ozet1<- moov %>% filter(kirasaat1>= as.POSIXct( input$tarih11 , origin = "1899-12-30", tz="GMT")  & kirasaat1<=as.POSIXct( input$tarih12 , origin = "1899-12-30", tz="GMT")) %>% group_by(yasgrup) %>%
                summarise(Kirasayi1=n(),
                          Ciro1=sum(kampanyasiztutar,na.rm = TRUE),
                          Mussayi1=n_distinct(Customer_ID),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kamp1=length(Kampanya[Kampanya!="-"])
                )
            
            ozet2<- moov %>% filter(kirasaat1>= as.POSIXct( input$tarih21 , origin = "1899-12-30", tz="GMT") & kirasaat1<= as.POSIXct( input$tarih22 , origin = "1899-12-30", tz="GMT"))  %>% group_by(yasgrup) %>%
                summarise(Kirasayi2=n(),
                          Ciro2=sum(kampanyasiztutar,na.rm = TRUE),
                          Mussayi2=n_distinct(Customer_ID),
                          #Musteribasiciro=Ciro/Musterisayisi,
                          #Kiralamabasiciro=Ciro/Kiralamasayisi,
                          Kamp2=length(Kampanya[Kampanya!="-"])
                )
            #ozet3<- cbind(ozet1,ozet2,deparse.level = 0)
            ozet3<-merge(ozet1,ozet2)
            
            ozet3$Kiradeg<- percent((ozet3$Kirasayi2/ozet3$Kirasayi1)-1) 
            ozet3$Cirodeg<- percent((ozet3$Ciro2/ozet3$Ciro1)-1)
            ozet3$Musdeg<- percent((ozet3$Mussayi2/ozet3$Mussayi1)-1)
            ozet3$Kampdeg<- percent((ozet3$Kamp2/ozet3$Kamp1)-1)
            ozet3$kiradeg<- (ozet3$Kirasayi2/ozet3$Kirasayi1)-1
            ozet3$cirodeg<- (ozet3$Ciro2/ozet3$Ciro1)-1
            ozet3$musdeg<- (ozet3$Mussayi2/ozet3$Mussayi1)-1
            ozet3$kampdeg<- (ozet3$Kamp2/ozet3$Kamp1)-1
            
            ozet3<- ozet3[,c(1,2,6,10,3,7,11,4,8,12,5,9,13,14,15,16,17)]
            
            brks<- quantile(ozet3$kiradeg, probs = seq(.05, .95, .05), na.rm = TRUE)
            clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
            clrs<- rev(clrs)
            
            ozet3<- datatable(ozet3,options=list(columnDefs = list(list(visible=FALSE, targets=c(14:17))))) %>% formatStyle(columns = names(ozet3)[c(4,7,10,13)], valueColumns =names(ozet3)[c(14:17)]  , backgroundColor = styleInterval(brks, clrs)) 
            ozet3
            
        })

        
        
    })
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

