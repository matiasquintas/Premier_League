
library(shiny)
library(ggplot2)
library(shinythemes)
library(plotly)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # themeSelector(),
   theme = shinytheme("superhero"),
    # Application title
    titlePanel(title=div(img(src="https://1.bp.blogspot.com/-qC4c_psdbfg/WPEES-RMzoI/AAAAAAAAAwA/4xLSPYvAgoE56iM92junl98KRa8aPuKGgCLcB/s1600/Premier%2BLeague%2BLogo.png",width = "100px", height = "100px")
                         ,'Premier League')),

    
    tabsetPanel(
        tabPanel('Búsqueda por Temporada',
                 sidebarLayout(
                    sidebarPanel(
                        selectInput('temporadas',
                                    label=h3(strong('Temporada')),
                                    choices = sort(unique(table_tot$Season), decreasing = TRUE)
                                    ),
                        tags$hr(),
                        checkboxInput(inputId='input_pred', 
                                      label=strong('Predecir próxima temporada'), 
                                      value = FALSE, width = NULL),
                        
                        hr(),
                        fluidRow(h3(strong('  Campeón'))),
                        h4(uiOutput('escudo_campeon')),
                        
                        
                        hr(),
                        fluidRow(h3(strong('  Goleador de la Temporada'))),
                        
                        h4(strong(textOutput('goleador'))),
                        br(),
                        hr(),
                        fluidRow(h3(strong('  Top 10 jugadores influyentes'))),
                        
                        
                        br(),
                        
                        h3(tableOutput("TablaCapital")),
                        br(),
                        h6(textOutput('Ref_Capital'))
                                ),
                    mainPanel(
                        plotlyOutput('campeones'),
                        tags$hr(),
                        plotlyOutput('rango'),
                        tags$hr(),
                        fluidRow(column(12, plotlyOutput('potxeq'))),
                        br(),
                        
                        tableOutput('TablaPred'),
                        h6(textOutput('Ref_TablaPred'))
                        
                              )
                            )
                 ),
 
 
        tabPanel('Búsqueda por Equipo',
                sidebarLayout(
                    sidebarPanel(
                        selectInput('equipos',
                                    label=h3(strong('Equipo')),
                                    choices = sort(unique(table_tot$Team))
                                    ),
                    hr(),
                    uiOutput("combo_temp"),
                    uiOutput('escudos'),
                    hr(),
                    fluidRow(h3(strong('  Goleador de la Temporada'))),
                    
                    h4(strong(textOutput('goleadorxequipo')))

                    ),
                   
                    mainPanel(
                        plotlyOutput('grafico4'),
                        tags$hr(),
                        fluidRow(column(12, plotlyOutput('grafico3'))),
                        br(),
                        tableOutput("TablaPlanilla"),
                        h6(textOutput('Ref_TablaPlanilla')))
                    
                        
                                
                            )
                )
            
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {

  rango1 <- table_tot
  rango1$Rango_Posicion <- ''
  
  for (i in 1:nrow(rango1)) {
    
    if (rango1$Capital_total[i]>=20000){
      rango1$Rango_Posicion[i]='1-4'
    }
    else if (rango1$Capital_total[i]<20000 & rango1$Capital_total[i]>=10000){
      rango1$Rango_Posicion[i]='4-10'
    }else{
      rango1$Rango_Posicion[i]='10-20'
    }
  }
  
  
  
  
  
  
    df.filt1 <- reactive({
        df.filt1= players_shiny[players_shiny$Season == input$output_temp & players_shiny$Team==input$equipos,]
        df.filt1
    })
    
    output$combo_temp <- renderUI({
        selectInput(inputId="output_temp", "Seleccionar la temporada",
                    choices = sort(unique(players_shiny[players_shiny$Team==input$equipos,'Season']), decreasing = TRUE), 
                    selected = 1)
                     })
   
    
    
     output$escudo_campeon<- renderUI({
       if(input$input_pred==FALSE){
        div( img(src=campeones_con_pred[campeones_con_pred$Season==input$temporadas,'escudos'],
              width = "150px", 
              height = "150px"), campeones_con_pred[campeones_con_pred$Season==input$temporadas,'Team'] )
       }else{
         div(  img(src=campeones_con_pred[campeones_con_pred$Season=='2019/20','escudos'],
                 width = "150px", 
                 height = "150px"), campeones_con_pred[campeones_con_pred$Season=='2019/20','Team'] ) 
       }
    })  
    


  
    output$escudos<- renderUI({
     
      img(src=data_escudos[data_escudos$Team==input$equipos,'escudos'],
       width = "150px", 
       height = "150px")
    })
    

    
    output$TablaCapital<- renderTable({
      
      if(input$input_pred==FALSE){
        players_shiny %>% 
                                    filter(Season==input$temporadas) %>% 
                                    select(Player, Team, Capital) %>% 
                                    arrange(desc(Capital)) %>% 
                                    head(10)
      }
      
    }, digits = 0, bordered = TRUE, align = 'c', hover = TRUE, width = 400)  
      
    output$goleador=renderText({
      if(input$input_pred==FALSE){
      a <- players_shiny %>% filter(Season==input$temporadas) %>% select(Player, Goals, Team) %>% filter(Goals==max(Goals))
      paste0(a[1,1],' - ',a[1,2], ' goles', ' (', a[1,3], ')') 
      } else{
        print('-')
      }
    })
    
    output$Ref_Capital=renderText({
      if(input$input_pred==FALSE){
        print('*Capital: Refiere al "peso" del jugador reflejando su rendimiento en la temporada en base a goles, asistencias, expulsiones, vallas invictas, etc.') 
      } else{
        print('-')
      }
    })
  
    
    output$Ref_TablaPred=renderText({
      if(input$input_pred==FALSE){
        print('W: Ganados  /  D: Empatados  /  L: Perdidos  /  F: Goles a favor  /  A: Goles en contra  /  Capital_total: Capital de la plantilla') 
      } else{
        print('-')
      }
    })
    
    output$Ref_TablaPlanilla=renderText({
      
        print('CS: Vallas invictas  /  Sent.Off: Expulsiones') 

    })
      
    output$goleadorxequipo=renderText({
      if(input$input_pred==FALSE){
        b <- players_shiny %>% filter((Season==input$output_temp)&(Team==input$equipos)) %>% select(Player, Goals) %>% filter(Goals==max(Goals))
        paste0(b[1,1],' - ',b[1,2], ' goles') 
      } else{
        print('-')
      }
    })
    
    
    
    output$TablaPred = renderTable({
      if(input$input_pred==TRUE){
        posiciones <- table19_20
        posiciones
      }else{
        posiciones <- table_tot %>%
                      filter(Season==input$temporadas) %>%
                      select('Team', 'Pos', 'W', 'D', 'L', 'F', 'A', 'Pts', 'Capital_total') %>% 
                      arrange(Pos) 
        posiciones
      }
    }, digits = 0, bordered = TRUE, align = 'c', hover = TRUE, width = 900)
    
    
    output$campeones<- renderPlotly({
        if(input$input_pred==TRUE){
                
                gr2 <-ggplot(data = campeones_con_pred, aes(x = Season, y = Pts)) + 
                    geom_text(aes(size = factor(Capital_total), color='red', label=Team)) +
                    ggtitle(label = 'Campeones por temporada')
                
                gr2 <- ggplotly({gr2})
                gr2 <- gr2 %>% layout(showlegend=FALSE)
                gr2
        }else{
            
            gr2 <-ggplot(data = champ, aes(x = Season, y = Pts)) + 
              geom_text(aes(size = factor(Capital_total), color='red', label=Team)) +
              ggtitle(label = 'Campeones por temporada')
            
            gr2 <- ggplotly({gr2})
            gr2 <- gr2 %>% layout(showlegend=FALSE)
            gr2
                
            }
        })
    
    
    output$rango <- renderPlotly({
      
      q <- ggplot(data = rango1, aes(x = Pos, y = Capital_total))
      q <- q + geom_point(aes(fill=factor(Rango_Posicion), size=15), shape=1)+
        scale_x_continuous(name = "Posición")+
        scale_y_continuous(name = "Capital")+
        ggtitle(label = 'Posición vs Capital del equipo')
      
      q <- ggplotly({q})
      q <- q %>% layout(showlegend=FALSE)
      q
      
    }) 
    
    
    output$potxeq <- renderPlotly({
            
      if(input$input_pred==TRUE){       
                bar1 <-ggplot(data = table19_20, aes(x = Team, y = Capital_total)) + 
                    geom_bar(show.legend = FALSE, stat = 'identity', aes(fill= factor(Posiciones))) + theme(axis.text.x=element_text(angle=65, hjust=1))+ xlab("Equipos") + ylab('Potencial') +
                    ggtitle(label = 'Potencial de la plantilla por equipo')
                
                bar1 <- ggplotly({bar1})
                bar1 <- bar1 %>% layout(showlegend=FALSE)
                bar1
            
           }else{
                 
                 bar1 <-ggplot(data = ds2(), aes(x = Team, y = Capital_total)) + 
                     geom_bar(show.legend = FALSE, stat = 'identity', aes(fill= factor(Pos))) + theme(axis.text.x=element_text(angle=65, hjust=1))+ xlab("Equipos") + ylab('Potencial') +
                     ggtitle(label = 'Potencial de la plantilla por equipo')
                 
                 bar1 <- ggplotly({bar1})
                 bar1 <- bar1 %>% layout(showlegend=FALSE)
                 bar1
                 
             }  
            
    })
    
    
    output$TablaPlanilla<- renderTable({
      players_shiny %>% filter((Season==input$output_temp)&(Team==input$equipos)) %>% select(c(5:13))
      
    
}, digits = 0, bordered = TRUE, align = 'c', hover = TRUE, width = 900)
    
    
    ds <- reactive({
       table_tot[table_tot$Team==input$equipos,]
        
    })
    
    ds2 <- reactive({
       table_tot[table_tot$Season==input$temporadas,]
        
    })
  

    
    output$grafico3 <- renderPlotly({
        
        bar2 <-ggplot(data = df.filt1(), aes(x = Player, y = Capital))  
        bar2 <- bar2 + geom_bar(show.legend = FALSE, stat = 'identity', aes(fill= factor(Goals))) + theme(axis.text.x=element_text(angle=65, hjust=1))+ xlab("Plantel") + ylab('Capital') +
            ggtitle(label = 'Injerencia por jugador') 
        
        bar2 <- ggplotly({bar2})
        bar2 <- bar2 %>% layout(showlegend=FALSE)
        bar2

    })  
    
    output$grafico4 <- renderPlotly({
        
        gr <-ggplot(data = ds(), aes(x = Season, y = Pos)) + 
            geom_point(aes(size = factor(Pts), color='red')) +
             ggtitle(label = 'Desempeño por temporada')
        gr <- ggplotly({gr})
        gr <- gr %>% layout(showlegend=FALSE)
        gr
        
    })      
    

    
    }

# Run the application 
shinyApp(ui = ui, server = server)
