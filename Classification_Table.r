# install.packages("RColorBrewer")
library(shiny)
library(highcharter)
library(RColorBrewer)
library(gplots)
library(readr)


#Import CSV file as dataset

ui <- fluidPage(
  titlePanel('Table'),
  
  sidebarLayout(
    sidebarPanel(
      # We're creating a drop down list with the neighborhoods
      selectInput('Div', 'Div: ', dataset$Div)
    ),
    mainPanel(
      dataTableOutput("table"))
  )
)

server <- function(input, output) {
  
  rank=function(dataset,Div){
    #Coloquem o ano com aspas
    
    season=dataset[dataset$Div==Div,]
    equipa=unique(c(season$Home,season$Away))
    
    vitoria=array(0,dim=length(equipa)); empate=array(0,dim=length(equipa))
    derrota=array(0,dim=length(equipa));gm=array(0,dim=length(equipa))
    gs=array(0,dim=length(equipa)); dg=array(0,dim=length(equipa))
    jogos=array(0,dim=length(equipa)); pontos=array(0,dim=length(equipa))
    
    
    for(i in 1:length(equipa)){
      jogos[i]=nrow(season[season$Home==equipa[i],])+nrow(season[season$Away==equipa[i],])
    }
    
    k=1
    for(i in equipa){
      for(linha in 1:nrow(season)){
        if(season$Home[linha]==i&&season$FT.Result[linha]=="H"||season$Away[linha]==i&&season$FT.Result[linha]=="A") vitoria[k]=vitoria[k]+1
        else if(season$Home[linha]==i&&season$FT.Result[linha]=="A"||season$Away[linha]==i&&season$FT.Result[linha]=="H") derrota[k]=derrota[k]+1
      }
      k=k+1
    }
    
    for(i in 1:length(equipa)){
      for(linha in 1:nrow(season)){
        if(season$Home[linha]==equipa[i]&&season$FT.Result[linha]=="D") empate[i]=empate[i]+1
        else if(season$Away[linha]==equipa[i]&&season$FT.Result[linha]=="D") empate[i]=empate[i]+1
      }}
    
    for(i in 1:length(equipa)){
      gm[i]=sum(season[season$Home==equipa[i],colnames(season)=="FT.Home.Goal"])+
        sum(season[season$Away==equipa[i],colnames(season)=="FT.Away.Goal"])
    }
    
    for(i in 1:length(equipa)){
      gs[i]=sum(season[season$Home==equipa[i],colnames(season)=="FT.Away.Goal"])+
        sum(season[season$Away==equipa[i],colnames(season)=="FT.Home.Goal"])
    }
    
    for(i in 1:length(equipa)) dg[i]=gm[i]-gs[i]
    
    for(i in 1:length(vitoria)) pontos[i]=vitoria[i]*3+empate[i]
    
    score=as.data.frame(cbind(equipa,pontos,jogos,vitoria,empate,derrota,gm,gs,dg))
    
    for(i in 2:length(score)) score[,i]=as.numeric(as.character(score[,i])); score=score[order(-score$pontos),]
    names(score)=c("Team","P","G","V","D","L","GS","GT","GD")
    rownames(score)=c()
    View(score)
    return(score)
  }
  
  output$table=renderDataTable(
    rank(dataset,input$Div))
}

shinyApp(ui, server)