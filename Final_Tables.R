# install.packages("RColorBrewer")
# install.packages("shinydashboard")
library(RColorBrewer)
library(shiny)
library(gplots)
library(readr)
library(shinydashboard)

#Import CSV file as dataset

#R.ui

ui <- dashboardPage(
  dashboardHeader(title="Premier League 2015-2019",titleWidth = 450),
  dashboardSidebar(disable=T),
  dashboardBody(
    fluidRow(
      tabBox( height ="250px",width=75,
              tabPanel("Championship Tables",selectInput('Div', 'Div: ', dataset$Div),dataTableOutput("table")),
              tabPanel("Discipline Heat Map",fluidRow(
                column(4,
                       selectInput("div",
                                   "Year:",
                                   c("All",
                                     unique(as.character(dataset$Div))))
                ),
                column(4,
                       selectInput("cards",
                                   "Cards:",
                                   c("Yellow", "Red"))
                ),
                column(4,
                       selectInput("place",
                                   "Place:",
                                   c("Home", "Away"))
                )),plotOutput("grafico",width = "100%", height = "700px")
              )
      ))))

#R.server

server <- function(input, output) {
  
  # Heatmap
  
  getHeatData = reactive({
    if (input$div != "All") {
      dataset <- dataset[dataset$Div == input$div,]
    }
    
    if (input$cards == "Red") {
      if (input$place == "Home") {
        heatdata <- tapply(dataset$`Home.Red.Card`, list(Team=dataset$Home, Referee=dataset$Referee), sum)
        
      } else if (input$place == "Away"){
        heatdata <- tapply(dataset$`Away.Red.Card`, list(Team=dataset$Away, Referee=dataset$Referee), sum)
      }
    } else if (input$cards == "Yellow") {
      if (input$place == "Home") {
        heatdata <- tapply(dataset$`Home.Yellow.Card`, list(Team=dataset$Home, Referee=dataset$Referee), sum)
      } else if (input$place == "Away"){
        heatdata <- tapply(dataset$`Away.Yellow.Card`, list(Team=dataset$Away, Referee=dataset$Referee), sum)
      }
    }
    
    heatdata[is.na(heatdata)] <- 0
    
    return(heatdata)
  })
  
  coul = colorRampPalette(brewer.pal(8, "YlOrRd"))(25)
  
  # Filter data based on selections
  
  output$grafico <- renderPlot(heatmap.2(getHeatData(),Colv=NA,Rowv=NA,col=coul,density.info="none",margins=c(6, 6),keysize = 0.7,trace="none",cellnote=getHeatData(),notecol="black"))
  
  #Table
  
  rank=function(dataset,Div){
    
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
    return(score)
  }
  
  output$table=renderDataTable(rank(dataset,input$Div))
}

shinyApp(ui, server)

