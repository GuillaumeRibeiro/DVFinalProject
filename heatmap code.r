# install.packages("RColorBrewer")
library(RColorBrewer)
library(shiny)
library(gplots)
library(readr)

#Import CSV file as dataset

ui <- fluidPage(
  titlePanel("Discipline Heat Map"),

  # Create a new Row in the UI for filters (inputs)

  fluidRow(
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
    )
  ),

  # Create a new row in the UI for the Heat Map

  plotOutput("grafico",width = "100%", height = "700px")
)


server <- function(input, output) {
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

}


shinyApp(ui = ui, server = server)