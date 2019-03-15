library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(shinyBS)

#install.packages('rsconnect')

#install.packages("devtools")
#devtools::install_github("wmurphyrd/fiftystater")

library(plotly)
library(fiftystater)

df<-read.csv("StateDataforMap_2018-19week8.csv")
twitter_data<-read.csv("final_states_tweets.csv")
influ_data<-read.csv("influenza_tweets.csv")
flu_data<-read.csv("flu_tweets.csv")
fluseason<-read.csv("fluseason.csv")

ui <- bootstrapPage(
  titlePanel("Flu Data Comparision"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "hashtag", label = strong("Hashtag"),
                  choices = c('All','#influenza','#flu','#fluseason'),
                  selected = "All") , width=3 
    ),
    
    mainPanel(
      plotOutput(outputId = "twittermap"),textOutput(outputId = 'text'),plotlyOutput(outputId = "heatmap")
      
    )
  )
)

server <- function(input, output) {
  
  
 output$heatmap <- renderPlotly({
   df$ACTIVITY.LEVEL<-as.integer(df$ACTIVITY.LEVEL)
   df[df$ACTIVITY.LEVEL < 3 & df$ACTIVITY.LEVEL.LABEL == "High", "ACTIVITY.LEVEL"] <- 10
   df$hover <- with(df, paste(STATENAME, '<br>', "Activity Level:", ACTIVITY.LEVEL,"<br>", "Label:",ACTIVITY.LEVEL.LABEL))
   l <- list(color = toRGB("white"), width = 2)
   g <- list(
     scope = 'usa',
     lakecolor = toRGB("white")
   )
   
   plot_geo(df, locationmode = 'USA-states') %>%
     add_trace(
       z = ~ACTIVITY.LEVEL,text = ~hover, locations = ~STATEABB,
       color = ~ACTIVITY.LEVEL, colors=colorRamp(c("#00c200","#5bf700","#5bf700","#8cf700","#baf700","#e0f500","#f7df00","#fcb100","#fc8200","#fa4f00","#d00000"))
     ) %>%
     colorbar(title = "Activity Level") %>%
     layout(
       title = '2018-19 Influenza Season Week 8 ending Mar 01, 2019',
       geo = g
     )
})
 
 output$twittermap<-renderPlot({
   if(input$hashtag=='#flu'){
     dataset<-flu_data
     plt_title="No of Flu keyword tweets by different states in USA"
   }
   else if(input$hashtag=='#influenza'){
     dataset<-influ_data
     plt_title="No of influenza keyword tweets by different states in USA"
   }
   else if(input$hashtag=='All'){
     dataset<-twitter_data
     plt_title="No of tweets by different states in USA"
     
   }
   else if(input$hashtag=='#fluseason'){
     dataset<-fluseason
     plt_title="No of fluseason keyword tweets by different states in USA"
   }
   

   plot2 <- ggplot(dataset, aes(map_id = region))
   plot2 <- plot2 + geom_map( aes( fill = count ), map = fifty_states, color="grey24") +
     expand_limits(x = fifty_states$long, y = fifty_states$lat) +
     scale_fill_gradient("Tweets Count", low="green" , high="#cc0000") +
     ggtitle(plt_title) + 
     coord_fixed(ratio = 5/3) +
     theme(
       plot.title = element_text(size=20, hjust = 0.2, face="bold"),
       panel.background = element_blank(),
       axis.text = element_blank(),
       axis.ticks = element_blank(),
       axis.title.x = element_blank(),
       axis.title.y = element_blank())
   plot2
   
 })
 
 
}

shinyApp(ui = ui, server = server)
























