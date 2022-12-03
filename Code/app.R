library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(tidyverse)
library(stringr)
library(word2vec)
library(shinydashboard)
library(shinyWidgets)

sushi_df<- read.csv("Data/sushi.csv", header=T)

sushi<- sushi_df %>% select(1:10)
setwd(paste0(getwd(),"/Data"))
sushi_reviews <- list.files(path = getwd(), pattern = "sushi_review") %>% 
  lapply(read_csv) %>% bind_rows 

ui<- fluidPage(
  useShinydashboard(),
  titlePanel("Sushi Restaurant "),
  navbarPage("Navigation",
             tabPanel("tab 1", fluid = TRUE,
                        sidebarPanel(width=4,
                          titlePanel("Restaurant Stats"),
                          selectizeInput("state", "Select State", choices = sort(unique(sushi$state))), 
                          selectizeInput("city", "Select a City", choices = NULL),
                          selectizeInput("name", "Select a Restaurant", choices = NULL)
                          ),
                        mainPanel( 
                          valueBoxOutput("starbox", width=5),
                          valueBoxOutput("reviewbox", width =5),
                          plotOutput("hist_all"),
                          plotOutput("hist_rest")
                          # more basics about restaurant performance - compared to average sushi?
                          ),
                      ), #end tab 1
             tabPanel("tab2", fluid=TRUE,
                      sidebarPanel(
                        titlePanel("Title"),
                        selectInput("choice", "Select what element you'd like to analyze", choices = c("service","rolls","fresh"))
                                  ),
                      mainPanel(
                        verbatimTextOutput("wordvecfun")
                        #sushi roll output
                               )
                      ), # end tab 2
             
             tabPanel("tab3", fluid=TRUE,
                      sidebarPanel(titlePanel("Title"),
                                   selectInput("", "", choices = NULL)
                                   ),
                      mainPanel(
                        #sushi roll output
                      )
             ) # end tab 2
             ) # end navbarPage
) # end UI

# function
wordvec=function(selected_business,keywords=c("service","rolls","fresh")){
  # read reviews for this particular business
  review<- sushi_reviews %>% filter(business_id == selected_business)
  # average stars and the number of reviews
  avg_star=mean(review$stars)
  num_reviews=nrow(review)
  # histogram for stars of this business
  hist=review %>% 
    ggplot(aes(stars)) +
    geom_histogram(fill="skyblue",binwidth=0.5)
  bad_review=review %>% filter(stars<3)
  good_review=review %>% filter(stars>3)
  model_good=word2vec(x=good_review$V10,type="skip-gram",dim=15,window=5,iter=20)
  model_bad=word2vec(x=bad_review$V10,type="skip-gram",dim=15,window=5,iter=20)
  # we can change the keywords.
  nn_good=predict(model_good,keywords,type="nearest",top_n=10)
  nn_bad=predict(model_bad,keywords,type="nearest",top_n=10)
  return(c(avg_star,num_reviews,hist,nn_good,nn_bad))
}



server<- function(input, output, session) {
  observeEvent(input$state, {
    updateSelectizeInput(session, "city", "Select a City", server = TRUE, choices = sort(unique(sushi$city[sushi$state == input$state])))
    }, priority = 2)

  observeEvent(c(input$state, input$city), {
   updateSelectizeInput(session, "name", "Select a Restaurant", server = TRUE, choices = sort(unique(sushi$name[which( sushi$state == input$state & sushi$city == input$city)])))
  }, priority = 1)
  
  business<- reactive({
    sushi$business_id[which(sushi$name == input$name & sushi$city== input$city)]
  })
  output$wordvecfun<- renderText({wordvec(business(), input$choice)})
  
  
  stars<- reactive({
    sushi$stars[which(sushi$name == input$name & sushi$city== input$city)]
  })
  output$starbox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox("Stars", stars(), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill=TRUE
    )
  })
  
  reviews<- reactive({
    sushi$review_count[which(sushi$name == input$name & sushi$city== input$city)]
  })
  output$reviewbox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox("# of Reviews", reviews(), icon = icon("list"),
                            color = "blue", fill=TRUE)
  })
  
  output$hist_all <- renderPlot({
    ggplot(sushi_reviews,aes(stars))+
      geom_histogram(bins=10, color = "black",fill = "#9FBEF0")+
      xlab("Star Rating")+
      ylab("Count")+
      ggtitle("Review Ratings for All Sushi Restaurants")
  })
  
  hist_chosen<- reactive({
    sushi_reviews %>% filter(business_id == sushi$business_id[which(sushi$name == input$name & sushi$city== input$city)])
  })
  output$hist_rest <- renderPlot({
    ggplot(hist_chosen(), aes(stars))+
      geom_histogram(bins=10, color = "black",fill = "#00BE67")+
      xlab("Star Rating")+
      ylab("Count")+
      ggtitle("Review Ratings for Chosen Restaurant")
  })
}

  


shinyApp(ui, server)
