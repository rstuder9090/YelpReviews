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
sushi$city[which(sushi$city == "Tuscon" & sushi$state == "FL" )] <- "Tuscon"
sushi$city[which(sushi$city == "Clearwater Beach" & sushi$state == "FL" )] <- "Clearwater"
sushi$city[which(sushi$city == "Land o lakes" & sushi$state == "FL" )] <- "Land O Lakes"
sushi$city[which((sushi$city == "St Petersburg" | sushi$city == "St. Petersburg") & sushi$state=="FL" )] <- "Saint Petersburg"
sushi$city[which((sushi$city == "South Tampa" | sushi$city == "Tampa Bay")& sushi$state=="FL" )] <- "Tampa"
sushi$city[which((sushi$city == "St Louis" | sushi$city == "St. Louis")& sushi$state=="MO" )] <- "Saint Louis"
sushi$city[which(sushi$city == "O Fallon" & sushi$state == "IL" )] <- "O'Fallon"
sushi$city[which(sushi$city == "St Charles" & sushi$state == "MO" )] <- "St. Charles"
sushi$city[which(sushi$city == "Deptford Township" & sushi$state == "NJ" )] <- "Deptford"
sushi$city[which(sushi$city == "Ewing" & sushi$state == "NJ" )] <- "Ewing Township"
sushi$city[which(sushi$city == "Mount Laurel" & sushi$state == "NJ" )] <- "Mount Laurel Township"
sushi$city[which(sushi$city == "Pennsauken" & sushi$state == "NJ" )] <- "Pennsauken Township"
sushi$city[which(sushi$city == "Voorhees" & sushi$state == "NJ" )] <- "Voorhees Township"
sushi$city[which(sushi$city == "MEDIA" & sushi$state == "PA" )] <- "Media"
sushi$city[which(sushi$city == "Warrington Township" & sushi$state == "PA" )] <- "Warrington"
sushi$city[which((sushi$city == "Mt Juliet Louis" | sushi$city == "Mt. Juliet")& sushi$state=="TN" )] <- "Mount Juliet"

sushi<- sushi_df %>% select(1:10)
setwd(paste0(getwd(),"/Data"))
sushi_reviews <- list.files(path = getwd(), pattern = "sushi_review") %>% 
  lapply(read_csv) %>% bind_rows 

ui<- fluidPage(
  useShinydashboard(),
  titlePanel("Sushi Restaurant "),
  navbarPage("Navigation",
             tabPanel("tab 1", fluid = TRUE,
                        sidebarPanel(width=3,
                          titlePanel("Restaurant Stats"),
                          selectizeInput("state", "Select State", choices = c(" ", sort(unique(sushi$state))), selected=NULL), 
                          selectizeInput("city", "Select a City", choices = NULL, selected=NULL),
                          selectizeInput("name", "Select a Restaurant", choices = NULL, selected=NULL)),
                        mainPanel( 
                          valueBoxOutput("starbox", width=5),
                          valueBoxOutput("reviewbox", width =5),
                          plotOutput("hist_all"),
                          conditionalPanel(
                            condition =  "!is.null(hist_chosen())",
                            plotOutput("hist_rest"))
                          # more basics about restaurant performance - compared to average sushi?
                          ),
                      ), #end tab 1
             tabPanel("tab2", fluid=TRUE,
                      sidebarPanel(
                        titlePanel("Title"),
                        selectInput("choice", "Select what element you'd like to analyze", choices = c("service","rolls","fresh"))
                                  ),
                      mainPanel(
                        #not sure what output is of wordvec()
                        # verbatimTextOutput("wordvecfun")
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
    
    updateSelectizeInput(session, "city", "Select a City", server = TRUE, choices = c(" ", sort(unique(sushi$city[sushi$state == input$state]))))
    }, priority = 1)

  observeEvent(c(input$state, input$city), {
   updateSelectizeInput(session, "name", "Select a Restaurant", server = TRUE, choices = c(" ",sort(unique(sushi$name[which( sushi$state == input$state & sushi$city == input$city)]))))
  }, priority = 2)
  

  business<- reactive({
    if(!is.null(input$name)){
      sushi$business_id[which(sushi$name == input$name & sushi$city== input$city)]
    }
  })
  
  
  #output$wordvecfun<- renderText({wordvec(business(), input$choice)})
  
  
  stars<- reactive({
    if(!is.null(input$name)){
      sushi$stars[which(sushi$name == input$name & sushi$city== input$city)]
    }
  })
  
  output$starbox <- shinydashboard::renderInfoBox({
    if(!is.null(input$name)){
      shinydashboard::infoBox("Stars", stars(), icon = icon("thumbs-up", lib = "glyphicon"),
                              color = "yellow", fill=TRUE)
    }
  })
  
  reviews<- reactive({
    if(!is.null(input$name)){
      sushi$review_count[which(sushi$name == input$name & sushi$city== input$city)]
    }})
  
  output$reviewbox <- shinydashboard::renderInfoBox({
    if(!is.null(input$name)){
      shinydashboard::infoBox("# of Reviews", reviews(), icon = icon("list"),
                              color = "blue", fill=TRUE)
    }})
  
  output$hist_all <- renderPlot({
    ggplot(sushi_reviews,aes(stars))+
      geom_histogram(bins=10, color = "black",fill = "#9FBEF0")+
      xlab("Star Rating")+
      ylab("Count")+
      ggtitle("Review Ratings for All Sushi Restaurants")
  })
  
  hist_chosen<- reactive({
    if(!is.null(input$name)){
      sushi_reviews %>% filter(business_id == sushi$business_id[which(sushi$name == input$name & sushi$city== input$city)])
    }
  })
  
  output$hist_rest <- renderPlot({
    if(!is.null(hist_chosen())){
      ggplot(hist_chosen(), aes(stars))+
        geom_histogram(bins=10, color = "black",fill = "#00BE67")+
        xlab("Star Rating")+
        ylab("Count")+
        ggtitle("Review Ratings for Chosen Restaurant")
    }else{
      ggplot(sushi_reviews, aes(stars))+
        geom_blank()+
        xlab("Star Rating")+
        ylab("Count")+
        ggtitle("Review Ratings for Chosen Restaurant")
    }

  })
}

shinyApp(ui, server)
