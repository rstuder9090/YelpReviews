library(shiny)
library(tidyverse) #includes many relevant packages
library(word2vec)
library(tidytext)
library(tm)
library(syuzhet)
library(shinydashboard)
library(shinyWidgets)


sushi<- read_csv("app_data/app_business_data.csv"); sushi <- rename(sushi, index = `...1`)

sushi_reviews_stars <- read_csv("app_data/stars_all_reviews.csv"); sushi_reviews_stars <- rename(sushi_reviews_stars, index = `...1`)

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
                            condition =  "!is.null(hist_chosen())",      #This code panel not working as intended consider removing 
                            plotOutput("hist_rest"))
                          # more basics about restaurant performance - compared to average sushi?
                          ),
                      ), #end tab 1
              tabPanel("Topic Analysis", fluid=TRUE,
                      sidebarPanel(titlePanel("Title"),
                                   selectInput("", "", choices = NULL)
                      ),
                      mainPanel(
                        #Topics data
                               )
                      ), # end tab 2
             
             tabPanel("Word2Vec", fluid=TRUE,
                      sidebarPanel(
                        titlePanel("Title"),
                        selectInput("choice", "Select what element you'd like to analyze", choices = c("service","rolls","fresh"))
                      ),
                      mainPanel(
                        #not sure what output is of wordvec()
                        # verbatimTextOutput("wordvecfun")
                        #sushi roll output
                      )
             ) # end tab 3
             ) # end navbarPage
) # end UI

condition <- function(x){ x == " " | x == ""}  #Define condition expression to fix bugs regarding input$name == null


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
      sushi$business_id[which(sushi$name == input$name & sushi$city == input$city)]
    }
  })
  

  
  #output$wordvecfun<- renderText({wordvec(business(), input$choice)})
  
  stars <- reactive({
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
    if(!condition(input$name)){
      sushi$review_count[which(sushi$name == input$name & sushi$city== input$city)]
    }})
  
  output$reviewbox <- shinydashboard::renderInfoBox({
    if(!is.null(input$name)){
      shinydashboard::infoBox("# of Reviews", reviews(), icon = icon("list"),
                              color = "blue", fill=TRUE)
    }})
  
  output$hist_all <- renderPlot({
    ggplot(sushi_reviews_stars,aes(stars))+
      geom_histogram(bins=10, color = "black",fill = "#9FBEF0")+
      xlab("Star Rating")+
      ylab("Count")+
      ggtitle("Review Ratings for All Sushi Restaurants")
  })
  
  hist_chosen<- reactive({
    if(!condition(input$name)){
      sushi_reviews_stars %>% filter(business_id == sushi$business_id[which(sushi$name == input$name & sushi$city== input$city)])
    }
  })
  output$hist_rest <- renderPlot({
    if(!is.null(hist_chosen())){     #here if selection made
      ggplot(hist_chosen(), aes(stars))+
        geom_histogram(bins=10, color = "black",fill = "#00BE67")+
        xlab("Star Rating")+
        ylab("Count")+
        ggtitle("Review Ratings for Chosen Restaurant")
    }else{
      #here if selection not made
      ggplot(sushi_reviews_stars, aes(stars))+
        geom_blank()+
        xlab("Star Rating")+
        ylab("Count")+
        ggtitle("Review Ratings for Chosen Restaurant")
    }
    
  })
}

shinyApp(ui, server)
