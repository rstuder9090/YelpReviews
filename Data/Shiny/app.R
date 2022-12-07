library(shiny)
library(tidyverse) #includes many relevant packages
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)

#Descriptive data  
sushi<- read_csv("app_business_data.csv"); sushi <- rename(sushi, index = `...1`)
sushi_reviews_stars <- read_csv("stars_all_reviews.csv"); sushi_reviews_stars <- rename(sushi_reviews_stars, index = `...1`)
#Topics Playground Data
good_review_grams <- read_csv('good_reviews_grams.csv'); good_review_grams <- rename(good_review_grams, index = `...1`)
bad_review_grams <- read_csv('bad_reviews_grams.csv'); bad_review_grams <- rename(bad_review_grams, index = `...1`)
tips_grams <- read_csv('tips_grams.csv'); tips_grams <- rename(tips_grams, index = `...1`)
quote_reviews <- read_csv('LDA/quotes/quote_reviews.csv')
tip_reviews <- read_csv('LDA/quotes/quote_tips.csv')

ui<- fluidPage(
  useShinydashboard(),
  titlePanel("Sushi Restaurant "),
  navbarPage("Navigation",
             tabPanel("Statistics", fluid = TRUE,
                        sidebarPanel(width=3,
                          titlePanel("Restaurant Stats"),
                          selectizeInput("state", "Select State", choices = c(" ", sort(unique(sushi$state))), selected=NULL), 
                          selectizeInput("city", "Select a City", choices = NULL, selected=NULL),
                          selectizeInput("name", "Select a Restaurant", choices = NULL, selected=NULL)),
                        mainPanel( 
                          tags$style(".recalculating { opacity: inherit !important; }"),
                          verticalLayout(
                            fluidRow(valueBoxOutput("starbox"),valueBoxOutput("reviewbox"),valueBoxOutput("sentbox")),
                           h3(htmlOutput("note")),
                          splitLayout(cellWidths = c("50%", "50%"), plotOutput("hist_all"), plotOutput("hist_rest")),
                          plotOutput("sent_plot")
                          # more basics about restaurant performance - compared to average sushi?
                          )),
                      ), #end tab 1
               tabPanel("Topic Playground", fluid=TRUE,
                      sidebarPanel(titlePanel(htmlOutput("name") ),
                                   selectInput("data", "Choose which insights to view", choices = c("Reviews","Tips"), selected = "Reviews"), 
                                   strong('Below are quotes from users that most embody the topics of your business:'),
                                   br(),
                                   p('Warning: Chosen restaurant must have over 10 good or 10 bad reviews and 20 tips otherwise table will not appear'),
                                   br(),
                                   htmlOutput("quote_header"),
                                   htmlOutput("review_quotes")
                      ),
                      mainPanel(conditionalPanel(condition = "input.data == 'Reviews'", 
                                                 verticalLayout(
                                                   strong("Popular Topics from N-grams --- Left: Good Reviews with stars > 3. ------------------ ------------------  Right: Bad Review with stars < 3"),
                                                   splitLayout(cellWidths = c("50%", "50%"), dataTableOutput("good_reviews_table"), dataTableOutput("bad_reviews_table")), br(),
                                                   strong("Unique Topics from LDA ---Left: Unique Topics in Good Reviews. ------------------------------------ Right: Unique Topics in Bad Reviews "),
                                                   splitLayout(cellWidths = c("50%", "50%"), dataTableOutput("good_review_topics_table"), dataTableOutput("bad_review_topics_table"))
                                                 ) #close vert layout - reviews
                                                 
                                                 ), #close conditional panel - reviews
          
                                conditionalPanel(condition = "input.data == 'Tips'", 
                                verticalLayout(
                                  strong("Popular Topics from N-grams on Tips Dataset"),
                                  dataTableOutput("tips_table"), br(),
                                  strong("Unique Topics from LDA on Tips Dataset"),
                                  dataTableOutput("tips_topics_table")
                                ) #close vert layout - tips
                                
                      ) #close conditional panel - tips
                      )#close main panel
                      ), # end tab 2
             
             tabPanel("Likes & Dislikes", fluid=TRUE,
                      sidebarPanel(
                        selectInput("choice", "Select what element you'd like to analyze", choices = c("Service","Roll"))
                      ),
                      mainPanel(
                        verticalLayout(
                        fluidRow(plotOutput("vecplot1")),
                        fluidRow(plotOutput("vecplot2"))
                        )
                      )
             ), #end tab3
             tabPanel("About", fluid = TRUE,
                      mainPanel(
                        h2("Project for UW-Madison STAT 628 Module 3"),
                        h4("Authors: Jack Bressett, Rachel Studer, Ran Zhao"),
                        br(),
                        h4("The goal of this project is to parse through Yelp Review data to help 
                           businesses find information on themselves and what topics patron's talk 
                           about in good reviews, bad reviews, and tips. Understanding topics and 
                           concepts between different levels of reviews will help sushi restaurants' 
                           strive for better ratings."),
                        br(),
                        em("For error reporting or questions please contact rlstuder@wisc.edu")
                      )
               
             ) # end tab 4
             )) # end UI


condition <- function(x){ x == " " | x == ""}  #Define condition expression to fix bugs regarding input$name == null




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
  
  
  stars <- reactive({
    if(!is.null(input$name)){
      sushi$stars[which(sushi$name == input$name & sushi$city== input$city)]
    }
  })
  
  output$starbox <- shinydashboard::renderValueBox({
    if(!is.null(input$name)){
      shinydashboard::valueBox(tags$p(stars(), style = "font-size: 20px;"),"Stars", 
                               icon = tags$i(icon("thumbs-up", lib = "glyphicon"), style="font-size: 30px; color: white"),
                              color = "yellow")
    }
  })
  
  reviews<- reactive({
    if(!condition(input$name)){
      sushi$review_count[which(sushi$name == input$name & sushi$city== input$city)]
    }})
  
  output$reviewbox <- shinydashboard::renderValueBox({
    if(!is.null(input$name)){
      shinydashboard::valueBox(tags$p(reviews(), style = "font-size: 20px;"),"# of Reviews", 
                               icon = tags$i(icon("list"), style="font-size: 30px; color: white"),
                              color = "blue")
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
  
  sentimentfile<- reactive({
    if(!condition(input$name)){
      paste0(business(), ".csv")
    }
  })
    

  s<- reactive({
    if(!is.null(sentimentfile())){
    read_csv(paste0("sentiment/", sentimentfile()))}
  })
  
  sentiment_score<- reactive({
    if(!is.null(s())){
    round(mean(s()$score),2) }
    })
  
  output$note<- renderText({
    paste0("Your restaurant has ",sentiment_score(), " more positive than negative words in the ", reviews(), " reviews provided." )
  })
  
  output$sentbox <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(tags$p(sentiment_score(), style = "font-size: 20px;"),"Sentiment Score", 
                               icon = tags$i(icon("cog", lib = "glyphicon"), style="font-size: 30px; color: white"),
                              color = "green")
    })
  
 output$sent_plot<- renderPlot({
   if(!is.null(s())){
     barplot(
       sort(colSums(prop.table(s()[, 1:8]))),
       horiz = TRUE,
       col='skyblue',
       cex.names = 0.7,
       las = 1,
       main = "Emotions in Reviews", xlab="Percentage") 
   }
 }) 
   ##############End stats page logic
  #helper functions for LDA discriminative  topics
  kl_div <- function(p1, p2) {
    p1 * log(p1 / p2) + (p2 - p2)
  }
  kl_mat <- function(p) {
    K <- matrix(0, nrow = length(p), ncol = length(p))
    
    for (i in seq_along(p)) {
      for (j in seq_len(i - 1)) {
        K[i, j] <- kl_div(p[i], p[j])
      }
    }
    K
  }
  discrepancy <- function(p, lambda = 1e-7) {
    p <- (p + lambda) / sum(p + lambda) # Laplace smoothing
    K <- kl_mat(p)
    max(K)
  }
  
  
  output$name <- renderText({
    aa <- paste0("Explore Popular and Unique Topics for ","<font color=\"#FF0000\"><b>", input$name, " in ", input$city, ", ", input$state, ".") 
    paste0(aa,"<font color=\"000000\"><b>","<br> Please Select: Yelp Reviews  or Yelp Tips.")
    })
  
  
  business_ID <- reactive({
      id <- sushi %>% 
        filter(name == input$name) %>% 
        select(business_id)
      return(id[[1]])
  })

#Good Review N-gram    
output$good_reviews_table <- renderDataTable({
  id <- business_ID()
  current_business_good <- good_review_grams %>% 
    filter(business_id==id) %>% 
    select(-c(index, business_id))
  
}, options = list(pageLength=10))

#Bad Review N-gram    
output$bad_reviews_table <- renderDataTable({
  id <- business_ID()
  current_business_bad <- bad_review_grams %>% 
    filter(business_id==id) %>% 
    select(-c(index, business_id))
  
},options = list(pageLength=10))  

#Tip N-gram
output$tips_table <- renderDataTable({
  id <- business_ID()
  current_business_tips <- tips_grams %>% 
    filter(business_id==id) %>% 
    select(-c(index, business_id))
  
}, options = list(pageLength=10))

#Good Review LDA 
output$good_review_topics_table <- renderDataTable({
  id <- business_ID()
  prepath <- 'LDA/review-LDA/'
  path <- paste0(prepath,id,"good-.rda")
  loaded_good <- load(path)
  
  discriminative_terms <- topics %>%
    group_by(term) %>%
    mutate(D = discrepancy(beta)) %>%
    ungroup() %>%
    slice_max(D, n = 25) %>%
    mutate(term = fct_reorder(term, -D))
  
  
  discriminative_terms <- discriminative_terms %>%
    select(-D) %>%
    pivot_wider(names_from = "topic", values_from = "beta") 
  
  discriminative_terms %>% 
    mutate(rank = 1:nrow(discriminative_terms)) %>% 
    select(term,rank)
  
})

#Bad Review LDA 
output$bad_review_topics_table <- renderDataTable({
  id <- business_ID()
  prepath <- 'LDA/review-LDA/'
  path <- paste0(prepath,id,"bad-.rda")
  loaded_good <- load(path)
  
  discriminative_terms <- topics %>%
    group_by(term) %>%
    mutate(D = discrepancy(beta)) %>%
    ungroup() %>%
    slice_max(D, n = 25) %>%
    mutate(term = fct_reorder(term, -D))
  
  
  discriminative_terms <- discriminative_terms %>%
    select(-D) %>%
    pivot_wider(names_from = "topic", values_from = "beta") 
  
  discriminative_terms %>% 
    mutate(rank = 1:nrow(discriminative_terms)) %>% 
    select(term,rank)
})  

#Tips LDA 
output$tips_topics_table <- renderDataTable({
  id <- business_ID()
  prepath <- 'LDA/tips-LDA/'
  path <- paste0(prepath,id,".rda")
  loaded_good <- load(path)
  
  discriminative_terms <- topics %>%
    group_by(term) %>%
    mutate(D = discrepancy(beta)) %>%
    ungroup() %>%
    slice_max(D, n = 25) %>%
    mutate(term = fct_reorder(term, -D))
  
  
  discriminative_terms <- discriminative_terms %>%
    select(-D) %>%
    pivot_wider(names_from = "topic", values_from = "beta") 
  
  discriminative_terms %>% 
    mutate(rank = 1:nrow(discriminative_terms)) %>% 
    select(term,rank)
})  



#Header for quotes
output$quote_header <- renderText({
  if(input$data == 'Reviews'){
  return('Two Good Reviews and Two Bad Reviews: <br><br>')}
else{return('Two Important Tips: <br><br>')}
})

#Conditional quote generator from LDA analysis
output$review_quotes <- renderText({
  id <- business_ID()
  if(input$data == 'Reviews'){
    text_tibble <- quote_reviews %>% 
      filter(business_id == id) %>%  
      select(text)
  }
  else{
    text_tibble <- tip_reviews %>% 
      filter(business_id == id) %>%  
      select(text)
  }
  
  quotes <- ''
  for(i in text_tibble[,1]){
    temp <- paste0(i, '<br><br>')
    quotes <- paste0(quotes,temp)
  }
quotes
})


##############End topics page logic
 vecfile<- reactive({
   paste0("word2vec_", input$choice, ".csv")
 })
 
 pos<-reactive({
   if(!is.null(vecfile())){
     read_csv(vecfile()) %>% filter(direction=='positive')}
 })
 
 output$vecplot1<- renderPlot({
     ggplot(pos(), aes(x=reorder(keywords,similarity),y=similarity)) + 
     geom_col(fill='#00A087B2')+
     labs(x="Keywords",y='Conditional Probability',title=paste0("Top 10 Keywords Related to Positive"," ",input$choice, " ", "Reviews"))+
     theme(plot.title = element_text(size=22), axis.title=element_text(size=16), 
           axis.text.y=element_text(size=12), 
           axis.text.x = element_text(size=12, angle = 45, hjust=1))
 })
 
 neg<-reactive({
   if(!is.null(vecfile())){
     read_csv(vecfile()) %>% filter(direction=='negative')}
 })
 
 output$vecplot2<- renderPlot({
   ggplot(neg(), aes(x=reorder(keywords,similarity),y=similarity)) + 
     geom_col(fill="#DC0000B2")+
     labs(x="Keywords",y='Conditional Probability',title=paste0("Top 10 Keywords Related to Negative"," ", input$choice," ", "Reviews"))+
     theme(plot.title = element_text(size=22), axis.title=element_text(size=16), 
           axis.text.y=element_text(size=12), 
           axis.text.x = element_text(size=12, angle = 45, hjust=1))
 })
 
 
} # end of server

shinyApp(ui, server)
