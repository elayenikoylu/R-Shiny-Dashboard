if (interactive()) {
  
  
  #- MAIN UI START
  ui <- dashboardPagePlus(
    title = "Valentine's Day Survey",
    skin = "red",
    
    #- Dashboard Title
    dashboardHeader(title = span(tagList(icon("heart"), "Valentine's Day Survey")), titleWidth = 300),
    
    #- Left Menu
    dashboardSidebar(
      sidebarMenu(
        menuItem("Sentimental WordCloud", tabName = "cloud_lo", icon = icon("cloud")),
        menuItem("Frequency Histogram", tabName = "freq_lo", icon = icon("search")),
        menuItem("Sentiment Analysis", tabName = "senti_lo", icon = icon("smile")),
        menuItem("TF-IDF Histogram", tabName = "tfidf_lo", icon = icon("home")),
        menuItem("N-gram and Neural Network", tabName = "ngram_lo", icon = icon("user-friends")),
        menuItem("Naive Bayes Analysis", tabName = "nba_lo", icon = icon("user-friends"))
        
      )
    ),
    
    #- Dashboard Body
    dashboardBody(
      
      tabItems(
        # First tab content
        tabItem(tabName = "nba_lo",
                fluidRow(
                  column(width = 4,
                         h2("Naive-Bayes Simulation"),
                         textInput('llife', "1. Describe your love life?", value = "", width = NULL, placeholder = ''),
                         textInput('favday', "2. What has been your favorite valentines day yet?", value = "", width = NULL, placeholder = ''),
                         textInput('perf', "3. Describe your perfect date.", value = "", width = NULL, placeholder = ''),
                         textInput('gift', "4. What is an ideal gift for you?", value = "", width = NULL, placeholder = ''),
                         textInput('worst', "5. Describe your worst Valentines Day.", value = "", width = NULL, placeholder = ''),
                         
                         actionButton("goButton", "Go!")),
                         
                  column(h3(textOutput("userText")),width = 4, offset= 2
                      )
                )),
        tabItem(tabName = "cloud_lo",
                fluidRow(
                  box(
                    selectInput('ques', 'Select The Question', c("All Questions", "1. Describe your love life?","2. What has been your favorite valentines day yet?","3. Describe your perfect date.","4. What is an ideal gift for you?","5. Describe your worst Valentines Day."))
                    ,width=3),
                  box(
                    plotOutput("senticloudPlot"),width=9,offset=9    
                )  
                )  
        ),
        tabItem(tabName = "freq_lo",
                fluidRow(
                  box(
                    sliderInput('ques_no', 'Select Question Number : ', min=1,max=5,value=1,step=1) 
                    ,width=3),
                  box(
                    plotly::plotlyOutput("freqPlot"),width=8
                  )
                )  
        ),
        
        # Second tab content
        tabItem(tabName = "senti_lo",
                fluidRow(
                  box(
                    selectInput('ques_sa', 'Choose The Question', c("Question 1","Question 2","Question 3","Question 4","Question 5"),selected="Question 3")
                    ,width=3),
                  box(
                    plotly::plotlyOutput("sentiPlot"),width=9
                    )
                )  
            ),
        tabItem(tabName = "tfidf_lo",
                fluidRow(
                  box(
                    sliderInput('ques_no2', 'Select Question Number : ', min=1,max=5,value=1,step=1) 
                    ,width=3),
                  box(
                      plotly::plotlyOutput("tfidfPlot"),width=8
                  )
                )  
        
    ),
    tabItem(tabName = "ngram_lo",
            fluidRow(
                column(
                      h2("Bigram Graph"),
                      plotOutput('bigramPlot'),width=4),
                column(
                      h2("Quadrograms"),
                      tableOutput("quad_Table"), style = "font-size: 80%; width: 80%",width=4))
                    )   
    )
)
)




server <- function(input, output, session){
  # Function to create a data frame of top 10 names by sex and year 

  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(DT)
  library(shinythemes)
  library(dplyr)
  library(textreadr)
  library(textdata)
  library(tidyr)
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(wordcloud)
  library(igraph)
  library(ggraph)
  library(pdftools)
  library(plotly)
  library(ggplot2)
  library(tm)
  library(quanteda)
  library(RColorBrewer)
  library(quanteda.textmodels)
  library(widyr)
  library(reshape2)
  
  cap3 <- eventReactive(input$goButton, {
    my_naive_fn()
  })
  
  

  output$userText <- renderText({
    cap3() 
  })
    
  my_naive_fn<- function(){
    ######## Naive Bayes prediction Model ##########
    ################################################
    input_string <- paste(input$llife , input$favday , input$perf , input$gift , input$worst , sep=" ")
    print(input_string)  
    my_df$AllQues <-
      paste(my_df$Question1, my_df$Question2, my_df$Question3, 
            my_df$Question4, my_df$Question5, sep = " ")
    
    
    de<-data.frame(input$llife , input$favday , input$perf , input$gift , input$worst,"yesorno",input_string)
    names(de)<-c("Question1", "Question2", "Question3", 
                 "Question4", "Question5", "Question6","AllQues")
    
    newdf <- rbind(my_df, de)
    
    corp <- corpus(newdf, text_field = c("AllQues"))
    
    msg.dfm <- dfm(corp, tolower = TRUE) #generating document 
    msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
    msg.dfm <- dfm_weight(msg.dfm)
    
    msg.dfm<-dfm_select(msg.dfm, pattern = c("broke","ready","single","mingle","life","married","boring","coding","engaged","hard","messy","relationship","beach","night","cafe","cancun","enjoying","morning","eating","feel","walk","wine","bad","fight","remember","huge","memory","roses","school","2007","grade","outfit","pay","ago","girlfriend","picnic","lot","wife","cooked","dad","hotel","park","surprise","vacation","visit","weekend","visited","chocalate","experience","jewelry","money","related","sanitizer","personal","buy","chocalates","diamond","diamonds","expensive","monetary","rings","woman"), selection = "keep", valuetype = "fixed")
    
    q6 %>% count( text == 'yes' | text == 'Yes' |  text == 'Yes.' | text == 'Yeah.' )
    q6 %>% count( text == 'No' | text == 'no' | text == 'No.' )
    
    ## 35 Yes  .....   22 No .....
    35/57
    0.9*57
    8 * (35/57)
    
    msg.dfm
    #Splitting the docs into training and testing data using 90/10
    
    # Splitting the data with equal no of yes's & no's - STRATIFIED split
    
    msg.dfm.train<-msg.dfm[-c(1,5,15,23,28,33,38,43),] 
   # msg.dfm.test<-msg.dfm[c(1,5,15,23,28,33,38,43),] 
    
    msg.dfm.test<-msg.dfm[c(58),] 
    
    #building the Naive Bayes model:
    NB_classifier <- textmodel_nb(msg.dfm.train,
                                  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                    1, 1, 1, 1, 1, 1, 1, 1, 1 )) # We are imputiing this success values manually
    NB_classifier
    summary(NB_classifier)
    
    odds<- 0
    # predicting the testing data
    pred <- predict(NB_classifier, msg.dfm.test,type='prob')
    
    outp <- paste("Thank You !! The likelihood of you celebrating Valentines Day with someone this year is",round((pred[1,'1'] * 100),2),'% !!!')
    
    outp
    

  }
  
  
  output$senticloudPlot <- renderPlot({
    user_ques_selection <- input$ques
    if (user_ques_selection == 'All Questions'){
        tidy_var<- tidy_survey
    } else if ( user_ques_selection == "1. Describe your love life?") {
      tidy_var<- tidy_q1
    } else if ( user_ques_selection == "2. What has been your favorite valentines day yet?") {
      tidy_var<- tidy_q2
    } else if ( user_ques_selection == "3. Describe your perfect date.") {
      tidy_var<- tidy_q3
    } else if ( user_ques_selection == "4. What is an ideal gift for you?") {
      tidy_var<- tidy_q4
    } else if ( user_ques_selection == "5. Describe your worst Valentines Day.") {
      tidy_var<- tidy_q5
    }   
    
    tidy_var %>%
      inner_join(get_sentiments("nrc")) %>%
      count(word, sentiment, sort=TRUE) %>%
      acast(word ~sentiment, value.var="n", fill=0) %>%
      comparison.cloud(color = c("red", "blue"),
                       max.words=200,scale=c(1,1))  
    },height = 530, width = 760)
  
  output$freqPlot <- plotly::renderPlotly({
    colorch2 <- "coral2"
    user_ques_selection2 <- input$ques_no
    if (user_ques_selection2 == 1){
      tidy_var2<- tidy_q1
      colorch2 <- "coral2"
      questio<- "1. Describe your love life?"
    } else if ( user_ques_selection2 == 2) {
      tidy_var2<- tidy_q2
      colorch2 <- "turquoise4"
      questio<- "2. What has been your favorite valentines day yet?"
    } else if ( user_ques_selection2 == 3) {
      tidy_var2<- tidy_q3
      colorch2 <- "violetred4"
      questio<- "3. Describe your perfect date."
    } else if ( user_ques_selection2 == 4) {
      tidy_var2<- tidy_q4
      colorch2 <- "palegreen3"
      questio<- "4. What is an ideal gift for you?"
    } else if ( user_ques_selection2 == 5) {
      tidy_var2<- tidy_q5
      colorch2 <- "burlywood3"
      questio<-"5. Describe your worst Valentines Day."
    }   
    
    freq_hist <- tidy_var2 %>%
      count(word, sort=TRUE) %>%
      mutate(word=reorder(word, n)) %>%
      top_n(10) %>%
      ggplot(aes(word, n))+
      geom_col(fill=colorch2)+
      labs(x = "Words",
           y = "Frequency",
           title = questio) +
           coord_flip()
    print(freq_hist)
    
  })
  
  
  
  output$sentiPlot <- plotly::renderPlotly({
    user_ques_selectionsa <- input$ques_sa
    if (user_ques_selectionsa == "Question 1"){
      tidy_var_sa<- tidy_q1
      questio2<- "1. Describe your love life?"
      
        #    colorch2 <- "coral2"
    } else if ( user_ques_selectionsa == "Question 2") {
      tidy_var_sa<- tidy_q2
  #    colorch2 <- "turquoise4"
      questio2<- "2. What has been your favorite valentines day yet?"
      
      } else if ( user_ques_selectionsa == "Question 3") {
      tidy_var_sa<- tidy_q3
      questio2<- "3. Describe your perfect date."
      
  #    colorch2 <- "violetred4"
    } else if ( user_ques_selectionsa == "Question 4") {
      tidy_var_sa<- tidy_q4
      questio2<- "4. What is an ideal gift for you?"
      
      #    colorch2 <- "palegreen3"
    } else if ( user_ques_selectionsa == "Question 5") {
      tidy_var_sa<- tidy_q5
  #    colorch2 <- "burlywood3"
      questio2<-"5. Describe your worst Valentines Day."
      
    }   
      bing_survey <- tidy_var_sa %>%
      count(word, sort=TRUE)
    
    bing_survey <- bing_survey %>% 
      inner_join(get_sentiments("bing")) %>% 
      ungroup()
    
    sentiment_count <- bing_survey %>% 
      count(sentiment, sort = TRUE)
    
    bing_survey %>% 
      group_by(sentiment) %>%
      ungroup() %>% 
      mutate(word = reorder(word, n)) %>% 
      head(20) %>% 
      ggplot(aes(reorder(word, n), n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(x = NULL,
           y = "Frequency",
           title = questio2) +
      coord_flip() + theme_bw()
    })
  
  
  output$tfidfPlot <- plotly::renderPlotly({
    colorch3 <- "purple3"
    user_ques_selection3 <- input$ques_no2
    if (user_ques_selection3 == 1){
      q_choice<- "Question1"
      colorch3 <- "purple3"
      questio3<- "1. Describe your love life?"
      
      } else if ( user_ques_selection3 == 2) {
      q_choice<- "Question2"
      colorch3 <- "tomato2"
      questio3<- "2. What has been your favorite valentines day yet?"
      
      } else if ( user_ques_selection3 == 3) {
      q_choice<- "Question3"
      colorch3 <- "turquoise4"
      questio3<- "3. Describe your perfect date."
      
      } else if ( user_ques_selection3 == 4) {
      q_choice<- "Question4"
      colorch3 <- "violetred4"
      questio3<- "4. What is an ideal gift for you?"
      
      } else if ( user_ques_selection3 == 5) {
      q_choice<- "Question5"
      colorch3 <- "palegreen3"
      questio3<-"5. Describe your worst Valentines Day."
      
      }   
    ####### Applying TF-IDF
    survey_words <- survey_words %>%
      bind_tf_idf(word, question, n)
    
    survey_words 
    
    survey_words %>%
      arrange(desc(tf_idf)) # %>% 
    
    
    survey_words %>%
      arrange(desc(tf_idf)) %>%
      mutate(word=factor(word, levels=rev(unique(word)))) %>%
      filter(question==q_choice) %>%
      top_n(10) %>%
      ggplot(aes(reorder(word, tf_idf),tf_idf))+
      geom_col(show.legend=FALSE,fill=colorch3)+
      labs(x=NULL, y="TF-IDF",title=questio3)+
      facet_wrap(~question, ncol=2, scales="free")+
      coord_flip()
    
  })
  
  output$bigramPlot <- renderPlot({
    ######################### N-grams and Neural Network ######################### 
    my_bigrams <- survey %>%
      unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
      filter(!is.na(bigram))
    
    my_bigrams %>%
      count(bigram, sort = TRUE) #this has many stop words, need to remove them 
    
    #to remove stop words from the bigram data, we need to use the separate function:
    bigrams_separated <- my_bigrams %>%
      separate(bigram, c("word1","word2"), sep = " ")
    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    bigram_count <- bigrams_filtered %>%
      count(word1, word2, sort = TRUE)
    
    bigram_count
    
    bigram_graph <- bigram_count %>%
      filter(n>1) %>%  #lower n to 1 or 2
      graph_from_data_frame()
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link(colour= "grey")+
      geom_node_point(colour = "black")+
      geom_node_text(aes(label=name,color=name), vjust =1, hjust=1,show.legend = FALSE)
    
  },height = 350, width = 600)
  
  output$quad_Table <- renderTable({
      my_quadrogram <- survey %>%
        unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
        filter(!is.na(quadrogram)) %>%
        separate(quadrogram, c("words1", "words2", "words3", "words4"), sep=" ") %>%
        filter(!words1 %in% stop_words$word) %>%
        filter(!words2 %in% stop_words$word) %>%
        filter(!words3 %in% stop_words$word) %>%
        filter(!words4 %in% stop_words$word) 
        my_quadrogram
  })
  

  
  
  my_data <- read_document(file="C:/Users/18579/Documents/MSBA/R/textual/team/flie/all_text.txt") #This comes out as a vector
  
  #Define parameters and create a empty dataframe
  rows <- 57 #how many observations to you have - how many people you have
  cols <- 6 #how many variables do you have - how many answers per person
  my_df <- as.data.frame(matrix(nrow=rows, ncol=cols))
  
  # Creating a nested for loop to fill in dataframe with corresponding line item
  for(z in 1:cols){
    for(i in 1:rows){
      my_df[i,z]<- my_data[i*cols+z-cols]
    }#closing z loop
  }#closing i loop
  
  #Create a dataframe for each question
  q1 <- data_frame(text=my_df$V1)
  q2 <- data_frame(text=my_df$V2)
  q3 <- data_frame(text=my_df$V3)
  q4 <- data_frame(text=my_df$V4)
  q5 <- data_frame(text=my_df$V5)
  q6 <- data_frame(text=my_df$V6)
  questions <- c("Question1", "Question2", "Question3", 
                 "Question4", "Question5", "Question6")
  colnames(my_df) <- questions
  survey <- bind_rows(mutate(q1, question = questions[1]),
                      mutate(q2, question = questions[2]),
                      mutate(q3, question = questions[3]),
                      mutate(q4, question = questions[4]),
                      mutate(q5, question = questions[5]),
                      mutate(q6, question = questions[6]))
  
  
  
  ######################## Creating custome Stop_words #####################
  custom_stop_words <- tribble(
    ~word,  ~lexicon,
    "valentine's","CUSTOM",
    "valentines","CUSTOM",
    "day","CUSTOM",
    "worst","CUSTOM",
    "perfect","CUSTOM",
    "favorite","CUSTOM",
    "gift","CUSTOM",
    "ideal","CUSTOM",
    "describe","CUSTOM",
    "a lot","CUSTOM",
    "date","CUSTOM",
    "jeez","CUSTOM",
    "gosh","CUSTOM",
    "haha","CUSTOM",
    "memory","CUSTOM"
  )
  #"celebrate","CUSTOM",
  
  stop_words <- stop_words %>% 
    bind_rows(custom_stop_words)
  stop_words
  
  
  ######################## Creating tidy df's##################
  
  tidy_q1 <- q1 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  #counting frequencies for tokens
  tidy_q1 %>%
    count(word, sort=TRUE)
  
  tidy_q2 <- q2 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  #counting frequencies for tokens
  tidy_q2 %>%
    count(word, sort=TRUE)
  
  tidy_q3 <- q3 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  #counting frequencies for tokens
  tidy_q3 %>%
    count(word, sort=TRUE)
  
  tidy_q4 <- q4 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  #counting frequencies for tokens
  tidy_q4 %>%
    count(word, sort=TRUE)
  
  tidy_q5 <- q5 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  #counting frequencies for tokens
  tidy_q5 %>%
    count(word, sort=TRUE)
  
  # Getting the dataset ready for TF-IDF
  tidy_survey <- survey %>% 
    group_by(question) %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% #here's where we remove tokens
    count(word, sort=TRUE) %>%
    ungroup()
  
  # Counting the total words
  total_words <- tidy_survey %>%
    group_by(question) %>% 
    summarize(total=sum(n))
  
  # Joining it to the original set
  survey_words <- left_join(tidy_survey, total_words)
  
}
shinyApp(ui = ui, server = server)

}