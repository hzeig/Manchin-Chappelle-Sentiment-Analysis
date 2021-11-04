library(shiny)
library(dplyr)
library(ggplot2)
library(egg)
library(wordcloud2)
library(lubridate)
library(scales)

# reddit controversy colour palette                
pal <- c("#FF5700", "black", "#858B8E", "white")


# read in data:
# data preparation done outside of shiny     

# text with sentiment
dc_full <- read.csv("dc_full.csv")
dc_titlf <- read.csv("dc_titl.csv")
dc_postf <- read.csv("dc_post.csv")
dc_comm <- read.csv("dc_comm.csv")

jm_full <- read.csv("jm_full.csv")
jm_titlf <- read.csv("jm_titl.csv")
jm_postf <- read.csv("jm_post.csv")
jm_comm <- read.csv("jm_comm.csv")

dc_titl <- read.csv("dc_titl_unique.csv")
dc_post <- read.csv("dc_post_unique.csv")
jm_titl <- read.csv("jm_titl_unique.csv")
jm_post <- read.csv("jm_post_unique.csv")

# # tokens for wordclouds
# dc_clean <-
# jm_clean <-

### Tab Two - Overview Comparisons by Text ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### density and scatter (sentiment x number of comments,siz = score)

##### SCORE PREP ##### add score to vader files ##### 
#~~~~~~~~~~~~~~~~~~~~DC~~~~~~~~~~~~~~~~~~~~~~~#
# title
r <- select(dc_titl, title=text, pos, neg)
t <- select(dc_full, title, thread.score, thread.number.comments)
s <- unique(t)
dc_titl_score <-merge(s, r, by = "title")

# post
r <- select(dc_post, post=text, pos, neg)
t <- select(dc_full, post, thread.score, thread.number.comments)
s <- unique(t)
dc_post_score <-merge(s, r, by = "post")

# comment
r <- select(dc_comm, comment=text, pos, neg)
t <- select(dc_full, comment, comment.score)
dc_comment_score <-merge(t, r, by = "comment")
dc_comm_score <- unique(dc_comment_score)

#~~~~~~~~~~~~~~~~~~~JM~~~~~~~~~~~~~~~~~~~~~~~~#
# title
r <- select(jm_titl, title=text, pos, neg)
t <- select(jm_full, title, thread.score, thread.number.comments)
s <- unique(t)
jm_titl_score <-merge(s, r, by = "title")

# post
r <- select(jm_post, post=text, pos, neg)
t <- select(jm_full, post, thread.score, thread.number.comments)
s <- unique(t)
jm_post_score <-merge(s, r, by = "post")

# comment
r <- select(jm_comm, comment=text, pos, neg)
t <- select(jm_full, comment, comment.score)
jm_comment_score <-merge(t, r, by = "comment")
jm_comm_score <- unique(jm_comment_score)

################ PLOTS ###################################
######################################### Title ##########
########## pos ##########


p1 <- ggplot() + 
  geom_histogram(dc_titl, mapping=aes(x= pos, fill="Dave Chapelle"), alpha = .4) +
  geom_histogram(jm_titl, mapping = aes(x = pos, fill="Joe Manchin"), alpha = .4) +
  labs(title = "Distribution of Positive Sentiment in Titles") +
  xlab("level of positivity in sentiment") +
  scale_fill_manual(name="Controversy", values = c("#FF5700","black"))

p2 <- ggplot() + 
  geom_point(dc_titl_score, 
             mapping=aes(x= pos, 
                         y=thread.score, 
                         size = thread.number.comments,
                         color="Dave Chapelle"), alpha = .4) +
  geom_point(jm_titl_score, 
             mapping = aes(x= pos, 
                           y=thread.score, 
                           size = thread.number.comments, 
                           color="Joe Manchin"), alpha = .4) +
  labs(title = "Positive Sentiment of Title vs Thread Upvotes") +
  labs(x="level of positivity in sentiment", y="number of upvotes", size = "Number of Comments") +
  scale_color_manual(name="Controversy", values = c("#FF5700","black"))

########### neg ########

p3 <- ggplot() + 
  geom_histogram(dc_titl, mapping=aes(x= neg, fill="Dave Chapelle"), alpha = .4) +
  geom_histogram(jm_titl, mapping = aes(x = neg, fill="Joe Manchin"), alpha = .4) +
  labs(title = "Distribution of Title Negative Sentiment in Titles") +
  xlab("level of negativity in sentiment") +
  scale_fill_manual(name="Controversy", values = c("#FF5700","black"))

p4 <- ggplot() + 
  geom_point(dc_titl_score, 
             mapping=aes(x= neg, 
                         y=thread.score, 
                         size = thread.number.comments,
                         color="Dave Chapelle"), alpha = .4) +
  geom_point(jm_titl_score, 
             mapping = aes(x= neg, 
                           y=thread.score, 
                           size = thread.number.comments, 
                           color="Joe Manchin"), alpha = .4) +
  labs(title = "Negative Sentiment of Title vs Thread Upvotes") +
  labs(x="level of negativity in sentiment", y="number of upvotes", size = "Number of Comments") +
  scale_color_manual(name="Controversy", values = c("#FF5700","black"))


##################################### Post ##############
####### pos ######

p5 <- ggplot() + 
  geom_histogram(dc_post, mapping=aes(x= pos, fill="Dave Chapelle"), alpha = .4) +
  geom_histogram(jm_post, mapping = aes(x = pos, fill="Joe Manchin"), alpha = .4) +
  labs(title = "Distribition of level of positivity in sentiment in Posts") +
  xlab("level of positivity in sentiment") +
  scale_fill_manual(name="Controversy", values = c("#FF5700","black"))

p6 <- ggplot() + 
  geom_point(dc_post_score, 
             mapping=aes(x= pos, 
                         y=thread.score, 
                         size = thread.number.comments,
                         color="Dave Chapelle"), alpha = .4) +
  geom_point(jm_post_score, 
             mapping = aes(x= pos, 
                           y=thread.score, 
                           size = thread.number.comments, 
                           color="Joe Manchin"), alpha = .4) +
  labs(title = "Positive Sentiment of Post vs Thread Upvotes") +
  labs(x="level of positivity in sentiment", y="number of upvotes", size = "Number of Comments") +
  scale_color_manual(name="Controversy", values = c("#FF5700","black"))

####### neg ######

p7 <- ggplot() + 
  geom_histogram(dc_post, mapping=aes(x= neg, fill="Dave Chapelle"), alpha = .4) +
  geom_histogram(jm_post, mapping = aes(x = neg, fill="Joe Manchin"), alpha = .4) +
  labs(title = "Distribition of Negative Sentiment in Posts") +
  xlab("level of negativity in sentiment") +
  scale_fill_manual(name="Controversy", values = c("#FF5700","black"))

p8 <- ggplot() + 
  geom_point(dc_post_score, 
             mapping=aes(x= neg, 
                         y=thread.score, 
                         size = thread.number.comments,
                         color="Dave Chapelle"), alpha = .4) +
  geom_point(jm_post_score, 
             mapping = aes(x= neg, 
                           y=thread.score, 
                           size = thread.number.comments, 
                           color="Joe Manchin"), alpha = .4) +
  labs(title = "Negative Sentiment of Post vs Thread Upvotes") +
  labs(x="level of negativity in sentiment", y="number of upvotes", size = "Number of Comments") +
  scale_color_manual(name="Controversy", values = c("#FF5700","black"))

################################## Comment ##############
######## pos #########

p9 <- ggplot() + 
  geom_histogram(dc_comm, mapping=aes(x= pos, fill="Dave Chapelle"), alpha = .4) +
  geom_histogram(jm_comm, mapping = aes(x = pos, fill="Joe Manchin"), alpha = .4) +
  labs(title = "Distribution of Positive Sentiment in Comments") +
  xlab("level of positivity in sentiment") +
  scale_fill_manual(name="Controversy", values = c("#FF5700","black"))

p10 <- ggplot() + 
  geom_point(dc_comment_score, 
             mapping=aes(x= pos, 
                         y=comment.score, 
                         color="Dave Chapelle"), alpha = .4) +
  geom_point(jm_comment_score, 
             mapping = aes(x= pos, 
                           y=comment.score, 
                           color="Joe Manchin"), alpha = .4) +
  labs(title = "Positive Sentiment of Comment vs Comment Upvotes") +
  labs(x="level of positivity in sentiment", y="number of upvotes") +
  scale_color_manual(name="Controversy", values = c("#FF5700","black"))


######## neg ########

p11 <- ggplot() + 
  geom_histogram(dc_comm, mapping=aes(x= neg, fill="Dave Chapelle"), alpha = .4) +
  geom_histogram(jm_comm, mapping = aes(x = neg, fill="Joe Manchin"), alpha = .4) +
  labs(title = "Distribition of Negative Sentiment in Comments") +
  xlab("level of negativity in sentiment") +
  scale_fill_manual(name="Controversy", values = c("#FF5700","black"))

p12 <- ggplot() + 
  geom_point(dc_comment_score, 
             mapping=aes(x= neg, 
                         y=comment.score, 
                         color="Dave Chapelle"), alpha = .4) +
  geom_point(jm_comment_score, 
             mapping = aes(x= neg, 
                           y=comment.score, 
                           color="Joe Manchin"), alpha = .4) +
  labs(title = "Negative Sentiment of Comment vs Comment Upvotes") +
  labs(x="level of negativity in sentiment", y="number of upvotes") +
  scale_color_manual(name="Controversy", values = c("#FF5700","black"))


### Tab Four - Time Series ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### DATE PREP ##### add date to vader files ##### 
#~~~~~~~~~~~~~~~~~~~~DC~~~~~~~~~~~~~~~~~~~~~~~#
# title
r <- select(dc_titl, title=text, pos, neg)
t <- select(dc_full, title, date=thread.date)
s <- unique(t)
dc_titl_date <-merge(s, r, by = "title")

# post
r <- select(dc_post, post=text, pos, neg)
t <- select(dc_full, post, date=thread.date)
s <- unique(t)
dc_post_date <-merge(s, r, by = "post")

# comment
r <- select(dc_comm, comment=text, pos, neg)
t <- select(dc_full, comment, date=comment.date)
dc_comment_date <-merge(t, r, by = "comment")
dc_comm_date <- unique(dc_comment_date)

#~~~~~~~~~~~~~~~~~~~JM~~~~~~~~~~~~~~~~~~~~~~~~#
# title
r <- select(jm_titl, title=text, pos, neg)
t <- select(jm_full, title, date=thread.date)
s <- unique(t)
jm_titl_date <-merge(s, r, by = "title")

# post
r <- select(jm_post, post=text, pos, neg)
t <- select(jm_full, post, date=thread.date)
s <- unique(t)
jm_post_date <-merge(s, r, by = "post")

# comment
r <- select(jm_comm, comment=text, pos, neg)
t <- select(jm_full, comment, date=comment.date)
jm_comment_date <-merge(t, r, by = "comment")
jm_comm_date <- unique(jm_comment_date)


# lubridate dfs

dc_titl_date$date <- ymd(dc_titl_date$date)
dc_post_date$date <- ymd(dc_post_date$date)
dc_comm_date$date <- ymd(dc_comm_date$date)
jm_titl_date$date <- ymd(jm_titl_date$date)
jm_post_date$date <- ymd(jm_post_date$date)
jm_comm_date$date <- ymd(jm_comm_date$date)

################ PLOTS ###################################
################################### Title ################
#### pos ####
e <- ggplot() +
  geom_line(dc_titl_date, 
            mapping = aes(x=date, 
                          y=pos, 
                          color="Dave Chappelle")) + 
  geom_line(jm_titl_date, 
            mapping = aes(x=date,
                          y=pos,
                          color="Joe Manchin")) +
  labs(title="Positive Sentiment of Titles over Time",
       x="date", y="level of positivity in sentiment") +
  scale_x_date(labels=date_format ("%b %y")) +
  scale_color_manual(name="Controversy", values = c("#FF5700","black")) 


#### neg ####

f <- ggplot() +
  geom_line(dc_titl_date, 
            mapping = aes(x=date, 
                          y=neg, 
                          color="Dave Chappelle")) + 
  geom_line(jm_titl_date, 
            mapping = aes(x=date,
                          y=neg,
                          color="Joe Manchin")) +
  labs(title="Negative Sentiment of Titles over Time",
       x="date", y="level of negativity in sentiment") +
  scale_x_date(labels=date_format ("%b %y")) +
  scale_color_manual(name="Controversy", values = c("#FF5700","black")) 



#####################3############## Post ################
#### pos ####

g <- ggplot() +
  geom_line(dc_post_date, 
            mapping = aes(x=date, 
                          y=pos, 
                          color="Dave Chappelle")) + 
  geom_line(jm_post_date, 
            mapping = aes(x=date,
                          y=pos,
                          color="Joe Manchin")) +
  labs(title="Positive Sentiment of Posts over Time",
       x="date", y="level of positivity in sentiment") +
  scale_x_date(labels=date_format ("%b %y")) +
  scale_color_manual(name="Controversy", values = c("#FF5700","black")) 


#### neg ####

h <- ggplot() +
  geom_line(dc_post_date, 
            mapping = aes(x=date, 
                          y=neg, 
                          color="Dave Chappelle")) + 
  geom_line(jm_post_date, 
            mapping = aes(x=date,
                          y=neg,
                          color="Joe Manchin")) +
  labs(title="Negative Sentiment of Posts over Time",
       x="date", y="level of negativity in sentiment") +
  scale_x_date(labels=date_format ("%b %y")) +
  scale_color_manual(name="Controversy", values = c("#FF5700","black")) 


################################### Comment ###############
##### pos ####

i <- ggplot() +
  geom_line(dc_comm_date, 
            mapping = aes(x=date, 
                          y=pos, 
                          color="Dave Chappelle")) + 
  geom_line(jm_comm_date, 
            mapping = aes(x=date,
                          y=pos,
                          color="Joe Manchin")) +
  labs(title="Positive Sentiment of Comments over Time",
       x="date", y="level of positivity in sentiment") +
  scale_x_date(labels=date_format ("%b %y")) +
  scale_color_manual(name="Controversy", values = c("#FF5700","black")) 


##### neg ####

j <- ggplot() +
  geom_line(dc_comm_date, 
            mapping = aes(x=date, 
                          y=neg, 
                          color="Dave Chappelle")) + 
  geom_line(jm_comm_date, 
            mapping = aes(x=date,
                          y=neg,
                          color="Joe Manchin")) +
  labs(title="Negative Sentiment of Comments over Time",
       x="date", y="level of negativity in sentiment") +
  scale_x_date(labels=date_format ("%b %y")) +
  scale_color_manual(name="Controversy", values = c("#FF5700","black")) 



### Tab Five - Text Collection by Sentiment ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All
# pos

# neg


# Title
# pos

# neg


# Post
# pos

# neg

# Comment
# pos

# neg



### Tab Six - Subreddit Stats top 5 ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# number of threads

# number of comments

# range of sentimet

# most positive

# most negative



### Tab Seven - Wordcloud ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# all

# dc

# jm




# SHINY UI
ui <- navbarPage(inverse = TRUE, "Reddit Speaks",
                 
                 # First Page - Intro        
                 tabPanel("Intro", includeCSS("styles.css"),
                          fluidPage(h3("Controversies on Reddit: Dave Chapelle vs Joe Manchin"),
                                    h4("A Sentiment Analysis of Reddit-user Response to Current Controversies"),
                                    br(),
                                    h5(strong(em("Public Disagreements"))),
                                    p("- On October 5th, 2021, Netflix premiered",
                                      a("Dave Chapelle", href="https://en.wikipedia.org/wiki/Dave_Chappelle"),
                                      "'s hour-long stand-up comedy show special",
                                      a("'The Closer' (2021)", href="https://en.wikipedia.org/wiki/The_Closer_(2021_film)"),
                                      "on it's streaming platform. Within a few days of it's release, viewers of all backgrounds became vocally critical of the harmful jokes Chappelle made in the show, jokes expressing anti-trans/lbtq, anti-asian, anti-semitic, pro-transphobic, pro-racist, and pro-mysogynist sentiments.", 
                                      "In an escalating battle for Netflix to discontinue streaming Chappelle's special, Netflix employee's have been protesting with political action in various forms. These include staging a walkout, leaking profitability data to Bloomberg News, and filing a federal labor charge", 
                                    p("- Over the past month, senators in Congress have been negotiating an infrastructure bill that is a key part of POTUS Joe Biden's",
                                      a("'Build Back Better' agenda.", href="https://en.wikipedia.org/wiki/Build_Back_Better_Plan"),
                                      "One of the key actors in these negotiations has been",
                                      a("Joe Manchin,",href="https://en.wikipedia.org/wiki/Joe_Manchin"), 
                                      "who effectively blocked ambitious climate- and social-action policy from being passed into the House of Representatives. Manchin's private shares in coal brokerage Enersystems and recipiency of large donations from coal, oil, and gas corporations has called into question his motives as a public servant in Congress, making him a highly controversial figure.",
                                      br(),
                                      br(),
                                      h5(strong(em("So, what is this page about?"))),
                                      p("The aim of this tool is to", strong("express how citizens feel about these issues"),
                                        ". To some extent online discussion forums such as Reddit have replaced other forms of political expression and physical locations for community discussions (such as town halls or school gyms)."),
                                      p(strong("This page is just a prototype."), 
                                        "The goal is to one day have a dynamic, updatable app that uses machine learning models, like those from", 
                                        a("SocialSent", href="https://nlp.stanford.edu/projects/socialsent/"),
                                          "to create custom word-embeddings derived by unsupervised machine learning models that deduce domain-specific, sentiment-weighted lexicons for these online, user-selected discussion boards,",
                                          "so as to,", strong("more accurately understand popular opinion regarding controversial issues"),
                                          ", and thereby bring clarity to policy and media makers.",
                                        br(),
                                        br(),
                                        h4(em("\"There's a tremendous gap between public opinion and public policy\""), "- Noam Chomsky"),
                                        br(),
                                        div(a(img(src = "manchin-chappelle.jpg", height = 230.4, width = 800), style="text-align: center;"), href="https://unsplash.com/photos/Sot0f3hQQ4Y?utm_source=unsplash&utm_medium=referral&utm_content=creditShareLink"),
                                        br(),
                                        br(),
                                        br(),
                                        div(p(strong("Built by"), a("Hadar Zeigerson", href = "https://nycdatascience.com/blog/author/hzeigersongmail-com/"), "using the power of RStudio and RShiny."), 
                                            p(strong("R Packages:"), "RedditExtractoR, sentimentr, vader, tidyverse, dplyr, ggplot2, wordcloud2."),
                                            p(strong("Sources:"), a("reddit.com", href = "https://www.reddit.com/"), "for data on the", a("Dave Chappelle", href = "https://www.reddit.com/search/?q=dave%20chappelle%20netflix%20trans"), "and", a("Joe Manchin", href = "https://www.reddit.com/search/?q=joe%20manchin%20climate%20bill"), "controversies."),
                                            style="text-align: right;")
                                      )
                                    )
                                 ))
                              ),                 
                          
                 # Second Page  - Overall Comparison      
                 tabPanel("At First Glance",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("text_type", 
                                                                                     "Text Type", 
                                                                                     choices = list("title", "post", "comment", "Select Text"),
                                                                                     selected = "Select Text")
                                                               )),
                                                  mainPanel(
                                                    h3("Overview: EDA Comparisons",
                                                       br(),
                                                       h5("Choose a text type to see how reddit-user sentiments compare across controversies."),
                                                       br(),
                                                       plotOutput("comparison")
                                                       
                                                    )
                                                  ))
                          )
                 ),
                 
                 
                 # Third Page  - Time Series      
                 tabPanel("Trends",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("text_type_time", 
                                                                                     "Text Type", 
                                                                                     choices = list("title", "post", "comment", "Select Text"),
                                                                                     selected = "Select Text")
                                                               )),
                                                  mainPanel(
                                                    h3("Comparing Sentiments Over Time",
                                                       br(),
                                                       h5("Choose a text type to see how reddit-user sentiments have changed over a month."),
                                                       br(),
                                                       plotOutput("time_series")
                                                       
                                                    )
                                 ))
                          )
                   ),

                                  
                 # Third Page - Sentiment Slider      
                 tabPanel("Tell Me How You Really Feel",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("controversy", 
                                                                                     "Controversy", 
                                                                                     choices = list("Dave Chapelle", "Joe Manchin", "Select Controversy"),
                                                                                     selected = "Select Controversy")
                                                               ),
                                                               wellPanel(style = "background: white",
                                                                         sliderInput("sentiment", "Range of Sentiment:",
                                                                                     min = -1, max = 1, step = .01, value = c(-0.3, 0.2))
                                                               ),
                                                               wellPanel(style = "background: white",
                                                                         selectInput("text_type", 
                                                                                     "Text Type", 
                                                                                     choices = list("title", "post", "comment", "Select Text"),
                                                                                     selected = "Select Text")
                                                               )),
                                                  mainPanel(
                                                    h3("Peeking into the subs",
                                                       br(),
                                                       h5("Use the slider to see what redditors are saying about these controversies within your chosen range of sentiment intensity."),
                                                       br(),
                                                       # tableOutput("sentiments")
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       h2("This page is under construction")
                                                    )
                                                  ))
                          )
                 ),
                 
                 
                 # Fourth Page - Top Subreddit Stats      
                 tabPanel("Sub Stats",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("controversy", 
                                                                                     "Controversy", 
                                                                                     choices = list("Dave Chapelle", 
                                                                                                    "Joe Manchin", 
                                                                                                    "Select Controversy"),
                                                                                     selected = "Select Controversy")
                                                               ),
                                                               wellPanel(style = "background: white",
                                                                         selectInput("text_type", 
                                                                                     "Text Type", 
                                                                                     choices = list("title", 
                                                                                                    "post", 
                                                                                                    "comment", 
                                                                                                    "Select Text"),
                                                                                     selected = "Select Text")
                                                               ),
                                                               wellPanel(style = "background: white",
                                                                         selectInput("topic", 
                                                                                     "Topic", 
                                                                                     choices = list("most positive",
                                                                                                    "most negative",
                                                                                                    "broadest range of sentiment",
                                                                                                    "number of threads", 
                                                                                                    "number of comments", 
                                                                                                    "Select Topic"),
                                                                                     selected = "Select Topic")
                                                               )),
                                                  mainPanel(
                                                    h3("Subreddit-Specific Statistics",
                                                       br(),
                                                       h5("Choose a topic to see the top ten subreddits in that measure."),
                                                       br(),
                                                       # tableOutput("sentiments")
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       h2("This page is under construction")
                                                    )
                                                  ))
                          )
                 ),
                 
                 
                 
                 # Fifth Page - WordClouds      
                 tabPanel("Wordclouds",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("controversy", 
                                                                                     "Controversy", 
                                                                                     choices = list("Dave Chapelle", 
                                                                                                    "Joe Manchin"), 
                                                                                     selected = list("Dave Chapelle", 
                                                                                                     "Joe Manchin"))
                                                               ),
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("text_type", 
                                                                                     "Text Type", 
                                                                                     choices = list("title", 
                                                                                                    "post", 
                                                                                                    "comment"),
                                                                                     selected = list("title",
                                                                                                     "post", 
                                                                                                     "comment"))
                                                               ),
                                                               wellPanel(style = "background: white",
                                                                         selectInput("subreddit", 
                                                                                     "Subreddit", 
                                                                                     choices = list("all",
                                                                                                    "Threads about DC",
                                                                                                    "Threads about JM",
                                                                                                    "Select Subreddit"),
                                                                                     selected = "Select Subreddit")
                                                               )),
                                                  mainPanel(
                                                    h3("Wordcloud",
                                                       br(),
                                                       h5("Choose your filters to see what people are saying about this topic."),
                                                       br(),
                                                       # tableOutput("sentiments")
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       h2("This page is under construction")
                                                    )
                                                  ))
                          )
                 ),
                   
                  # Sixth Page - NLP Notes and Future Steps
                  tabPanel("Notes",
                           fluidPage(h3("Technological Niavity")),
                                     h5("Due to the", strong("niche use of language within individual subreddits,"),
                                        "sentiment evaluations in this project are subject to a potentially large margin of error."),
                                     p("Luckily, solutions to this issue exist. A project by", a("William L. Hamilton, Kevin Clark, Jure Leskovec, and Dan Jurafsky", href = "https://arxiv.org/abs/1606.02820"), 
                                       ",", em("Inducing Domain-Specific Sentiment Lexicons from Unlabeled Corpora"), 
                                       "(2016) contains code that could be adapted to create unsupervised machine learning models that automatically generate and update individualized lexicon dictionaries for every subreddit scraped.", 
                                       "This would be useful both for improving the accuracy of the data presented, as well as to open the possibility of a self-updating app that relays the progression of sentiments over time."),
                                     br(),
                                     br(),
                                     p("Additionally, these models could incorporate algorithms inspired by packages like", a("syuzhet", href="https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html"), 
                                        "(among others) that currently have limited valence sensitivity but offer a broader range of emotional information (surprise, disgust, fear, anger, etc) or the ability to pull up the topics associated with high-valence language."), 
                                     p("The data in presented in this app was derived using", a("RedditExtractoR", href="https://cran.r-project.org/web/packages/RedditExtractoR/RedditExtractoR.pdf"),
                                       "for data collection, and for sentiment analysis ",
                                       a("VADER (Valence Aware Dictionary and sEntimentiment Reasoner),", href = "https://cran.r-project.org/web/packages/vader/vader.pdf"),
                                       "an R package designed to assess the emotion of language used in social media specifically."),
                  ),
                  
                  
                  # Seventh Page - About
                  tabPanel("About",
                           fluidPage(h3("A New Leaf"),
                                     br(),
                                     h4(strong("Hadar is a student of data in the early chapters of their journey...")),
                                     br(),
                                     h5(em("Background")),
                                     p("Born in Baltimore and raised in Colorado,", a("Hadar", href = "https://nycdatascience.com/blog/author/hzeigersongmail-com/"),
                                       "grew up in an scientific household. After graduating from Colorado College in 2015 with a BA in Psychology, Hadar went on to explore a wide range of professions, from dishwashing at a loved local restaurant to providing student support at Waldorf and other hands-on learning schools. They've also worked in compost management, sustainability education, permaculture and organic farming, professional busking (street performance), music production, animation, film-editing, website design, field-management and canvassing, as well as ABA therapy."),
                                     br(),
                                     h5(em("Journey into Data")),
                                     p("After a few years of exploration, Hadar missed the challenges of academia. At the recommendation of friends working in software and data technologies, Hadar began dabbling in code, and found the challenge of learning syntax and creating functional code engaging and fun. From there the question became, \"Where to apply these skills?\""),
                                     br(),
                                     h4(strong(em("\"The development of exponential technologies like new biotech and AI hint at a larger trend - one in which humanity can shift from a world of constrainst to one in which we think with a long-term purpose where sustainable food production, housing, and fresh water is available for all.\""), "- Arvind Gupta")),
                                     br(),
                                     div(a(img(src = "wall-e.jpg", height = 461.85, width = 800), style="text-align: center;"), href="https://unsplash.com/photos/Sot0f3hQQ4Y?utm_source=unsplash&utm_medium=referral&utm_content=creditShareLink"),
                           )
                  )
         )                   
  



# SHINY SERVER

server <- function(input, output) {
  
  # Tab2 Reactive
  text_type <- reactive({
    switch(input$text_type, 
           "title" = a <- grid.arrange(p1, p2, p3, p4, nrow = 2), 
           "post" = b <- grid.arrange(p5, p6, p7, p8, nrow = 2), 
           "comment" = c <- grid.arrange(p9, p10, p11, p12, nrow = 2),
           "Select Text" = NULL)
  })
  # Tab3 Reactive
  text_type_time <- reactive({
    switch(input$text_type_time, 
           "title" = grid.arrange(e,f, nrow = 2), 
           "post" = grid.arrange(g, h, nrow = 2), 
           "comment" = grid.arrange(i,j, nrow = 2),
           "Select Text" = NULL)
  })
  # # Tab4 Reactives
  # controversy <- reactive({
  #   switch(input$controversy,
  #          "Dave Chapelle" = ,
  #          "Joe Manchin" = ,
  #          "Select Controversy" = NULL)
  # })
  # text_type <- reactive({
  #   switch(input$text_type, 
  #          "title" = , 
  #          "post" = , 
  #          "comment" = ,
  #          "Select Text" = NULL) 
  # })
  # Tab2 Output
  output$comparison <- renderPlot({
    plot <- text_type()
    print(plot)
  })
  
  # # Tab3 Outputs
  output$time_series <- renderPlot({
    plot <- text_type_time()
    print(plot)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)