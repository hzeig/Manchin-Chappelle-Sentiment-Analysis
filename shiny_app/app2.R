library(shiny)
library(dplyr)
library(ggplot2)
library(wordcloud2)

# data prep in 'cleaning' and 'analysis' files
# read in data

# numbers
# dc_titl <-
# dc_post <-
# dc_comm <-
# jm_titl <-
# jm_post <-
# jm_comm <-
# 
# # tokens
# dc_clean <-
# jm_clean <-
#   
# # numbers and text
# dc_sent <-
# jm_sent <-

  
  

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
                                      "on it's streaming platform. Within a few days of it's release, viewers of all backgrounds became vocally critical of remarks Chappelle made in the show. In an escalating battle for Netflix to discontinue streaming Chappelle's special, Netflix employee's have been protesting with political action in various forms. These include staging a walkout, leaking profitability data to Bloomberg News, and filing a federal labor charge", 
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
                                      p("My hope is that by using sentiment analysis tools", 
                                        a("---such as custom word-embeddings created by unsupervised machine learning models to create domain-specific sentiment lexicons for these online, user-selected discussion boards---",),
                                        "that we can more accurately understand popular opinion regarding controversial issues such as the ones explored here, and thereby bring clarity to policy makers and public servants.",
                                        h4(em("There's a tremendous gap between public opinion and public policy"), "- Noam Chomsky"),
                                        br(),
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
                          
                          
                          # Sixth Page - NLP Notes and Future Steps
                          tabPanel("Notes",
                                   fluidPage(h3("Technological Niavity")),
                                             h5("Due to the", strong("niche use of language within individual subreddits"),
                                                ", sentiment evaluations in this project are subject to a potentially large margin of error."),
                                             p("Luckily, solutions to this issue exist. A project by", a("William L. Hamilton, Kevin Clark, Jure Leskovec, and Dan Jurafsky", href = "https://arxiv.org/abs/1606.02820"), 
                                               ",", em("Inducing Domain-Specific Sentiment Lexicons from Unlabeled Corpora"), 
                                               "(2016) contains code that could be adapted to create unsupervised machine learning models that automatically generate and update individualized lexicon dictionaries for every subreddit scraped. This would be useful both for improving the accuracy of the data presented, as well as to open the possibility of a self-updating app that relays the progression of sentiments over time."),
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
                          tabPanel("About the Author",
                                   fluidPage(h3("A New Leaf"),
                                             br(),
                                             h4(strong("Hadar is a student of data in the early chapters of their journey...")),
                                             br(),
                                             h5(em("Background")),
                                             p("Born in Baltimore and raised in Colorado,", a("Hadar", href = "https://nycdatascience.com/blog/author/hzeigersongmail-com/"),
                                               "grew up in an scientific household. After graduating from Colorado College in 2015 with a BA in Psychology, Hadar went on to explore a wide range of professions, from dishwashing at a loved local restaurant to providing student support at Waldorf and other hands-on learning schools. They've also worked in compost management, sustainability education, permaculture and organic farming, professional busking (street performance), music production, animation, film-editing, website design, field-management and canvassing, as well as ABA therapy."),
                                             h5(em("Journey into Data")),
                                             p("After a few years of exploration, Hadar started missing the challenges of academia. At the recommendation friends working in software and data technologies, Hadar began dabbling in code, and found the challenge of learning syntax and creating functional code engaging and fun. From there the question became, \"Where to apply these skills?\""),
                                             br(),
                                             br(),
                                             h6(strong(em("\"The development of exponential technologies like new biotech and AI hint at a larger trend - one in which humanity can shift from a world of constrainst to one in which we think with a long-term purpose where sustainable food production, housing, and fresh water is available for all.\""), "- Arvind Gupta")),
                                             div(a(img(src = "wall-e.jpg", height = 461.85, width = 800), style="text-align: center;"), href="https://unsplash.com/photos/Sot0f3hQQ4Y?utm_source=unsplash&utm_medium=referral&utm_content=creditShareLink"),
                                   )
                          )
                          
                 )



# SHINY SERVER

server <- function(input, output) {
  
}


# Run the application 
shinyApp(ui = ui, server = server)