# Reddit Controversies: Dave Chapelle vs Joe Manchin 
# A Sentiment Analysis of Reddit-user Response to Current Controversies

# Load libraries                  # not done
library(shiny)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(wordcloud2)

# reddit controversy colour palette                
pal <- c("#FF5700", "black", "#858B8E", "white")

# data preparation done outside of server     
# read in data
dc_full <- read.csv("dc_full.csv")
dc_titl <- read.csv("dc_titl.csv")
dc_post <- read.csv("dc_post.csv")
dc_comm <- read.csv("dc_comm.csv")

jm_full <- read.csv("jm_full.csv")
jm_titl <- read.csv("jm_titl.csv")
jm_post <- read.csv("jm_post.csv")
jm_comm <- read.csv("jm_comm.csv")

# ggplot(data=data.frame(x=df1$x,y=df2$y), aes(x=x,y=y))    for safe keeping

# SECOND TAB - Manchin vs Chapelle Overall



# SHINY UI

ui <- navbarPage(inverse = TRUE, "Reddit",
                 
                 # First Page - Intro        
                 tabPanel("Intro", includeCSS("styles.css"),
                          fluidPage(h1("Controversies on Reddit: Dave Chapelle vs Joe Manchin"),
                                    br(),
                                    h3(strong(b("A Sentiment Analysis of Reddit-user Response to Current Controversies"))),
                                    br(),
                                    h5(em("Public Disagreements")),
                                      p("- On October 5th, 2021, Netflix premiered"),
                                        a("Dave Chapelle", href="https://en.wikipedia.org/wiki/Dave_Chappelle"),
                                        "'s hour-long stand-up comedy show special",
                                        a("'The Closer' (2021)", href="https://en.wikipedia.org/wiki/The_Closer_(2021_film)"),
                                        "on it's streaming platform. Within a few days of it's release, viewers of all backgrounds became vocally critical of remarks Chappelle made in the show. In an escalating battle for Netflix to discontinue streaming Chappelle's special, Netflix employee's have been protesting with political action in various forms. These include staging a walkout, leaking profitability data to Bloomberg News, and filing a federal labor charge", 
                                      p("- Over the past month, senators in Congress have been negotiating an infrastructure bill that is a key part of POTUS Joe Biden's",
                                        a("'Build Back Better' agenda.", href="https://en.wikipedia.org/wiki/Build_Back_Better_Plan"),
                                        "One of the key actors in these negotiations has been",
                                        a("Joe Manchin,",href="https://en.wikipedia.org/wiki/Joe_Manchin"), 
                                        "who effectively blocked ambitious climate- and social-action policy from being passed into the House of Representatives. Manchin's private shares in coal brokerage Enersystems and recipiency of large donations from coal, oil, and gas corporations has called into question his motives as a public servant in Congress, making him a highly controversial figure.",
                                    h5("So, what is this page about?"),
                                    p("The aim of this tool is to express how citizens feel about these issues. To some extent online discussion forums such as Reddit have replaced other forms of political expression and physical locations for community discussions (such as town halls or school gyms)."),
                                    p("My hope is that by using sentiment analysis tools", 
                                      a("---such as custom word-embeddings created by unsupervised machine learning models to create domain-specific sentiment lexicons for these online, user-selected discussion boards---"),
                                      "that we can more accurately understand popular opinion regarding controversial issues such as the ones explored here, and thereby bring clarity to policy makers and public servants.",
                                    h4(em("There's a tremendous gap between public opinion and public policy"), "- Noam Chomsky"),
                                    br(),
                                    br(),
                                    div(a(img(src = "manchin-chappelle.jpg", height = 230.4, width = 800), style="text-align: center;"), href="https://unsplash.com/photos/Sot0f3hQQ4Y?utm_source=unsplash&utm_medium=referral&utm_content=creditShareLink"),
                                    br(),
                                    br(),
                                    br(),
                                    div(p(strong("Built by"), a("Hadar Zeigerson", href = "https://nycdatascience.com/blog/author/hzeigersongmail-com/"), "using the power of Rstudio and Shiny."), 
                                        p(strong("R Packages:"), "RedditExtractoR, sentimentr, vader, tidyverse, dplyr, ggplot2, wordcloud2."),
                                        p(strong("Sources:"), a("reddit.com", href = "https://www.reddit.com/"), "for data on the", a("Dave Chappelle & Netflix", href = "https://www.reddit.com/search/?q=dave%20chappelle%20netflix%20trans"), "and", a("Joe Manchin", href = "https://www.reddit.com/search/?q=joe%20manchin%20climate%20bill"), "controversies."),
                                        style="text-align: right;")
                          )
                 )
        ),                 
                 # Second Page  - Overall Comparison      
                 tabPanel("At First Glance",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("text",
                                                                                     "Text:",
                                                                                     choices = 1:3,
                                                                                     selected = 1)),
                                                               wellPanel(style = "background: white",
                                                                         sliderInput("n", "Number of plots", value=1, min=1, max=4),
                                                                         
                                                               # wellPanel(style = "background: white",
                                                               #           h3("Info:"),
                                                               #           textOutput("lovecount_desc"),
                                                               #           br(),
                                                               #           p("The 'loveliest' songs are annotated.")),
                                                               )
                                                  ),
                                                  
                                                  mainPanel(
                                                    h3(strong("Overview: EDA Comparisons"),
                                                    br(),
                                                    p("Choose text and any number of plots to see how reddit-user sentiments compare."),
                                                    br(),
                                                    uiOutput("plots")
                                                        
                                                      ))
                                                    )
                                                   
                                                  )
                          )
                          )
                 )
                 
                 
                 # Third Page - Sentiment Explore
                 tabPanel("'Tell Me How You Really Feel'",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("sent_fill",
                                                                                     "Choose topic:",
                                                                                     choices = c("Joe Manchin Controversy" = "jm_full", "Dave Chappelle Controversy" = "dc_full"),
                                                                                     selected = "jm_full")),
                                                                         selectInput("sent_fill",
                                                                                     "Choose text format:",
                                                                                     choices = c("Titles" = "", "Posts" = "", "Comments" = ""),
                                                                                     selected = "disc_number")),
                                                               wellPanel(style = "background: white",
                                                                         p("Two songs, A Chicken with Its Head Cut Off and My Sentimental Melody, 
                                                           have high proportions of both positive and negative words,
                                                           suggesting they are 'songs of love and trouble'."),
                                                           p("I Shatter comfortably takes the most negative award.")),
                                                           wellPanel(style = "background: white",
                                                                     htmlOutput("sentiment_text"))),
                                                  mainPanel(
                                                    p(strong(em("\"Some of us can only live in songs of love and trouble.\""), "2.11 - My Only Friend")),
                                                    br(),
                                                    p("Let's try to assess sentiment, based on songs with the highest % of positive or negative words:"),
                                                    br(),
                                                    plotOutput("sentimentPlot"),
                                                    br(),
                                                    br(),
                                                    p(strong("Notes:"), "Songs with less than 20 distinct words were not considered. So for example, Punk Love, with it's repeated refrain \"Punk Rock Love\" (3 pretty positive things right?) does not appear.",
                                                      p("Positive and Negative sentiment was established using the Bing Lexicon."))
                                                  )
                          )
                          )
                 
                 # Fourth.2 Page  - Subreddit data      
                 tabPanel("Subs",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("controversy",
                                                                                            "Controversial Topic:",
                                                                                            choices = c("Dave Chappelle, Netflix, 'The Closer'","Joe Manchin, Build Back Better"),
                                                                                            selected = c("Dave Chappelle, Netflix, 'The Closer'","Joe Manchin, Build Back Better"))),
                                                               wellPanel(style = "background: white",
                                                                         sliderInput("n", "Number of plots", value=1, min=1, max=4),
                                                                         
                                                                         # wellPanel(style = "background: white",
                                                                         #           h3("Info:"),
                                                                         #           textOutput("lovecount_desc"),
                                                                         #           br(),
                                                                         #           p("The 'loveliest' songs are annotated.")),
                                                               )
                                                  ),
                                                  
                                                  mainPanel(
                                                    h3(strong("Overview: EDA Comparisons"),
                                                       br(),
                                                       p("Choose text and any number of plots to see how reddit-user sentiments compare."),
                                                       br(),
                                                       uiOutput("plots")
                                                       
                                                    ))
                          )
                          
                          )
                 )
                 )
)
                 
                                  
                 # Fifth Page - Clouds        
                 tabPanel("Clouds",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("controversy",
                                                                                            "Controversial Topic:",
                                                                                            choices = c("Dave Chappelle, Netflix, 'The Closer'","Joe Manchin, Build Back Better"),
                                                                                            selected = c("Dave Chappelle, Netflix, 'The Closer'","Joe Manchin, Build Back Better"))),
                                                                         selectInput("subreddit",
                                                                           "Subreddit(s)",
                                                                           choices = c(),
                                                                           selected = NULL,
                                                                           multiple = FALSE,
                                                                           selectize = TRUE,
                                                                           width = NULL,
                                                                           size = NULL
                                                                         ),
                                                                              ),
                                                            
                                                  mainPanel( 
                                                    p("Hover over the words below to see each word's appearance count"),
                                                    wordcloud2Output("wordcloud", width = "100%", height = "565px")
                                                  )
                          )
                          )
                 )


                 
                 # Sixth Page - NLP Notes and Future Steps
                 tabPanel("Notes and References", includeCSS("styles.css"),
                          fluidPage(h1("Technological Niavete"),
                                    h5("It should be stated that due to the extremely niche use of language within individual subreddits, all sentiment evaluations in this project are subject to a potentially large margin of error."),
                                    p("Luckily, solutions to this issue exist. A project by William L. Hamilton, Kevin Clark, Jure Leskovec, and Dan Jurafsky about"), 
                                      a("inducing domain-specific sentiment lexicons from unlabeled corpora", href = "https://arxiv.org/abs/1606.02820"),
                                      "(2016) contains code that could be adapted to create unsupervised machine learning models that automatically generate and update individualized lexicon dictionaries for every subreddit scraped. This would be useful both for improving the accuracy of the data presented, as well as to open the possibility of a self-updating app that relays the progression of sentiments over time."),
                                    p("Additionally, these models could incorporate algorithms inspired by packages like", a("syuzhet", href="https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html"), 
                                      "(among others) that currently have limited valence sensitivity but offer a broader range of emotional information (surprise, disgust, fear, anger, etc) or the ability to pull up the topics associated with high-valence language."), 
                                    p("The data in presented in this app was derived using", a("RedditExtractoR", href="https://cran.r-project.org/web/packages/RedditExtractoR/RedditExtractoR.pdf"),
                                      "for data collection, and for sentiment analysis ",
                                      a("VADER (Valence Aware Dictionary and sEntimentiment Reasoner)", href = "https://cran.r-project.org/web/packages/vader/vader.pdf"),
                                    ", an R package designed to assess the emotion of language used in social media specifically."),
                 )
                 
                 
                 # Seventh Page - About
                 tabPanel("About the Author", includeCSS("styles.css"),
                          fluidPage(h1("Turning A New Leaf"),
                                    br(),
                                    h3(strong(b("Hadar is a student of data in the early chapters of their journey..."))),
                                    br(),
                                    h5("Background"),
                                      p("Born in Baltimore and raised in Colorado,", a("Hadar", href = "https://nycdatascience.com/blog/author/hzeigersongmail-com/"),
                                        "grew up in an scientific household. After graduating from Colorado College in 2015 with a BA in Psychology, Hadar went on to explore a wide range of professions, from dishwashing at a loved local restaurant to providing student support at Waldorf and other hands-on learning schools. She's also worked in compost management, sustainability education, permaculture and organic farming, professional busking (street performance), music production, animation, film-editing, website design, field-management and canvassing, as well as ABA therapy."),
                                    h5("Journey into Data"),
                                      p("After a few years of exploration, Hadar started missing the challenges of academia. At the recommendation friends working in software and data technologies, Hadar began dabbling in code, and found the challenge of learning syntax and creating functional code engaging and fun. From there the question became, 'Where to apply these skills?'"),
                                    h4(em("The development of exponential technologies like new biotech and AI hint at a larger trend - one in which humanity can shift from a world of constrainst to one in which we think with a long-term purpose where sustainable food production, housing, and fresh water is available for all."), "- Arvind Gupta"),
                                    br(),
                                    br(),
                                    div(a(img(src = "wall-e.jpg", height = 461.85, width = 800), style="text-align: center;"), href="https://unsplash.com/photos/Sot0f3hQQ4Y?utm_source=unsplash&utm_medium=referral&utm_content=creditShareLink"),
                                  )
                               )
                       



# SHINY SERVER

server <- function(input, output) {
  
  # love count data
  lovecount_subset <- reactive({
    req(input$disc)
    filter(love_counts, disc_number %in% input$disc)
  })
  toplove_subset <- reactive({
    req(input$disc)
    filter(top_love, disc_number %in% input$disc)
  })
  commentary_subset <- reactive({
    req(input$disc)
    filter(commentary, disc_number %in% input$disc)
  })
  averages_subset <- reactive({
    req(input$disc)
    filter(averages, disc_number %in% input$disc) %>% 
      pull(avg_love)
  })   
  # Plot of love per volume
  output$lovecountPlot <- renderPlot({
    ggplot(lovecount_subset(), aes(x = track_number, y = love_count)) +
      geom_col(aes(fill = disc_colour), show.legend = FALSE) +
      geom_hline(aes(yintercept = avg_love, group = disc_number, 
                     colour = disc_colour), linetype = 2, show.legend = FALSE) +
      geom_text(data = toplove_subset(), aes(label = song), angle = 90, size = 3.2, colour = "white",
                hjust = 1.1, family = "Raleway") +
      geom_text(data = commentary_subset(), aes(label = comm, colour = disc_colour), 
                size = 4, hjust = 1, family = "Raleway", show.legend = FALSE) +
      geom_curve(aes(xend = xend, yend = yend, colour = disc_colour), 
                 data = commentary_subset(),
                 curvature = -0.2, 
                 arrow = arrow(type = "closed", length = unit(0.2,"cm")),
                 show.legend = FALSE) +
      scale_x_continuous(breaks = seq(1, 23, 2)) +
      scale_y_continuous(expand = c(0,0), limits = c(0, 22.5)) +
      scale_fill_identity() +
      scale_colour_identity() +
      labs(x = "Track", y = "Count") +
      theme_minimal() +
      theme(text = element_text(family = "Raleway"),
            panel.grid.minor.x = element_blank(),
            axis.line.x = element_line(colour = "black", size = 1),
            axis.text = element_text(size = 12),
            panel.grid.major.x = element_blank(),
            axis.ticks.x = element_line(),
            plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
    
  }, height = 500, width = 800)
  
  
  # Description of love per volume
  # output$lovecount_desc <- renderText({ 
  #   paste("Volume", input$disc, "has an average love count of", averages_subset(), "per song. Indicated by the dotted line.")
  # })
  
  
  # word counts (excluding stop words) for word clouds
  word_counts <- reactive({ 
    req(input$controversy)
    req(input$subreddit)
    lovesongs_tidy %>% 
      filter(disc_number %in% input$disc_cloud) %>%
      select(word) %>% 
      anti_join(stop_words, by = "word") %>%
      #filter(word != "love") %>% 
      count(word, sort = TRUE) 
  }) 
  
  # Word Clouds
  output$wordcloud <- renderWordcloud2({
    wordcloud2(word_counts(), size = 1.6, fontFamily = "Raleway",
               color=rep_len(pal[1:3], nrow(word_counts())), backgroundColor = "white")
  })
  
  
  # # Love Network
  # 
  # love_graph_data <- reactive({
  #   req(input$love_list)
  #   bigram_separated_love %>%
  #     filter(word1 %in% input$love_list | word2 %in% input$love_list) %>% 
  #     as_tbl_graph() %>% 
  #     mutate(color.background = if_else(name %in% input$love_list, "#E00008", "black"),
  #            color.border = if_else(name %in% input$love_list, "#E00008", "black"),
  #            label = name,
  #            labelHighlightBold = TRUE,
  #            #size = if_else(name == "love", 70, 25),
  #            font.face = "Raleway",
  #            font.size = if_else(name == "love", 70, 40),
  #            font.color = if_else(name %in% input$love_list, "#E00008", "black"),
  #            shape = if_else(name %in% input$love_list, "icon", "dot"),
  #            icon.face = "FontAwesome",
  #            icon.code = "f004",
  #            icon.size = if_else(name == "love", 200, 100),
  #            icon.color = if_else(name %in% input$love_list, "#E00008", "black")) %>% 
  #     activate(edges) %>% 
  #     mutate(hoverWidth = n,
  #            selectionWidth = n,
  #            scaling.max = 20)
  #   
  # }) 
  # 
  # 
  # output$lovenetwork <- renderVisNetwork({
  #   
  #   visIgraph(love_graph_data()) %>% 
  #     visInteraction(hover = TRUE, tooltipDelay = 0) %>% 
  #     addFontAwesome()
  #   
  # })
  
  
  # sentiment plot
  sentiment_pal <- c("black", "#E00008", "#858B8E", "#62A8E5", "#4000FF", "#1D2951")
  
  output$sentimentPlot <- renderPlot({
    ggplot(song_sentiment_tidy, aes(x = order, y = perc)) +
      geom_col(aes_string(fill = input$sent_fill)) + 
      geom_text(aes(label = song, y = 0.48), hjust = 1,
                family = "Raleway") +
      geom_text(aes(label = scales::percent(perc, accuracy = .1)), hjust = 1.1,
                family = "Raleway", colour = "white") +
      coord_flip() +
      facet_wrap(~ sentiment, scales = "free_y") +
      scale_x_continuous(breaks = song_sentiment_tidy$order,
                         expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1),
                         limits = c(0,0.5)) +
      scale_fill_manual(values = sentiment_pal) +
      labs(y = "\n% of Positive/Negative Sentiment", fill = NULL) +
      theme_minimal() +
      theme(text = element_text(family = "Raleway"),
            panel.grid.minor.y = element_blank(),
            panel.spacing = unit(1, "cm"),
            panel.border = element_rect(fill = NA, colour = "black", size = 1),
            strip.background = element_rect(fill = "white", colour = "black", size = 1),
            strip.text = element_text(colour = "black", face = "bold", size = 18),
            #axis.line.y = element_line(colour = "black", size = 1),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_line(),
            plot.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"),
            legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 1))
  })
  
  # Sentiment text
  output$sentiment_text <- renderUI({
    if (input$sent_fill == "disc_number") {
      HTML("<h4>Volumes:</h4><p>No songs from Volume 3 appear in the top 10 most positive list, with the majority coming from Volume 1.</p><p>3 songs from Volume 3 do appear in the negative list. Is Volume 3 the dark album?</p>")
    } else {
      HTML("<h4>Singers:</h4><p>Four guest vocalists appear on 69 Love Songs.</p><p>Dudley Klute takes on 2 of the highest proportioned negative songs, but misses out on the most positive songs.</p>")
    }
  })
  
  # Plot of blues
  blueshades <- tibble(colourname = c("Pantone 292", "Crayola Blue", "Liberty", "Space Cadet", "Teal", "Ultramarine"),
                       colourhex = c("#62A8E5", "#1F75FE", "#545AA7", "#1D2951", "#008080", "#4000FF"))
  
  blueshade <- reactive({
    blueshades %>% 
      filter(colourname == input$shade)
  })
  
  output$blues <- renderPlot({
    ggplot(tibble(x = 1:10, y = 1:10, label = paste0("...", blueshade()$colourname)), 
           aes(x, y)) +
      geom_point(colour = blueshade()$colourhex, show.legend = FALSE) +
      geom_text(aes(label = label, x = 5, y = 5), family = "Raleway", size = 8,
                colour = "white", show.legend = FALSE) +
      scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
      scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
      theme_void() +
      theme(plot.background = element_rect(fill = blueshade()$colourhex, colour = blueshade()$colourhex))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

