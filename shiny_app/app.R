# Reddit Controversies: Dave Chapelle vs Joe Manchin 
# A Sentiment Analysis of Reddit-user Response to Current Controversies

# Load libraries                  # not done
library(shiny)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(wordcloud2)
library(glue)
library(visNetwork)

# reddit controversy colour palette                
pal <- c("#FF5700", "black", "#858B8E", "white")

# data preparation to be done outside of server     # not done
# read in data
lovesongs <- read_rds("69LoveSongs.rds")

# tidy text                                 # ugh would be so cool... not done
lovesongs_tidy <- lovesongs %>% 
  # remove intstructions in [ ]
  filter(!str_detect(lyric, "(\\[.*\\])")) %>% 
  unnest_tokens(word, lyric)

# FIRST TAB - Manchin vs Chapelle Overall

# SECOND TAB - Top 10

# THIRD TAB - Wordcloud

# all words beginning with lov - to get occurences of 'love' plus variants
love_words <- sort(unique(str_subset(lovesongs_tidy$word, "^lov")))
love_words

# put above words into single string for use in title of plot
love_words_collapse <- paste(love_words, collapse = ", ")

# count up uses of love (and variants in each song). calculate average use per song, at disc level
love_counts <- lovesongs_tidy %>% 
  group_by(song, disc_number, track_number) %>% 
  summarise(love_count = sum(word %in% love_words)) %>% 
  arrange(disc_number, track_number) %>% 
  ungroup() %>% 
  group_by(disc_number) %>% 
  mutate(avg_love = mean(love_count),
         disc_colour = case_when(disc_number == 1 ~ "black",
                                 disc_number == 2 ~ "#E00008",
                                 TRUE ~ "#858B8E")) %>% 
  ungroup()

# songs with most love ocurrences
top_love <- love_counts %>% 
  top_n(5, love_count)

# annotation to add to most loved songs
commentary <- tibble(track_number = c(18, 11, 5), 
                     love_count = c(18, 11, 14), 
                     disc_number = c(1, 2, 3),
                     disc_colour =  c("black", "#E00008", "#858B8E"),
                     xend = c(21.5, 13, 2.5), 
                     yend = c(20, 9, 12),
                     comm = c("Contains a lot of lovin'", "Also contains music,\nwine and revolution", "...to fall in love "))


# average love per song for each disc
averages <- love_counts %>% 
  distinct(disc_number, avg_love) %>% 
  mutate(avg_love = round(avg_love, 2))


# THIRD TAB - LOVE BIGRAM NETWORK

# Love bigrams for network
lovesongs_bigram <- lovesongs %>% 
  # remove intstructions in [ ]
  filter(!str_detect(lyric, "(\\[.*\\])")) %>% 
  unnest_tokens(bigram, lyric, token = "ngrams", n = 2)

lovesongs_bigram %>% 
  count(bigram, sort = TRUE)

bigram_separated <- lovesongs_bigram %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

# bigrams including a love element
bigram_separated_love <- 
  bigram_separated %>% 
  filter(word1 %in% love_words | word2 %in% love_words) %>%  
  count(word1, word2, sort = TRUE)


# 4TH TAB - SENTIMENT ANALYSIS

# sentiment by volume and singer

# read in singers for each song
singers <- read_rds("69LoveSongs_Singers.rds")

# add onto tidy data
lovesongs_tidy_singer <- lovesongs_tidy %>% 
  inner_join(singers, by = "song_number")

# attach positive or negative sentiment to each word
bing_sentiment <- lovesongs_tidy_singer %>% 
  left_join(get_sentiments("bing"), by = "word")

# get positive and negative percentage for each song
# keep songs with at least 20 distinct words
song_sentiment <- bing_sentiment %>% 
  group_by(song, disc_number, disc_track_no, singer) %>% 
  summarise(Positive = sum(sentiment == "positive", na.rm = TRUE) / n(),
            Negative = sum(sentiment == "negative", na.rm = TRUE) / n(),
            distinct_words = n_distinct(word),
            all_words = n()) %>% 
  filter(distinct_words >= 20) %>% 
  ungroup()

# top 10 positive sentiment % and top 10 negative sentiment %
song_sentiment_tidy <- song_sentiment %>% 
  gather(sentiment, perc, -c(1:4, 7:8)) %>% 
  group_by(sentiment) %>% 
  arrange(desc(perc)) %>% 
  filter(row_number() <= 10) %>% 
  ungroup() %>% 
  arrange(sentiment, perc) %>% 
  mutate(order = row_number(),
         perc = round(perc, 3),
         disc_number = as.factor(disc_number),
         singer = fct_relevel(singer, c("Stephin Merritt")),
         singer = fct_relevel(singer, c("Merritt, Beghtol"), after = Inf))


# LAST TAB - Making me blue dataset
blueshades <- tibble(colourname = c("Pantone 292", "Crayola Blue", "Liberty", "Space Cadet", "Teal", "Ultramarine"),
                     colourhex = c("#62A8E5", "#1F75FE", "#545AA7", "#1D2951", "#008080", "#4000FF"))



# SHINY UI
ui <- navbarPage(inverse = TRUE, "Controversied on Reddit",
                 
                 # First Page - Intro        
                 tabPanel("Intro", includeCSS("styles.css"),
                          fluidPage(h1("Reddit Controversies: Dave Chapelle vs Joe Manchin"),
                                    br(),
                                    h3(strong(b("A Sentiment Analysis of Reddit-user Response to Current Controversies"))),
                                    br(),
                                    h5(em("Public Disagreements")),
                                      p("- On October 5th, 2021, Netflix premiered"),
                                        a("Dave Chapelle", href="https://en.wikipedia.org/wiki/Dave_Chappelle"),
                                        "'s hour-long stand-up comedy show special",
                                        a("'The Closer' (2021)", href="https://en.wikipedia.org/wiki/The_Closer_(2021_film)"),
                                        "on it's streaming platform. Within a few days of it's release, viewers of all backgrounds became vocally critical of offensive remarks Chappelle made in the show. In an escalating battle for Netflix to discontinue streaming Chappelle's special, Netflix employee's have been protesting with political action in various forms. These include staging a walkout, leaking profitability data to Bloomberg News, and filing a federal labor charge", 
                                      p("- Over the past month, senators in Congress have been voting and negotiating on an infrastructure bill that's part of POTUS Joe Biden's",
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
                                    div(a(img(src = "wall-e.jpg", height = 461.85, width = 800), style="text-align: center;"), href="https://unsplash.com/photos/Sot0f3hQQ4Y?utm_source=unsplash&utm_medium=referral&utm_content=creditShareLink"),
                                    br(),
                                    br(),
                                    br(),
                                    div(p(strong("Built by"), a("Hadar Zeigerson", href = "https://nycdatascience.com/blog/author/hzeigersongmail-com/"), "using the power of Rstudio and Shiny."), 
                                        p(strong("R Packages:"), "RedditExtractoR, sentimentr, tidyverse, dplyr, ggplot2, wordcloud2."),
                                        p(strong("Sources:"), a("reddit.com", href = "https://www.reddit.com/"), "for data on the", a("Dave Chappelle & Netflix", href = "https://www.reddit.com/search/?q=dave%20chappelle%20netflix%20trans"), "and", a("Joe Manchin", href = "https://www.reddit.com/search/?q=joe%20manchin%20climate%20bill"), "controversies."),
                                        style="text-align: right;")
                          )
                 ),
                 
                 # Second Page  - Overall Comparison      
                 tabPanel("Overview",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("text",
                                                                                     "Text:",
                                                                                     choices = 1:3,
                                                                                     selected = 1)),
                                                               wellPanel(style = "background: white",
                                                                         selectInput("plot",
                                                                                     "Plot type:",
                                                                                     choices = 1:3,
                                                                                     selected = 1)             
                                                               # wellPanel(style = "background: white",
                                                               #           h3("Info:"),
                                                               #           textOutput("lovecount_desc"),
                                                               #           br(),
                                                               #           p("The 'loveliest' songs are annotated.")),
                                                               )
                                                  ),
                                                  
                                                  mainPanel(
                                                    p(strong(em("\"How Fucking Romantic, must we really waltz?\""), "1.14 - How Fucking Romantic")),
                                                    br(),
                                                    p("But just ", em("how")," romantic is 69 Love Songs? Let's measure this (crudely) by the number of 'loves' in each song:"),
                                                    br(),
                                                    plotOutput("lovecountPlot", width = "100%")
                                                  )
                          )
                          )
                 ),
                 
                 
                 # Network diagram - love bigrams
                 tabPanel("Love, or Nothing At All",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("love_list",
                                                                                            "How much Love?:",
                                                                                            choices = love_words,
                                                                                            selected = "love",
                                                                                            inline = TRUE)),
                                                               wellPanel(style = "background: white",
                                                                         h4("Info:"),
                                                                         p("The words appearing before or after the word 'love' (and variants if chosen above), are shown in this network."),
                                                                         p("Words appearing after 'love' have a red line, and words appearing before 'love' have a black line.")),
                                                               wellPanel(style = "background: white",
                                                                         h4("Interact:"),
                                                                         p("Zoom in, drag, hover and select nodes to reveal the strength of the connection."),
                                                                         p("For example, common combinations such as 'in love' and 'love you' have thicker lines."))),
                                                  
                                                  mainPanel( 
                                                    p(strong(em("\"I'm for free love, and I'm in free fall. This could be love or nothing at all.\""), "1.4 - A Chicken With Its Head Cut Off")),
                                                    br(),
                                                    p("Let's put all this love into context. Explore the network of love below:"),
                                                    visNetworkOutput("lovenetwork", width = "100%", height = "565px")
                                                  )
                          )
                          )
                 ),
                 
                 # Sentiment analysis by disc and singer
                 tabPanel("Love and Trouble",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("sent_fill",
                                                                                     "Colour bars by:",
                                                                                     choices = c("Volume" = "disc_number", "Singer" = "singer"),
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
                 ),
                 
                 # Third Page - Clouds        
                 tabPanel("Wordclouds",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(style = "background: #FF5700",
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("disc_cloud",
                                                                                            "Choose Controversial Topic:",
                                                                                            choices = c("Dave Chappelle, Netflix, 'The Closer'","Joe Manchin, Build Back Better"),
                                                                                            selected = c("Dave Chappelle, Netflix, 'The Closer'","Joe Manchin, Build Back Better"))),
                                                                         selectInput("disc_cloud",
                                                                           "Choose subreddit(s)",
                                                                           choices = ,
                                                                           selected = NULL,
                                                                           multiple = FALSE,
                                                                           selectize = TRUE,
                                                                           width = NULL,
                                                                           size = NULL
                                                                         ),
                                                                              ),
                                                            
                                                  mainPanel( 
                                                    p("Hover over the word cloud below, or search for the count of word appearances in the table (bottom left):"),
                                                    wordcloud2Output("wordcloud", width = "100%", height = "565px")
                                                  )
                          )
                          )
                 ),
),


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
                hjust = 1.1, family = "Courier") +
      geom_text(data = commentary_subset(), aes(label = comm, colour = disc_colour), 
                size = 4, hjust = 1, family = "Courier", show.legend = FALSE) +
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
      theme(text = element_text(family = "Courier"),
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
    req(input$disc)
    lovesongs_tidy %>% 
      filter(disc_number %in% input$disc_cloud) %>%
      select(word) %>% 
      anti_join(stop_words, by = "word") %>%
      #filter(word != "love") %>% 
      count(word, sort = TRUE) 
  }) 
  
  # Word Clouds
  output$wordcloud <- renderWordcloud2({
    wordcloud2(word_counts(), size = 1.6, fontFamily = "Courier",
               color=rep_len(pal[1:3], nrow(word_counts())), backgroundColor = "white")
  })
  
  
  # Love Network
  
  love_graph_data <- reactive({
    req(input$love_list)
    bigram_separated_love %>%
      filter(word1 %in% input$love_list | word2 %in% input$love_list) %>% 
      as_tbl_graph() %>% 
      mutate(color.background = if_else(name %in% input$love_list, "#E00008", "black"),
             color.border = if_else(name %in% input$love_list, "#E00008", "black"),
             label = name,
             labelHighlightBold = TRUE,
             #size = if_else(name == "love", 70, 25),
             font.face = "Courier",
             font.size = if_else(name == "love", 70, 40),
             font.color = if_else(name %in% input$love_list, "#E00008", "black"),
             shape = if_else(name %in% input$love_list, "icon", "dot"),
             icon.face = "FontAwesome",
             icon.code = "f004",
             icon.size = if_else(name == "love", 200, 100),
             icon.color = if_else(name %in% input$love_list, "#E00008", "black")) %>% 
      activate(edges) %>% 
      mutate(hoverWidth = n,
             selectionWidth = n,
             scaling.max = 20)
    
  }) 
  
  
  output$lovenetwork <- renderVisNetwork({
    
    visIgraph(love_graph_data()) %>% 
      visInteraction(hover = TRUE, tooltipDelay = 0) %>% 
      addFontAwesome()
    
  })
  
  
  # sentiment plot
  sentiment_pal <- c("black", "#E00008", "#858B8E", "#62A8E5", "#4000FF", "#1D2951")
  
  output$sentimentPlot <- renderPlot({
    ggplot(song_sentiment_tidy, aes(x = order, y = perc)) +
      geom_col(aes_string(fill = input$sent_fill)) + 
      geom_text(aes(label = song, y = 0.48), hjust = 1,
                family = "Courier") +
      geom_text(aes(label = scales::percent(perc, accuracy = .1)), hjust = 1.1,
                family = "Courier", colour = "white") +
      coord_flip() +
      facet_wrap(~ sentiment, scales = "free_y") +
      scale_x_continuous(breaks = song_sentiment_tidy$order,
                         expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1),
                         limits = c(0,0.5)) +
      scale_fill_manual(values = sentiment_pal) +
      labs(y = "\n% of Positive/Negative Sentiment", fill = NULL) +
      theme_minimal() +
      theme(text = element_text(family = "Courier"),
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
      geom_text(aes(label = label, x = 5, y = 5), family = "Courier", size = 8,
                colour = "white", show.legend = FALSE) +
      scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
      scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
      theme_void() +
      theme(plot.background = element_rect(fill = blueshade()$colourhex, colour = blueshade()$colourhex))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

