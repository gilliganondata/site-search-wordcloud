#
# App to generate a word cloud of site search terms. The user has to authenticate
# and choose a view. This is the Shiny-fication of some code created by Sébastien 
# Brodeur, which he demo'd at Superweek Hungary 2017. See his original post/code at:
# https://www.linkedin.com/pulse/using-r-get-real-insights-from-your-searched-terms-words-brodeur
#

library(shiny)              # We must web-enable this whole thing
library(DT)                 # For the frequency table display

# There's a wrinkle for running this on shinyapps.io that requires using the Github
# version of googleAuthR. I don't know if this is actually still an issue or not, 
# actually. See: https://twitter.com/ryanpraski/status/783754506681155584
library(googleAuthR)        # To prompt for authentication by the user

library(googleAnalyticsR)   # For the pulling of the data
library(tidyverse)          # For data transformations -- primarily just uses dplyr commands

# The libraries needed for working with the text.
library("tm")
library("SnowballC")
library("wordcloud")

# Uncomment the below and hardcode your credentials (available from the 
# Google Developer Console) if needed -- if the "Login" isn't working and
# you're getting errors (you will also need to set up the origin for the
# credentials for your app to match). I'm doing that crappy thing here where the
# comments aren't *really* providing sufficient detail, but that's largely
# because I'm fuzzy on what exactly needs to be done, when, and why.
# options("googleAuthR.webapp.client_id" = "[GOOGLE APP CLIENT ID]")
# options("googleAuthR.webapp.client_secret" = "[GOOGLE APP CLIENT SECRET]")

####################
# Set up the different options for interaction
####################

# DATE OPTIONS
# This could also be set as date selectors easily enough, but, for now, it's just set
# as some preset options. As a note, even though the values are being set as numerics 
# here, they actually get treated as characters, so they have to be converted 
# back to numerics when setting start_date in the get_base_data() function.
daterange_options <- list("Last 7 Days" = 7,
                          "Last 30 Days" = 30,
                          "Last 60 Days" = 60,
                          "Last 90 Days" = 90)

######################
# Define the UI
######################

ui <- fluidPage(
  
  # Include Google Tag Manager container (put the file in the same
  # folder as app.R). Remove the <script> tags and the comments from the GTM script.
  tags$head(includeScript("gtm.js")),
  
  theme = "cosmo",   # Change to a different theme to slightly alter the look and feel as desired
  
  # Title and Overview
  titlePanel("Google Analytics Site Search Term Viewer"),
  "Log in and select a view to explore the individual search terms and how often they",
  "get used on your site. This effort was inspired by (and cribbed from) work initially",
  "created by ", tags$a(href="https://www.linkedin.com/pulse/using-r-get-real-insights-from-your-searched-terms-words-brodeur",
                        target="_blank","Sébastien Brodeur"),
  ", with some additional inspiration from ",
  tags$a(href="https://nanalytics.wordpress.com/2014/07/14/who-what-where-when-why-how-harnessing-the-power-of-internal-site-search/",
         target="_blank","Nancy Koons"),". The source code for this application is available",
  tags$a(href="https://github.com/gilliganondata/site-search-wordcloud", target="_blank", "on Github"),".",
  
  tags$hr(),
  
  # Sidebar with the user-controllable inputs 
  sidebarLayout(
    sidebarPanel(
      
      # A bit of a hack, I suspect, but just centering the login button. And. the log OUT button
      # still winds up not centered. :-(
      tags$div(style="text-align: center;",
               
               # Get the user to log in and then select a view. This uses the JS version
               # because it seems to be less finicky.
               gar_auth_jsUI("auth_module", login_text = "Login with Google")
      ),
      
      # Get the account/property/view
      authDropdownUI("auth_menu"),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # The date range dropdown, including a default value
      selectInput("daterange", label = "Select a date range:", 
                  choices = daterange_options, 
                  selected = 30),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # Select the minimum number of occurrences to include
      sliderInput("min_occurrences",
                  label = "Adjust the minimum number of occurrences to include in the word cloud:",
                  min = 1,
                  max = 10,
                  value = 3),
      
      # Slider setting
      verbatimTextOutput("slider_setting"),
      
      # Terms to remove. This is so a non-stopword (like the the brand name) can
      # be entered and then removed from the wordcloud so it doesn't dominate.
      textInput("exclude_terms", label = paste("Enter (stemmed) terms separated by commas",
                 "to remove them from the word cloud and frequency table."), 
                value = NULL)
    ),
    
    # All of the actual output. This is just four tabs, which could easily be added onto if
    # other ways of exploring this same data are conceived. The bulk of the "code" for this
    # is just getting the descriptive text into each tab.
    mainPanel(
      tags$h4("Site Search Term Usage"),
      
      tabsetPanel(
        tabPanel("Word Cloud", tags$br(), "To remove specific terms from the word cloud, enter them in the",
                 "box in the sidebar to the left (this will remove the words from both the word cloud ",
                 "and the ", tags$strong("Frequency Table"), ".", tags$br(), " ",
                 plotOutput("word_cloud")),
        tabPanel("Frequency Table", tags$br(),
                 "The table below shows the frequency of each individual keyword (stemmed) ",
                 "that was included in searches. To remove specific terms from the frequency table, enter",
                 "them in the box in the sidebar to the left (this will remove the words from both the",
                 "frequency table and the", tags$strong("Word Cloud"), ".", tags$p(" "),
                 DT::dataTableOutput("freq_table")),
        tabPanel("Questions in Search", tags$br(),
                 "The table below shows (generally long-tail) searches that were likely ",
                 "specific questions being asked in the search box. These can provide insight ",
                 "as to very specific things for which visitors are looking for content. This ",
                 "way of exploring search data comes straight from ",
                 tags$a(href="https://nanalytics.wordpress.com/2014/07/14/who-what-where-when-why-how-harnessing-the-power-of-internal-site-search/",
                        target="_blank","Nancy Koons"), ".", tags$p(" "),
                 DT::dataTableOutput("question_searches")),
        tabPanel("Raw Google Analytics Results", tags$br(),
                 "The table below shows the search data as it was pulled from Google Analytics. ",
                 "This would match what you see if you view the ", tags$strong("Search Terms"), " report for the ",
                 "same view and same timeframe in Google Analytics.", tags$p(" "),
                 DT::dataTableOutput("raw_data"))
      )
    )
  )
)

################
# Define server logic
################

server <- function(input, output) {
  
  # Get the view ID (user-selected). I'd be lying if I said I fully understood this
  # piece -- pretty much lifted it straight from Mark Edmondson's example at:
  # http://code.markedmondson.me/googleAnalyticsR/shiny.html. Except... used the
  # JS option: https://mark.shinyapps.io/googleAuthRMarkdown/
  access_token <- callModule(gar_auth_js, "auth_module")
  
  # Get the accounts list
  ga_account <- reactive({
    validate(
      need(access_token(), "Authenticate")
    )
    with_shiny(google_analytics_account_list, shiny_access_token = access_token())
  })
  
  view_id <- callModule(authDropdown, "auth_menu", ga.table = ga_account)
  
  # Reactive function to actually pull the data. This will get run if
  # the view is changed or if the date range is changed.
  get_base_data <- reactive({
    
    # Calculate the start and end dates.
    start_date <- as.character(Sys.Date()-as.numeric(input$daterange)-1)
    end_date <- as.character(Sys.Date()-1)
    
    # Pull the data. Note this limits the results to 10000 rows. You
    # can probably fiddle around with that if need be -- possible just
    # set max = -1 and see if it borks on you.
    ga_data <- with_shiny(google_analytics_4,
                          viewId = view_id(),
                          date_range = c(start_date,end_date),
                          metrics = "searchUniques",
                          dimensions = "searchKeyword",
                          order = order_type("searchUniques", "DESCENDING", "VALUE"),
                          anti_sample = TRUE,
                          max = 10000,
                          shiny_access_token = access_token())
  })
  
  ############################
  # Create a term document matrix for the data. This is split up from the 
  # function that actually creates the final frequency table so that it 
  # doesn't need to get re-processed every time the user updates exclusion terms.
  create_term_doc_matrix <- reactive({
    
    # Get the data
    wrdcld_data <- get_base_data()
    
    #Convert UTF-8 to ASCII (needed because tm doesn't seem to like UTF-8 accented character)
    wrdcld_data$searchKeyword <- iconv(wrdcld_data$searchKeyword, "UTF-8", "ASCII") 
    
    # Repeat keyword by number of searches
    #  "A", 3
    #  "B", 2
    #  "C", 1
    # Becomes:
    #  "A"
    #  "A"
    #  "A"
    #  "B"
    #  "B"
    #  "C"
    wrdcld_data <- data.frame(searchKeyword = rep(wrdcld_data$searchKeyword, 
                                                  wrdcld_data$searchUniques))
    
    # IF the resulting data set has more than 10,000 rows, then pull a sample of
    # 10,000 rows to keep from running into memory limitation issues.
    if(nrow(wrdcld_data) > 10000){
      wrdcld_data_sample <- na.omit(as.data.frame(wrdcld_data$'searchKeyword'[sample(1:nrow(wrdcld_data), 10000)]))
    } else {
      wrdcld_data_sample <- wrdcld_data
    }
    
    # Create a corpus and try to reduce the corpus by transformation.
    wrdcld_data_corpus <- Corpus(DataframeSource(data.frame(as.character(wrdcld_data_sample$'searchKeyword'))))
    
    # Make all the characters lowercase
    wrdcld_data_corpus <- tm_map(wrdcld_data_corpus, content_transformer(tolower))
    
    # Remove stopwords: a, the, as, etc.
    wrdcld_data_corpus <- tm_map(wrdcld_data_corpus, function(x) removeWords(x, stopwords("english")))
    
    # Stem words: comptes + compte = compt
    wrdcld_data_corpus <- tm_map(wrdcld_data_corpus, stemDocument, language = "english") 
    
    # Remove any punctuation
    wrdcld_data_corpus <- tm_map(wrdcld_data_corpus, removePunctuation)
    
    # Create a Term Document Matrix
    wrdcld_data_tdm <- TermDocumentMatrix(wrdcld_data_corpus)
    
  })

  ############################
  # Do all of the manipulation to get to a frequency table that can be
  # used to both generate the word cloud and to generate the frequency table.
  create_freq_table <- reactive({
    
    # Get the full term document matrix
    term_doc_matrix <- create_term_doc_matrix()
    
    # Create the actual frequency table
    wrdcld_data_m <- as.matrix(term_doc_matrix)
    wrdcld_data_v <- sort(rowSums(wrdcld_data_m), decreasing=TRUE)
    wrdcld_data_d <- data.frame(word = names(wrdcld_data_v), freq=wrdcld_data_v)
    
    # Remove any of the exclusion terms that are entered.
    if(!is.null(input$exclude_terms)){
      # Take the comma-delimited list of terms and split them out to be a
      # character vector. The ", ?" regEx is so that this will work with
      # or without a space following the comma
      remove_terms <- unlist(strsplit(input$exclude_terms,", ?"))
      
      # Drop the rows from wrdcld_data_d that match those terms
      wrdcld_data_d <- filter(wrdcld_data_d, !word %in% remove_terms)
    }
  })
  
  ############################
  # Build the word cloud
  output$word_cloud <- renderPlot({
    
    # Make sure an access token is present before trying to render anything
    req(access_token())
    
    # Set seed. As I understand it, this will just mean that the same data set will
    # generate the same word cloud -- exactly -- every time. It sorta' sets a base
    # point for "random" for the randomizing aspects of word cloud generation.
    set.seed("12345")
    
    # Get the data and generate the word cloud
    freq_tbl_data <- create_freq_table()
    
    # Set the color palette to use
    pal2 <- rev(brewer.pal(8,"Spectral")) 
    
    # Generate the word cloud
    wordcloud(freq_tbl_data$word,freq_tbl_data$freq, 
              scale=c(5.5,0.6),
              min.freq=input$min_occurrences,
              max.words=500, 
              random.order=FALSE,
              rot.per=.0,
              colors=pal2)
  })
  
  ############################
  # Build the output for the frequency table
  output$freq_table <- DT::renderDataTable({
    
    freq_table <- create_freq_table()
    
    # Rename the column headings
    colnames(freq_table) <- c("(Stemmed) Term", "Unique Searches")
    
    # Repeat the actual table so we don't just output the column names
    freq_table
    
    },
    rownames = FALSE)
  
  ############################
  # Output a filtered view of the base data limited to searches that
  # include "specific question" words. The regEx could probably be a bit
  # cleaner, but it's just trying to minimize false positives and false negatives.
  output$question_searches <- DT::renderDataTable({
    
    ga_data <- get_base_data() %>% 
      filter(grepl("(?i)(^(who|what|why|where|how) )|( (who|what|why|where|how) )", searchKeyword))
    
    # Rename the column headings
    colnames(ga_data) <- c("Question-Like Searches","Unique Searches")
    
    # Repeat the actual table so we don't just output the column names
    ga_data
    
  },
  rownames = FALSE)

  ############################
  # Output the base data
  output$raw_data <- DT::renderDataTable({
    
    ga_data <- get_base_data()
    
    # Rename the column headings
    colnames(ga_data) <- c("Search Term","Unique Searches")
    
    # Repeat the actual table so we don't just output the column names
    ga_data
    
  },
  rownames = FALSE)
  }

############################
# Run the application 
shinyApp(ui = ui, server = server)