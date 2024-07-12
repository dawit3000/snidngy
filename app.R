source("my_helpers.R")  # Includes necessary helper functions

# Define UI for application
ui <- navbarPage(
  title = "Most Probable Next Word(s) Predictor",
  tabPanel("Prediction App",
           sidebarLayout(
             sidebarPanel(
               textInput("ngramInput", "Enter Ngram (e.g., 'takes two' or 'happy mother day to'):", value = "takes two"),
               actionButton("plotButton", "Click to Render"),
               
               p(" "),
               
               p("* =======   ***   ======== *"),
               tags$u(strong(p("Disclaimer"))),
               p("This app was trained on a curated corpora sourced from Blogs, Twitter, and News sources, totaling less than 55 MB due to server restriction. "),
             ),
             mainPanel(
               h3("Probability Table for the Next Word"),
               tableOutput("frequencyTable"), 
               plotOutput("ngramPlot")
             )
           )
  ),
  tabPanel("Documentation", includeMarkdown("readme.md"),
  ),
  
  tabPanel("Pitch Presentation", includeMarkdown("Pitch.md"),
  ),
  
  tabPanel(
    "Millstone",
    tags$a(href = "https://rpubs.com/dawit3000/A_Prelude_to_Text_Mining", "Click here for A Prelude to Text Mining")
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$plotButton, {
    ngram <- input$ngramInput
    
    # Clean and process input
    ## ngram <- tolower(ngram)
    ## ngram <- removeWords(ngram, stopwords("english")) # Removing stopwords causes crashes
    ## ngram <- stemDocument(ngram)
    ## ngram <- removePunctuation(ngram)            
    ## ngram <- stripWhitespace(ngram)
    ## ngram <- removeNumbers(ngram)  # Removing numbers causes crashes
    
    # Split the user-inserted ngram into words
    words <- unlist(strsplit(ngram, " "))
    
    # Initialize output data
    output_data <- NULL
    
    # logic for handling (for all n = 1, 2, ...)
    if (length(words) == 1) {
      ngram <- words
      output_data <- outputBigram(ngram)[, c(4, 2)]
      if (nrow(output_data) == 0) {
        output_data <- most_frequent_words  # Use default words for demonstration
      }
    } else if (length(words) == 2) {
      ngram <- paste(words[(length(words) - 1):length(words)], collapse = " ")
      output_data <- outputTrigram(ngram)[, c(4, 2)]  # Adjust columns as per the data structure
      if (nrow(output_data) == 0) {
        ngram <- words[2]
        output_data <- outputBigram(ngram)[, c(4, 2)]  # Adjust columns as per the data structure
        if (nrow(output_data) == 0) {
          output_data <- most_frequent_words  # Use default words for demonstration
        }
      }
    } else if (length(words) == 3) {
      ngram <- paste(words[(length(words) - 2):length(words)], collapse = " ")
      output_data <- outputFourgram(ngram)[, c(4, 2)]
      if (nrow(output_data) == 0) {
        ngram <- paste(words[(length(words) - 1):length(words)], collapse = " ")
        output_data <- outputTrigram(ngram)[, c(4, 2)]
        if (nrow(output_data) == 0) {
          ngram <- words[length(words)]
          output_data <- outputBigram(ngram)[, c(4, 2)]
          if (nrow(output_data) == 0) {
            output_data <- most_frequent_words  
          }
        }
      }
    } else if (length(words) == 4) {
      ngram <- paste(words[(length(words) - 3):length(words)], collapse = " ")
      output_data <- outputFivegram(ngram)[, c(4, 2)]
      if (nrow(output_data) == 0) {
        ngram <- paste(words[(length(words) - 2):length(words)], collapse = " ")
        output_data <- outputFourgram(ngram)[, c(4, 2)]
        if (nrow(output_data) == 0) {
          ngram <- paste(words[(length(words) - 1):length(words)], collapse = " ")
          output_data <- outputTrigram(ngram)[, c(4, 2)]
          if (nrow(output_data) == 0) {
            ngram <- words[length(words)]
            output_data <- outputBigram(ngram)[, c(4, 2)]
            if (nrow(output_data) == 0) {
              output_data <- most_frequent_words  
            }
          }
        }
      }
    } else if (length(words) >= 5) {
      ngram <- paste(words[(length(words) - 4):length(words)], collapse = " ")
      output_data <- outputSixegram(ngram)[, c(4, 2)]
      if (nrow(output_data) == 0) {
        ngram <- paste(words[(length(words) - 3):length(words)], collapse = " ")
        output_data <- outputFivegram(ngram)[, c(4, 2)]
        if (nrow(output_data) == 0) {
          ngram <- paste(words[(length(words) - 2):length(words)], collapse = " ")
          output_data <- outputFourgram(ngram)[, c(4, 2)]
          if (nrow(output_data) == 0) {
            ngram <- paste(words[(length(words) - 1):length(words)], collapse = " ")
            output_data <- outputTrigram(ngram)[, c(4, 2)]
            if (nrow(output_data) == 0) {
              ngram <- words[length(words)]
              output_data <- outputBigram(ngram)[, c(4, 2)]
              if (nrow(output_data) == 0) {
                output_data <- most_frequent_words  
              }
            }
          }
        }
      }
    }
    
    # Sort and prepare data for frequency/probability table
    output_data$frequency <- output_data$frequency / sum(output_data$frequency)
    output_data <- output_data[order(output_data$frequency, decreasing = TRUE), ]
    ## output_data <- arrange(output_data, desc(frequency))
    most_frequent_words <- most_frequent_words[order(most_frequent_words$frequency, decreasing = TRUE), ]
    ## most_frequent_words <- arrange(most_frequent_words, desc(frequency))   
    names(output_data)[2] <- "Probability"  # Rename the second column
    
    # Render Probability table
    output$frequencyTable <- renderTable({
      head(output_data, 10)
    }, digits = 4)
    
    # Render the plot
    output$ngramPlot <- renderPlot({
      output_data <- head(output_data, 10)  # Show top 10 results in the plot
      output_data$nextword <- factor(output_data$nextword, levels = unique(as.character(output_data$nextword)))  # Controls order of bars
      barchart(Probability ~ nextword, data = output_data,
               main = "Most Probable Next Words Sorted by Decreasing Probability Order",
               xlab = "Most Probable Nextword",
               scales = list(x = list(rot = 45)),
               ylab = "Probability of Next Word",
               col = rainbow(10))
    }) 
  })
}

# Run the application
shinyApp(ui = ui, server = server)
