library(rvest)
library(tm)
library(SnowballC)
library(hunspell)
library(openxlsx)
library(textclean)


urls <- c(
  "https://abcnews.go.com/International/trump-putin-stop-bloodbath-ukraine-peace-call/story?id=121939395",
  "https://edition.cnn.com/world/live-news/ukraine-russia-talks-istanbul-turkey-intl",
  "https://abcnews.go.com/US/wireStory/us-military-spent-6-billion-past-3-years-122174949",
  "https://abcnews.go.com/Technology/wireStory/half-us-states-now-laws-banning-regulating-cellphones-122016681",
  "https://abcnews.go.com/International/gaza-pediatrician-mother-loses-9-10-children-after/story?id=122140789"
)

selectors_list <- list(
  abcnews = list(
    title = c("h1", ".ArticleHeader__Headline", "[data-module='ArticleHeader'] h1", ".headline"),
    date = c(".JQYD", ".Byline__Meta", ".timestamp", "[data-module='ArticleHeader'] time", ".date"),
    author = c(".whbOj", ".Byline__Author", ".byline", "[data-module='ArticleHeader'] .author", ".author"),
    content = c(".gFyyv p", ".ArticleBody p", ".RichTextArticleBody p", "[data-module='ArticleBody'] p", "article p", ".story-body p", "p")
  ),
  cnn = list(
    title = c("h1", ".pg-headline", ".article__title", ".headline"),
    date = c(".timestamp", ".update-time", ".article__meta time", ".byline__timestamp"),
    author = c(".byline__name", ".metadata__byline", ".article__byline", ".author"),
    content = c(".live-story-content p", ".zn-body__paragraph", "article p", ".article__content p", "p")
  )
)

scrape_website <- function(url, selectors) {
  tryCatch({
    webpage <- read_html(url)
    
    try_selectors <- function(webpage, selector_list, element_name) {
      for (selector in selector_list) {
        result <- html_nodes(webpage, selector) %>% html_text() %>% paste(collapse = " ")
        if (nchar(trimws(result)) > 0) {
          print(paste("Found", element_name, "using selector:", selector))
          return(trimws(result))
        }
      }
      print(paste("No", element_name, "found with any selector"))
      return("")
    }
    
    title <- try_selectors(webpage, selectors$title, "title")
    date <- try_selectors(webpage, selectors$date, "date")
    author <- try_selectors(webpage, selectors$author, "author")
    content <- try_selectors(webpage, selectors$content, "content")
    
    if (nchar(content) == 0) {
      all_paragraphs <- html_nodes(webpage, "p") %>% html_text()
      meaningful_paragraphs <- all_paragraphs[nchar(all_paragraphs) > 20]
      content <- paste(meaningful_paragraphs, collapse = " ")
      
      if (nchar(content) == 0) {
        all_divs <- html_nodes(webpage, "div") %>% html_text()
        meaningful_divs <- all_divs[nchar(all_divs) > 50 & nchar(all_divs) < 1000]
        content <- paste(meaningful_divs[1:3], collapse = " ")
      }
    }
    
    print(paste("Content length:", nchar(content), "characters"))
    return(list(title = title, date = date, author = author, content = content))
  }, error = function(e) {
    message(paste("Error scraping", url, ":", e$message))
    return(list(title = "", date = "", author = "", content = ""))
  })
}

process_text <- function(text) {
  if (length(text) == 0 || all(text == "") || all(nchar(text) == 0)) {
    return(list(original = character(0), 
                processed = character(0),
                tokens = list(),
                corrected = list(),
                filtered = list(),
                stemmed = list()))
  }
  
  contractions <- replace_contraction(text)
  
  processed_text <- text
  for (contr in names(contractions)) {
    processed_text <- gsub(paste0("\\b", contr, "\\b"), contractions[contr], processed_text, ignore.case = TRUE)
  }
  
  processed_text <- gsub("<.*?>", "", processed_text)
  processed_text <- tolower(processed_text)
  processed_text <- gsub("[^a-z ]", "", processed_text)
  processed_text <- gsub("\\s+", " ", processed_text)
  processed_text <- trimws(processed_text)
  
  tokens <- strsplit(processed_text, " ")
  
  corrected_tokens <- lapply(tokens, function(words) {
    if (length(words) == 0) return(character(0))
    
    corrected_words <- words
    tryCatch({
      misspelled <- hunspell_check(words)
      
      for (i in seq_along(words)) {
        if (!is.na(misspelled[i]) && !misspelled[i]) {
          suggestions <- hunspell_suggest(words[i])[[1]]
          if (length(suggestions) > 0) {
            corrected_words[i] <- suggestions[1]
          }
        }
      }
    }, error = function(e) {
      message(paste("Error in spell checking:", e$message))
    })
    
    return(corrected_words)
  })
  
  stop_words <- stopwords("en")
  filtered_tokens <- lapply(corrected_tokens, function(x) {
    if (length(x) == 0) return(character(0))
    return(x[!x %in% stop_words])
  })
  
  stemmed_tokens <- lapply(filtered_tokens, function(x) {
    if (length(x) == 0) return(character(0))
    return(wordStem(x, language = "english"))
  })
  
  return(list(
    original = text,
    processed = processed_text,
    tokens = tokens,
    corrected = corrected_tokens,
    filtered = filtered_tokens,
    stemmed = stemmed_tokens
  ))
}

all_results <- list()

scraped_data <- data.frame(
  URL = character(),
  Title = character(),
  Date = character(),
  Author = character(),
  Content = character(),
  Content_Length = numeric(),
  Stemmed_Tokens = character(),
  stringsAsFactors = FALSE
)

for (i in 1:length(urls)) {
  print(paste("Processing URL", i, ":", urls[i]))
  
  website <- if (grepl("abcnews", urls[i])) "abcnews"
  else if (grepl("cnn", urls[i])) "cnn"
  else stop(paste("Unknown website for URL:", urls[i]))
  
  selectors <- selectors_list[[website]]
  
  scraped <- scrape_website(urls[i], selectors)
  
  print(paste("Title:", scraped$title))
  print(paste("Date:", scraped$date))
  print(paste("Author:", scraped$author))
  print(paste("Content (first 200 chars):", substr(scraped$content, 1, 200)))
  
  if (nchar(scraped$content) > 0) {
    results <- process_text(scraped$content)
    all_results[[i]] <- results
    stemmed_tokens <- paste(unlist(results$stemmed), collapse = ", ")
  } else {
    results <- list(stemmed = list())
    all_results[[i]] <- results
    stemmed_tokens <- ""
  }
  
  scraped_data <- rbind(scraped_data, data.frame(
    URL = urls[i],
    Title = scraped$title,
    Date = scraped$date,
    Author = scraped$author,
    Content = scraped$content,
    Content_Length = nchar(scraped$content),
    Stemmed_Tokens = stemmed_tokens,
    stringsAsFactors = FALSE
  ))
  
  if (length(unlist(results$stemmed)) > 0) {
    print("Final stemmed tokens (first 10):")
    print(head(unlist(results$stemmed), 10))
  } else {
    print("No stemmed tokens generated (no content found)")
  }
}

tryCatch({
  write.xlsx(scraped_data, "D:/news_data_data.xlsx", rowNames = FALSE)
}, error = function(e) {
  print(paste("Error writing to Excel:", e$message))
  print("Trying alternative path...")
  write.xlsx(scraped_data, "scraped_news_data.xlsx", rowNames = FALSE)
  
})

