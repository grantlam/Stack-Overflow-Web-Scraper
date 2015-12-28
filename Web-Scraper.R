library(RCurl)
library(XML)
library(plyr)

setwd("C:/Users/Grant/Documents/UC Davis/STA 141/Assignment 6")

baseUrl <- "http://stackoverflow.com"
trimXMLValue <- function(x) xmlValue(x, trim = TRUE)
setMissingAsNA <- function(x) {
  if (length(x) == 0) ans <- NA
  else ans <- x[[1]]
}

rdoc <- getURLContent("http://stackoverflow.com/questions/tagged/r?sort=newest&pagesize=50")
rhtml <- htmlParse(rdoc, asText = TRUE)

# Scrapes a page of Stack Overflow with a given tag (ie. r),
# ex. http://stackoverflow.com/questions/tagged/r?sort=newest&pageSize=50
#
# Args:
#   page: the page to be scraped, once it has been htmlParsed
#
# Returns:
#   A dataframe containing the summary for the page,
#   which has:
#     id, date, tags, title, url
#     views, votes, answers, user, reputation
scrapePage <- function(page) {
  post <- getNodeSet(page, "//div[@class = 'question-summary']")
  
  scrapePost <- function(post) {
    getId <- function(post) {
      id <- getNodeSet(post, "./@id")
      id <- setMissingAsNA(id)
      id <- as.character(gsub("\\D", "", id))
    }
    
    getDate <- function(post) {
      date <- getNodeSet(post, ".//span[@class = 'relativetime']/@title")
      date <- as.character(unlist(sapply(date, "[[", 1)))
    }
    
    getTags <- function(post) {
      tags <- getNodeSet(post, ".//div[contains(@class, 'tags ')]")
      tags <- sapply(tags, trimXMLValue)
      tags <- setMissingAsNA(tags)
      tags <- as.character(gsub(" ", "; ", tags))
    }
    
    getTitle <- function(post) {
      title <- getNodeSet(post, ".//div[@class = 'summary']/h3/a")
      title <- sapply(title, trimXMLValue)
      title <- setMissingAsNA(title)
    }
    
    getUrl <- function(post) {
      url <- getNodeSet(post, ".//div[@class = 'summary']/h3/a/@href")
      url <- setMissingAsNA(url)
      if (!is.na(url)) url <- paste0(baseUrl, url)
    }
    
    getViews <- function(post) {
      views <- getNodeSet(post, ".//div[@class = 'views ']/@title")
      views <- gsub("\\D", "", views)
      views <- setMissingAsNA(views)
    }
    
    getVotes <- function(post) {
      votes <- getNodeSet(post, ".//span[@class = 'vote-count-post ']")
      votes <- sapply(votes, trimXMLValue)
      votes <- setMissingAsNA(votes)
    }
    
    getAnswers <- function(post) {
      answers <- getNodeSet(post, ".//div[contains(@class, 'status')]")
      answers <- sapply(answers, trimXMLValue)
      answers <- setMissingAsNA(answers)
      answers <- gsub("answers*", "", answers)
    }
    
    getUser <- function(post) {
      numChildren <- xpathSApply(post, ".//div[@class = 'user-details']", xmlChildren)
      if (length(numChildren) > 2)
        user <- getNodeSet(post, ".//div[@class = 'user-details']/a")
      else
        user <- getNodeSet(post, ".//div[@class = 'user-details']")
      user <- sapply(user, trimXMLValue)
      user <- setMissingAsNA(user)
    }
    
    getReputation <- function(post) {
      reputation <- getNodeSet(post, ".//span[@class = 'reputation-score']")
      reputation <- sapply(reputation, trimXMLValue)
      reputation <- setMissingAsNA(reputation)
    }
    
    summary <- data.frame(id = getId(post),
                          date = getDate(post),
                          tags = getTags(post),
                          title = getTitle(post),
                          url = getUrl(post),
                          views = getViews(post),
                          votes = getVotes(post),
                          answers = getAnswers(post),
                          user = getUser(post),
                          reputation = getReputation(post),
                          stringsAsFactors = FALSE)
  }
  result <- sapply(post, scrapePost)
  t(result)
}

page1 <- as.data.frame(scrapePage(rhtml))

# Scrapes summaries from pages on Stack Overflow
# pages are ordered from newest first, with pagesize 50
#
# Args:
#   forum: the forum/tag to be scraped from
#   numPages: the number of pages to scrape, with default Inf
#
# Returns:
#   A dataframe containing the summaries for numPages,
#   see Returns from "scrapePage" for specifics
scrapeStackOverflow <- function(forum, numPages = Inf) {
  doc <- getURLContent(paste0(baseUrl, "/questions/tagged/", forum, "?sort=newest&pagesize=50"))
  page <- htmlParse(doc, asText = TRUE)
  result <- data.frame()
  curPage <- 1
  while (curPage <= numPages) {
    result <- rbind(result, scrapePage(page))
    nextUrl <- as.character(getNodeSet(page, "//div/a[@rel = 'next']/@href"))
    if (length(nextUrl) == 0)
      break
    tempDoc <- rawToChar(getURLContent(paste0(baseUrl, nextUrl), binary = TRUE))
    page <- htmlParse(tempDoc, asText = TRUE)
    curPage <- curPage + 1
  }
  result
}

R2pages <- scrapeStackOverflow("r", 2)
R200pages <- scrapeStackOverflow("r", 200)
saveRDS(R200pages, file = 'R200pages.rda')

allPagesR <- scrapeStackOverflow("r")

# Part 2

indPostUrl <- getURLContent("http://stackoverflow.com/questions/1395528/scraping-html-tables-into-r-data-frames-using-the-xml-package")
indPost <- htmlParse(indPostUrl, asText = TRUE)

# Scrapes all entries (question, answers, comments)
# from an individual Stack Overflow post
#
# Args:
#   post: the post to be scraped, once it has been htmlParsed
#
# Returns:
#   A dataframe containing:
#     user, userid, date, reputation, score, 
#     text, type, parent, id, qid
#   for every entry
scrapeEntries <- function(post) {
  question <- getNodeSet(post, "//div[@class = 'question']")
  comments <- getNodeSet(post, "//div[@class = 'comments ']")
  answers <- getNodeSet(post, "//div[@id = 'answers']")
  
  scrapeQuestion <- function(question) {
    getUser <- function(question) {
      numChildren <- xpathSApply(question, ".//div[@class = 'user-details']", xmlChildren)
      if (length(numChildren) > 2)
        user <- getNodeSet(question, ".//div[@class = 'user-details']/a")
      else
        user <- getNodeSet(question, ".//div[@class = 'user-details']")
      user <- sapply(user, trimXMLValue)
      user <- setMissingAsNA(user)
    }
    
    getUserid <- function(question) {
      userid <- getNodeSet(question, ".//div[@class = 'user-details']/a/@href")
      userid <- setMissingAsNA(userid)
      userid <- as.character(gsub("/(\\D*)/*", "", userid))
    }
    
    getDate <- function(question) {
      date <- getNodeSet(question, ".//span[@class = 'relativetime']/@title")
      date <- as.character(unlist(sapply(date, "[[", 1)))
    }
    
    getReputation <- function(question) {
      reputation <- getNodeSet(question, ".//span[@class = 'reputation-score']")
      reputation <- sapply(reputation, trimXMLValue)
      reputation <- setMissingAsNA(reputation)
    }
    
    getScore <- function(question) {
      score <- getNodeSet(question, ".//span[@class = 'vote-count-post ']")
      score <- sapply(score, trimXMLValue)
      score <- setMissingAsNA(score)
    }
    
    getText <- function(question) {
      text <- getNodeSet(question, ".//div[@class = 'post-text']")
      text <- sapply(text, xmlValue)
      text <- setMissingAsNA(text)
    }
    
    getType <- function(question) {
      type <- "question"
    }
    
    getParent <- function(question) {
      parent <- NA
    }
    
    getId <- function(question) {
      id <- as.character(getNodeSet(question, "./@data-questionid"))
      id <- setMissingAsNA(id)      
    }
    
    getQid <- function(question) {
      qid <- as.character(getNodeSet(question, "./@data-questionid"))
      qid <- setMissingAsNA(qid)
    }
    
    summary <- data.frame(user = getUser(question),
                          userid = getUserid(question),
                          date = getDate(question),
                          reputation = getReputation(question),
                          score = getScore(question),
                          text = getText(question),
                          type = getType(question),
                          parent = getParent(question),
                          id = getId(question),
                          qid = getQid(question), 
                          stringsAsFactors = FALSE)
  }
  
  scrapeComments <- function(comments) {
    comment <- getNodeSet(comments, ".//tr[@class = 'comment ']")
    
    scrapeComment <- function(comment) {
      getUser <- function(comment) {
        user <- getNodeSet(comment, ".//a[contains(@class, 'comment-user')]")
        user <- sapply(user, trimXMLValue)
        user <- setMissingAsNA(user)
      }
      
      getUserid <- function(comment) {
        userid <- getNodeSet(comment, ".//a[contains(@class, 'comment-user')]/@href")
        userid <- setMissingAsNA(userid)
        userid <- as.character(gsub("/(\\D*)/*", "", userid))
      }
      
      getDate <- function(comment) {
        date <- getNodeSet(comment, ".//span[@class = 'relativetime-clean']/@title")
        date <- as.character(unlist(sapply(date, "[[", 1)))
      }
      
      getReputation <- function(comment) {
        reputation <- getNodeSet(comment, ".//a[contains(@class, 'comment-user')]/@title")
        reputation <- setMissingAsNA(reputation)
        reputation <- as.character(gsub("\\D", "", reputation))
      }
      
      getScore <- function(comment) {
        score <- getNodeSet(comment, ".//td[@class = ' comment-score']")
        score <- suppressWarnings(as.numeric(sapply(score, trimXMLValue)))
      }
      
      getText <- function(comment) {
        text <- getNodeSet(comment, ".//span[@class = 'comment-copy']")
        text <- sapply(text, trimXMLValue)
        text <- setMissingAsNA(text)
      }
      
      getType <- function(comment) {
        type <- "comment"
      }
      
      getParent <- function(comment) {
        parent <- getNodeSet(comment, "./../../../@id")
        parent <- setMissingAsNA(parent)
        parent <- as.character(gsub("\\D", "", parent))
      }
      
      getId <- function(comment) {
        id <- getNodeSet(comment, "./@id")
        id <- setMissingAsNA(id)
        id <- as.character(gsub("\\D", "", id))
      }
      
      getQid <- function(comment) {
        qid <- as.character(getNodeSet(comment, "//div[@class = 'question']/@data-questionid"))
        qid <- setMissingAsNA(qid)
      }
      
      summary <- data.frame(user = getUser(comment),
                            userid = getUserid(comment),
                            date = getDate(comment),
                            reputation = getReputation(comment),
                            score = getScore(comment),
                            text = getText(comment),
                            type = getType(comment),
                            parent = getParent(comment),
                            id = getId(comment),
                            qid = getQid(comment), 
                            stringsAsFactors = FALSE)
    }
    
    sapply(comment, scrapeComment)
  }
  
  scrapeAnswers <- function(answers) {
    answer <- getNodeSet(answers, ".//div[@class = 'answer accepted-answer' or @class = 'answer']")
    
    scrapeAnswer <- function(answer) {
      getUser <- function(answer) {
        user <- getNodeSet(answer, ".//div[@class = 'user-details']")
        if (length(user) > 1) {
          user <- user[[length(user)]]
          user <- getNodeSet(user, "./a")
        }
        user <- sapply(user, trimXMLValue)
        user <- setMissingAsNA(user)
      }
      
      getUserid <- function(answer) {
        userid <- getNodeSet(answer, ".//div[@class = 'user-details']")
        if (length(userid) > 1) {
          userid <- userid[[length(userid)]]
          userid <- getNodeSet(userid, "./a/@href")
        }
        else userid <- NA
        userid <- as.character(gsub("/(\\D*)/*", "", userid))
      }
      
      getDate <- function(answer) {
        date <- getNodeSet(answer, ".//span[@class = 'relativetime']/@title")
        if (length(date) > 1)
          date <- date[[length(date)]]
        date <- as.character(unlist(sapply(date, "[[", 1)))
      }
      
      getReputation <- function(answer) {
        reputation <- getNodeSet(answer, ".//span[contains(@class, 'reputation-score')]/@title")
        reputation <- setMissingAsNA(reputation)
        reputation <- as.character(gsub("\\D", "", reputation))
      }
      
      getScore <- function(answer) {
        score <- getNodeSet(answer, ".//span[@class = 'vote-count-post ']")
        score <- suppressWarnings(as.numeric(sapply(score, trimXMLValue)))
      }
      
      getText <- function(answer) {
        text <- getNodeSet(answer, ".//div[@class = 'post-text']")
        text <- sapply(text, trimXMLValue)
        text <- setMissingAsNA(text)
      }
      
      getType <- function(answer) {
        type <- "answer"
      }
      
      getParent <- function(answer) {
        parent <- NA
      }
      
      getId <- function(answer) {
        id <- getNodeSet(answer, "./@id")
        id <- setMissingAsNA(id)
        id <- as.character(gsub("\\D", "", id))
      }
      
      getQid <- function(answer) {
        qid <- as.character(getNodeSet(answer, "//div[@class = 'question']/@data-questionid"))
        qid <- setMissingAsNA(qid)
      }
      
      summary <- data.frame(user = getUser(answer),
                            userid = getUserid(answer),
                            date = getDate(answer),
                            reputation = getReputation(answer),
                            score = getScore(answer),
                            text = getText(answer),
                            type = getType(answer),
                            parent = getParent(answer),
                            id = getId(answer),
                            qid = getQid(answer), 
                            stringsAsFactors = FALSE)
    }
    
    temp <- lapply(answer, scrapeAnswer)
    ldply(temp, data.frame)
  }
  
  questionDF <- t(data.frame(sapply(question, scrapeQuestion)))
  commentsDF <- t(data.frame(sapply(comments, scrapeComments)))
  answersDF <- data.frame(lapply(answers, scrapeAnswers))
  result <- rbind(questionDF, commentsDF, answersDF)
  rownames(result) <- NULL
  return(result)
}

scrapedPost <- data.frame(scrapeEntries(indPost))

post2 <- htmlParse(getURLContent("http://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes-in-r"), asText = TRUE)
scrapedPost2 <- scrapeEntries(post2)

# Part 3
load("rQAs.rda")

# 1
rQAs_answers <- subset(rQAs, type == 'answer')
ans_freq <- as.data.frame(table(table(rQAs_answers$user)))
plot(ans_freq, main = "Distribution of Questions Answered Per User", 
     xlab = "Number of Questions Answered", 
     ylab = "Frequency of Users")

# 2
R200pages <- readRDS("R200pages.rda")

# get the tags extracted in Part 1
tags <- as.character(R200pages$tags)

# Put them all in 1 big vector and get the top 10
top10tags <- sort(table(unlist(strsplit(tags, "; "))), decreasing = TRUE)[1:10]
top10tags <- data.frame(tag = names(top10tags),
                        freq = top10tags)
plot(top10tags, main = "Top 10 Most Common Tags By Frequency",
     xlab = "Tag", ylab = "Frequency")

# 3
rQAs_questions <- subset(rQAs, type == 'question')

# Since grepl returns logicals, sum them up
sum(grepl("ggplot", rQAs_questions$text, ignore.case = TRUE))

# 4
sum(grepl("(xml)|(html)|(web scraping)", rQAs$text, ignore.case = TRUE))

# 5
# rownames contain URLs, which will have the title of the post
rQAs_titles <- rownames(rQAs)

# Example URL is in this format
# "http://stackoverflow.com/questions/32566505/r-how-to-parallelize-multi-panel-plotting-with-lattice-in-r-3-2-1.title21"

# First, remove everything before the title
rQAs_titles_cleaned <- gsub(".+\\d+/", "", rQAs_titles)

# Then, remove everything after the title
rQAs_titles_cleaned <- gsub("\\..*", "", rQAs_titles_cleaned)

# Now, put it all in 1 big vector
rQAs_titles_words <- unlist(strsplit(rQAs_titles_cleaned, "-"))

checkFunction <- function(word) {
  # Checks word to see if it's a function
  #
  # Args:
  #   word: the word to determine if it's a function
  #
  # Returns:
  #   Either name of the function or NULL
  if (!is.character(word))
    return(NULL)
  if (object.size(word) > 10000)
    return(NULL)
  if (is.function(get0(word)))
    return(word)
}

title_functions <- sapply(rQAs_titles_words, checkFunction)

# Remove NULL values
title_functions_cleaned <- title_functions[!sapply(title_functions, is.null)]

unique(title_functions_cleaned)

# 6
rQAs_ans_com <- subset(rQAs, type != "question")

checkText <- function(text) {
  # Checks text to see if there are functions in it
  #
  # Args:
  #   text: the text to be checked for functions
  #
  # Returns:
  #   Either names of functions or NULL
  html <- htmlParse(text)
  code <- getNodeSet(html, "//code")
  code <- sapply(code, trimXMLValue)
  code <- setMissingAsNA(code)
  sapply(code, checkFunction)
}

text_functions <- sapply(rQAs_ans_com$text, checkText)

# Remove NULL values
text_functions_cleaned <- text_functions[!sapply(text_functions, is.null)]

unique(text_functions_cleaned)
