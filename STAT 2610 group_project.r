# Require libary : tm, SnowballC, textstem, ggplot2, dplyr
# install the missing packeges by below code 
# install.packages(c("tm", "SnowballC", "textstem", "ggplot2", "dplyr", "lexicon")
# Functions

simple_stemming <- function(wordToStem) { #stemming
  wordToStem <- tolower(wordToStem)
  if (nchar(wordToStem) <= 3) return(wordToStem)
  wordToStem <- gsub("(sses|ies|es|s)$", "", wordToStem)
  wordToStem <- gsub("(ing|ly|ed)$", "", wordToStem)
  wordToStem <- gsub("(tion|sion|ance|ence|ment)$", "", wordToStem)
  wordToStem <- gsub("(ful|ness|ive|ize)$", "", wordToStem)
  wordToStem <- gsub("(.)\\1+$", "\\1", wordToStem)
  return(wordToStem) 
}

clean_document <- function(rawText, stopWords) { #clean text with stemming and remove stopwords
  rawText <- gsub("[0-9]+", "", rawText) # Remove numbers
  allWords <- unlist(strsplit(rawText, split = "[[:space:]]+|[[:punct:]]+")) 
  allWords <- allWords[nzchar(allWords)]
  allWords <- tolower(allWords)
  stemmedWords <- sapply(allWords, simple_stemming, USE.NAMES = FALSE)
  stemmedWords <- stemmedWords[!stemmedWords %in% stopWords]
  stemmedWords <- stemmedWords[nchar(stemmedWords) > 5] #keywprd length 
  return(stemmedWords)
}

draw_bar_chart <- function(tokens, documentLabel, outputPath) { #bar chart for top 20 keywords
  if (length(tokens) == 0) {
    return(FALSE)
  }
  wordFrequency <- sort(table(tokens), decreasing = TRUE) 
  topWords <- head(wordFrequency, min(20, length(wordFrequency))) #get words
  if (length(topWords) == 0) {
    return(FALSE)
  }
  png(filename = outputPath, width = 900, height = 700, res = 100) #file format
  wordNames <- names(topWords)
  wordCounts <- as.numeric(topWords)
  par(mar = c(12, 5, 4, 2)) #para
  barPositions <- barplot(wordCounts, 
                names.arg = wordNames,
                las = 2,
                col = colorRampPalette(c("steelblue", "lightblue"))(length(wordNames)),
                main = paste("Top 20 Keywords (Length > 5)\n", documentLabel),
                xlab = "",
                ylab = "Frequency",
                cex.names = 0.8,
                border = "darkblue")
  text(x = barPositions, y = wordCounts + 0.5, 
       labels = wordCounts, cex = 0.8, pos = 3)
  dev.off()
  return(TRUE)
}

draw_word_cloud <- function(tokens, documentLabel, outputPath) {
  if (length(tokens) == 0) {
    return(FALSE)
  }
  wordFrequency <- sort(table(tokens), decreasing = TRUE)
  topWords <- head(wordFrequency, min(80, length(wordFrequency)))
  if (length(topWords) == 0) {
    return(FALSE)
  }
  png(filename = outputPath, width = 1000, height = 800, res = 100)
  plot(1, type = "n", xlim = c(0, 100), ylim = c(0, 100),
       xlab = "", ylab = "", main = paste("Word Cloud (Length > 5)\n", documentLabel),
       axes = FALSE)
  wordCounts <- as.numeric(topWords)
  wordNames <- names(topWords)
  highestFreq <- max(wordCounts)
  lowestFreq <- min(wordCounts)
  if (highestFreq > lowestFreq) {
    fontSizes <- 0.8 + (wordCounts - lowestFreq) / (highestFreq - lowestFreq) * 3.2
  } else {
    fontSizes <- rep(2, length(wordCounts))
  }
  set.seed(42)
  xPositions <- runif(length(wordNames), 5, 95)
  yPositions <- runif(length(wordNames), 5, 95)
  for (i in 1:length(wordNames)) {
    for (j in 1:length(wordNames)) {
      if (i != j && abs(xPositions[i] - xPositions[j]) < 8 && abs(yPositions[i] - yPositions[j]) < 5) {
        xPositions[j] <- xPositions[j] + runif(1, -5, 5)
        yPositions[j] <- yPositions[j] + runif(1, -3, 3)
        xPositions[j] <- max(5, min(95, xPositions[j]))
        yPositions[j] <- max(5, min(95, yPositions[j]))
      }
    }
  }
  colorPalette <- rainbow(length(wordNames))
  for (i in 1:length(wordNames)) {
    text(xPositions[i], yPositions[i], 
         labels = wordNames[i], 
         cex = fontSizes[i],
         col = colorPalette[i],
         font = 2)
  }
  dev.off()
  return(TRUE)
}

run_full_analysis <- function(stopWords, inputFiles, outputFolder) { #stemming->tokenization->tf-idf calculation
  cat("\nProcessing", length(inputFiles), "files with", length(stopWords), "stopwords...\n")
  documentTokens <- list() #create a list to store tokens for each document
  for (fileIndex in seq_along(inputFiles)) { 
    currentFile <- inputFiles[fileIndex]
    shortName <- basename(currentFile) 
    cat("  Working on:", shortName, "\n")
    fileLines <- readLines(currentFile, warn = FALSE, encoding = "UTF-8")
    fullText <- paste(fileLines, collapse = " ")
    cleanedTokens <- clean_document(fullText, stopWords)
    documentTokens[[shortName]] <- cleanedTokens
  }
  tokensFolder <- file.path(outputFolder, "01_tokenized_files") #file creation
  dir.create(tokensFolder, showWarnings = FALSE)
  for (fileName in names(documentTokens)) {
    tokenOutputPath <- file.path(tokensFolder, paste0("tokens_", fileName))
    writeLines(paste(documentTokens[[fileName]], collapse = " "), tokenOutputPath) 
  }
  cat("  Tokenized files saved.\n")
  barChartsFolder <- file.path(outputFolder, "02_bar_charts")
  dir.create(barChartsFolder, showWarnings = FALSE)
  wordCloudsFolder <- file.path(outputFolder, "03_word_clouds")
  dir.create(wordCloudsFolder, showWarnings = FALSE)
  for (fileName in names(documentTokens)) {
    barChartFile <- paste0("barchart_", gsub(".txt", "", fileName), ".png")
    barChartPath <- file.path(barChartsFolder, barChartFile)
    draw_bar_chart(documentTokens[[fileName]], fileName, barChartPath)
    wordCloudFile <- paste0("wordcloud_", gsub(".txt", "", fileName), ".png") 
    wordCloudPath <- file.path(wordCloudsFolder, wordCloudFile)
    draw_word_cloud(documentTokens[[fileName]], fileName, wordCloudPath)
  }
  cat("  Charts and word clouds saved.\n")
  uniqueTerms <- sort(unique(unlist(documentTokens)))
  if (length(uniqueTerms) == 0) {
    cat("  No terms remaining!\n")
    flush.console()
    return(list(tfidfMatrix = NULL, allTokens = NULL, topKeywords = NULL))
  }
  docFrequency <- sapply(uniqueTerms, function(term) {
    sum(sapply(documentTokens, function(singleDoc) any(singleDoc == term)))
  })
  inverseDocFreq <- log(length(inputFiles) / docFrequency)
  tfidfMatrix <- matrix(0, nrow = length(inputFiles), ncol = length(uniqueTerms),
                         dimnames = list(names(documentTokens), uniqueTerms)) #create a matrix to store tf-idf values
  for (docIndex in seq_along(documentTokens)) {
    currentDocTokens <- documentTokens[[docIndex]]
    if (length(currentDocTokens) == 0) next
    termCounts <- table(currentDocTokens)
    termFreqs <- termCounts / length(currentDocTokens)
    for (term in names(termCounts)) {
      colPos <- match(term, uniqueTerms)
      if (!is.na(colPos)) {
        tfidfMatrix[docIndex, colPos] <- termFreqs[term] * inverseDocFreq[term]
      }
    }
  }
  tfidfFolder <- file.path(outputFolder, "04_tfidf_matrix")
  dir.create(tfidfFolder, showWarnings = FALSE)
  tfidfFilePath <- file.path(tfidfFolder, "tfidf_matrix.csv")
  write.csv(as.data.frame(tfidfMatrix), file = tfidfFilePath, row.names = TRUE)
  cat("  TF-IDF matrix saved.\n")
  avgTfidf <- colMeans(tfidfMatrix)
  sortedTopTfidf <- sort(avgTfidf, decreasing = TRUE)
  topTenKeywords <- head(sortedTopTfidf, min(10, length(sortedTopTfidf)))
  cat("  TOP 10 KEYWORDS (Length > 5)\n")
  for (rank in 1:length(topTenKeywords)) {
    tfidfValue <- signif(topTenKeywords[rank], 3)
    cat("  ", rank, ". ", names(topTenKeywords)[rank], " - ", tfidfValue, "\n", sep="")
  }
  flush.console()   #refresh console as sometimes bug appear
  return(list(tfidfMatrix = tfidfMatrix, allTokens = documentTokens, 
              topKeywords = topTenKeywords))
}

# Main Program
inputFolder <- NULL 
while (is.null(inputFolder) || !dir.exists(inputFolder)) { #check folder exist,repeat loop
  if (!is.null(inputFolder)) { 
    cat("Folder not found. Please try again.\n\n")
  }
  cat("Enter the folder path containing your .txt files:\n")
  inputFolder <- readline(prompt = "Path: ")
  inputFolder <- trimws(inputFolder)
}
cat("\nFolder found:", inputFolder, "\n\n")
stopWordsFilePath <- file.path(inputFolder, "common_word.txt")
while (!file.exists(stopWordsFilePath)) {
  cat("common_word.txt not found in:", inputFolder, "\n") #check common_word for tokenization
  inputFolder <- readline(prompt = "Enter correct folder path: ")
  inputFolder <- trimws(inputFolder)
  stopWordsFilePath <- file.path(inputFolder, "common_word.txt")
}
stopWordsFileLines <- readLines(stopWordsFilePath, warn = FALSE, encoding = "UTF-8") #first token and tf-idf round
combinedStopWordsText <- paste(stopWordsFileLines, collapse = " ") 
rawStopWordsList <- unlist(strsplit(combinedStopWordsText, split = "[[:space:]]+|[[:punct:]]+"))
rawStopWordsList <- rawStopWordsList[nzchar(rawStopWordsList)]
stopWords <- unique(tolower(rawStopWordsList)) 
stopWords <- unique(sapply(stopWords, simple_stemming))
cat("Loaded", length(stopWords), "stopwords (stemmed)\n\n")
allTextFiles <- list.files(path = inputFolder, pattern = "\\.txt$", full.names = TRUE, 
                        recursive = FALSE, ignore.case = TRUE)
allTextFiles <- allTextFiles[!basename(allTextFiles) %in% c("common_word.txt", "common_word.TXT")]
if (length(allTextFiles) == 0) stop("No .txt files found.")
cat("Found", length(allTextFiles), "text files\n\n")
analysisOutputFolder <- file.path(inputFolder, "analysis_output")
if (dir.exists(analysisOutputFolder)) {
  unlink(analysisOutputFolder, recursive = TRUE)
}
dir.create(analysisOutputFolder, showWarnings = FALSE)
analysisResults <- run_full_analysis(stopWords, allTextFiles, analysisOutputFolder)
cat("\n")
userWantsMoreStopwords <- readline(prompt = "Add more stopwords? (yes/no): ")
userWantsMoreStopwords <- tolower(trimws(userWantsMoreStopwords))
while (userWantsMoreStopwords == "yes" || userWantsMoreStopwords == "y") { #repeat is keyword is needed
  additionalStopwordsInput <- readline(prompt = "Enter additional stopwords (comma-separated): ")
  newStopWords <- unlist(strsplit(additionalStopwordsInput, ","))
  newStopWords <- trimws(newStopWords)
  newStopWords <- tolower(newStopWords)
  newStopWords <- unique(sapply(newStopWords, simple_stemming))
  stopWords <- unique(c(stopWords, newStopWords))
  cat("Added", length(newStopWords), "words. Total:", length(stopWords), "\n")
  cat("\ndeleting\n")
  unlink(analysisOutputFolder, recursive = TRUE)
  dir.create(analysisOutputFolder, showWarnings = FALSE)
  cat("updating data\n")
  analysisResults <- run_full_analysis(stopWords, allTextFiles, analysisOutputFolder)
  cat("\n")
  userWantsMoreStopwords <- readline(prompt = "Add MORE stopwords? (yes/no): ")
  userWantsMoreStopwords <- tolower(trimws(userWantsMoreStopwords))
}
cat("analysis complete\n")
cat("Output folder:", analysisOutputFolder, "\n")
if (!is.null(analysisResults$topKeywords) && length(analysisResults$topKeywords) > 0) { #output final
  cat("Final Top 10 Keywords (Length > 5, 3 significant figures):\n")
  for (rank in 1:length(analysisResults$topKeywords)) {
    tfidfValue <- signif(analysisResults$topKeywords[rank], 3)
    keywordName <- names(analysisResults$topKeywords)[rank]
    cat(rank, ". ", keywordName, " - ", tfidfValue, "\n", sep="")
  }
} else {
  cat("No keywords found. Try reducing stopwords or check your text files.\n")
}
flush.console()