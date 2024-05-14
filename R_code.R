library(pdftools)
library(syuzhet)
library(ggplot2)
library(tidyverse)
library(tools)

#######################################################################################################################################################
# Sentimental analysis 
main_folder <- "C:/Users/yassi/OneDrive - Högskolan Dalarna/DATA_FINAL/Region"
sub_folders <- list.dirs(main_folder, recursive = FALSE)

analyze_pdf_sentiment_in_folder <- function(folder_path) {
  pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)
  sentiments <- sapply(pdf_files, analyze_and_classify_pdf)
  folder_name <- basename(folder_path)
  data.frame(Region = folder_name, Sentiment = sentiments)
}

analyze_and_classify_pdf <- function(pdf_path) {
  text <- pdf_text(pdf_path)
  full_text <- paste(text, collapse = " ")
  sentiment_scores <- get_sentiment(full_text, method = "syuzhet")
  mean_score <- mean(sentiment_scores)
  if (mean_score > 0.5) {
    return("Positive")
  } else if (mean_score < -0.05) {
    return("Negative")
  } else {
    return("Neutral")
  }
}

all_sentiments_data <- do.call(rbind, lapply(sub_folders, analyze_pdf_sentiment_in_folder))

sentiment_counts_by_region <- count(all_sentiments_data, Region, Sentiment)

ggplot(sentiment_counts_by_region, aes(x = Region, y = n, fill = Sentiment)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Positive" = "blue", "Neutral" = "grey", "Negative" = "red")) +
  labs(title = "General Sentiment Distribution of PDFs by Region",
       x = "Region",
       y = "Files") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
graph_filename <- paste0("sentiment_distribution_", current_time, ".pdf")
ggsave(graph_filename, path = main_folder)



################################################################################################################################################

install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("translateR")
library(dplyr)
library(ggplot2)
library(readxl)

#the total number of CSR reports in the GRI database for the finance sector-------
master <- as.data.frame(read.csv("C:\\Users\\gelaw\\Downloads\\gri_with_txt.csv"))
View(master)
#variable.names(master)


#install packages----
install.packages("pdftools")
library(pdftools)
require(pdftools)
require(tm)
require(tidyverse)
require(quanteda)
require(wordcloud2)
require(tidytext)

#Data Collection------
setwd("C:\\Users\\gelaw\\Fredrik BI\\data vis\\home assignment\\DATA2")



#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files<-list.files(pattern = "pdf$")


#read_pdf_text <- function(files) {tryCatch({pdf_text(file_path)}, error = function(e) {message(passte("Error reading", files, ":", conditionMessage(e)))NULL})}

#to read text from each PDF file using pdf_text and stored the results in the opinions list.
text_from_pdf<-lapply(files,pdf_text)

#verify how many pdf files has been loaded
length(PDFTEXTS)

#to check the number of pages in each pdf files
lapply(text_from_pdf,length)


#Save the output in a datafile.
saveRDS(text_from_pdf, file = "PDFTEXTS.RDS") 

#Call the datafile
PDFTEXTS <- readRDS("PDFTEXTS.RDS")


#Data Cleaning and Tokenization
#creating a pdf database
pdfdatabase<-Corpus(URISource(files),readerControl = list(reader=readPDF))

#Save the output in a datafile.
saveRDS(pdfdatabase, file = "pdfdatabase.RDS") 

#Call the datafile
pdfdatabase <- readRDS("pdfdatabase.RDS")


#cleans up the documents and load them into
#term document matrix
# count words that appear in at least 3 documents
textmatrix.tdm <- TermDocumentMatrix(pdfdatabase, 
                                     control = 
                                       list(removePunctuation = TRUE,
                                            stopwords =TRUE,
                                            tolower = TRUE,
                                            stemming = FALSE,
                                            removeNumbers = TRUE,
                                            bounds = list(global = c(3, Inf))))
inspect(textmatrix.tdm[1:10,]) #first 10 rows of the term-document matrix

ft<-findFreqTerms(textmatrix.tdm, lowfreq = 10, highfreq = Inf)
as.matrix(textmatrix.tdm[ft,])

ft.tdm <- as.matrix(textmatrix.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE)

#create subset to plot only 10 rows
ft1 <- sort(apply(ft.tdm, 1, sum), decreasing = TRUE)
subset_ft1 <- ft1[1:10]

top10 <- data.frame(subset_ft1)

view(top10)
# Assign column names
colnames(top10) <- c("Frequency")
rownames(top10, do.NULL = FALSE, prefix = Keywords)

# Create a bar plot
ggplot(top10, aes(x = reorder(rownames(top10), -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 10 Keyword Frequency for 1000 word dictionary",
       x = "Keywords",
       y = "Frequency") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#check specific Keyword Frequency
textmatrix.dfm <- as.dfm(textmatrix.tdm)

# Specify the keywords you want to check

# specific_keywords <- c("welfare", "inclusion", "esg", "equal", "gas", "carbon", "ethical", "awareness", "diversity", "responsibilities", "climate", "emissions", "csr", "responsible", "sustainable", "environment", "responsibility", "sustainability", "environmental", "greenhouse gas emissions", "diversity", "employee health & safety", "customer welfare")

specific_keywords <- c("welfare", "inclusion", "esg", "equal", "gas", "carbon", "ethical", "awareness", "diversity", "responsibilities", "climate", "emissions", "csr", "responsible", "sustainable", "environment", "responsibility", "sustainability", "environmental", "corporate social responsibility", "carbon dioxide", "methane", "greenhouse", "greenhouse gas", "greenhouse gas emissions", "environmental, social, and governance")
phrase_keywords<-phrase(specific_keywords)

# Check if specific keywords are present in the DFM
keywords_exist <- dfm_select(textmatrix.dfm, pattern = phrase_keywords)

# Sum the counts for each keyword
keyword_counts <- colSums(as.matrix(keywords_exist))

# Create a data frame for plotting
plot_data <- data.frame(Keyword = names(keyword_counts), Frequency = keyword_counts)
View(plot_data)
# Create a bar plot
ggplot(plot_data, aes(x = reorder(Keyword, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Keyword Frequency",
       x = "Keywords",
       y = "Frequency") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# With the additional keywords and original keywords 
# manualy processed we get the following 95 word list:
word95 <- read.table(file = "C:\\Users\\gelaw\\Fredrik BI\\data vis\\home assignment\\95 word list.txt", header = TRUE)
View(word95)

# Create a bar plot
ggplot(word95, aes(x = reorder(Keyword, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Keyword Frequency",
       x = "Keywords",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Select only top 10 results
subset_word95 <- word95[1:10, ]

view(subset_word95)

# Create a bar plot
ggplot(subset_word95, aes(x = reorder(Keyword, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Top 10 Keyword Frequency for 95 word dictionary",
       x = "Keywords",
       y = "Frequency") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# With the additional keywords and original keywords 
# manualy processed we get the following 79 word list:
word79 <- read.table(file = "C:\\Users\\gelaw\\Fredrik BI\\data vis\\home assignment\\79 word list.txt", header = TRUE)
View(word79)

# Create a bar plot
ggplot(word79, aes(x = reorder(Keyword, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Top 10 Keyword Frequency for 79 word dictionary",
       x = "Keywords",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Select only top 10 results
subset_word79 <- word79[1:10, ]

view(subset_word79)

# Create a bar plot
ggplot(subset_word79, aes(x = reorder(Keyword, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Top 10 Keyword Frequency for 79 word dictionary",
       x = "Keywords",
       y = "Frequency") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Topic analysis
# manualy processed we get the following sorting of the 79 word list 
# into the four CSR-groups legislators are interested in:
Topics <- read.table(file = "C:\\Users\\gelaw\\Fredrik BI\\data vis\\home assignment\\79 words list sorted on topic excel2.csv", sep = ";" , header = TRUE)
View(Topics)


# Create a bar plot
ggplot(Topics, aes(x = reorder(Topic, -Frequency, sum), y = Frequency, fill=Topic)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#CC0033", "#FF9900", "#3333FF", "darkgrey", "#66cc00")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Keyword Frequency by Topic Cluster",
       x = "Topics",
       y = "Frequency") +
  theme_bw() 

#############################################################################################################################################################


#packages---------
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("translateR")
library(dplyr)
library(ggplot2)
library(readxl)
install.packages("pdftools")
library(pdftools)
require(pdftools)
require(tm)
require(tidyverse)
require(quanteda)
require(wordcloud2)
require(tidytext)
require(wordcloud)
library(reticulate)
library(stringr)
library(SnowballC)
library(textdata)
library(quanteda)
library(quanteda.textstats)
library(tidyverse)
library(tidyr)
library(syuzhet)
library(reshape2)
library(ggraph)
library(igraph)
#Functions----------
#to count Keyword Frequency
count_combinations <- function(text, combinations) {
  occurrences <- sapply(combinations, function(comb) {
    sum(str_count(text, fixed(comb)))
  })
  return(occurrences)
}


#create a function that takes the corpus as input and performs the text preprocessing steps.
preprocess_corpus <- function(corpus) {
  # Convert to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation,preserve_intra_word_dashes=TRUE)
  
  # Remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  
  # Remove English stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  # Remove hyphens
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("-", " ", x)))
  
  # Remove symbols (assuming symbols are non-alphanumeric characters)
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))
  
  
  # Remove extra whitespaces
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

#Data Collection------
file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/Data clean"
#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files <- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
opinions<-lapply(files,pdf_text)

#verify how many pdf files has been loaded
length(opinions)

#Frequency of keyword combinations----------

keyword_combinations <- c("greenhouse gas emissions","diversity", "employee health", "customer welfare")
opinions_char<-unlist(opinions)

corpus <- Corpus(VectorSource(opinions_char))

# Perform text preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)             # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                 # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))   # Remove English stopwords
corpus <- tm_map(corpus, stripWhitespace)               # Remove extra whitespaces
# Remove hyphens
corpus <- tm_map(corpus, content_transformer(function(x) gsub("-", " ", x)))

# Remove symbols (assuming symbols are non-alphanumeric characters)
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))


# Extract cleaned text as a character vector
opinions_char_cleaned <- sapply(corpus, as.character)
class(opinions_char_cleaned)


head(opinions_char_cleaned)
count_combinations <- function(text, combinations) {
  occurrences <- sapply(combinations, function(comb) {
    sum(str_count(text, fixed(comb)))
  })
  return(occurrences)
}

# Count occurrences of each keyword combination
combination_counts <- count_combinations(opinions_char_cleaned, keyword_combinations)

# Create a data frame for plotting
plot_data <- data.frame(Combination = keyword_combinations, Count = combination_counts)
view(plot_data)
# Plot the occurrences

total_count <- sum(plot_data$Count)

# Calculate frequency percentage for each keyword
plot_data$Frequency_Percentage <- (plot_data$Count / total_count) * 100


# Reorder Combination based on Count in decreasing order
plot_data$Combination <- factor(plot_data$Combination, levels = plot_data$Combination[order(plot_data$Frequency_Percentage, decreasing = TRUE)])

keyword_plot<-ggplot(plot_data, aes(x = Combination, y = Frequency_Percentage, fill = Combination)) +
  geom_bar(stat = "identity") +
  labs(title = "Keyword Frequency Percentage Distribution",
       x = "Keyword Topics", y = "Frequency Percentage") +
  geom_text(aes(label = paste0(round(Frequency_Percentage, 2), "%")), 
            vjust = -0.5, color = "black", size = 3, position = position_dodge(width = 0.9)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Rotate x-axis labels for better readability
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave("keyword.png", plot = keyword_plot, width = 6, height = 4, dpi = 300)

#Wordcount on Financial Services 
word_cloud <- wordcloud(words = corpus,min.freq = 20,
                        max.words = 100,random.order = FALSE,rot.per = 0.35,scale = c(2,1.0),
                        colors = brewer.pal(8,"Dark2"))

# Save word cloud as PNG
ggsave("wordcloud.png", plot = word_cloud, width = 6, height = 4, dpi = 300)


#---------Keyword Context-------------
class(opinions_char_cleaned)

tokens_opinions <- tokens(opinions_char_cleaned)


# Define patterns for each keyword
pattern_greenhouse <- phrase("greenhouse gas emissions")
pattern_diversity <- phrase("diversity")
pattern_employee_health <- phrase("employee health")
pattern_customer_welfare <- phrase("customer welfare")

#Find the Keyword context
kwic_results_greenhouse <- kwic(tokens_opinions,pattern_greenhouse, window = 5)
kwic_results_diversity <- kwic(tokens_opinions,pattern_diversity, window = 5)
kwic_results_employee_health <- kwic(tokens_opinions,pattern_employee_health, window = 5)
kwic_results_customerwelfare <- kwic(tokens_opinions,pattern_customer_welfare, window = 5)
kwic_df_greenhouse <- as.data.frame(kwic_results_greenhouse)
kwic_df_customerwelfare <- as.data.frame(kwic_results_customerwelfare)
kwic_df_employeehealth <- as.data.frame(kwic_results_employee_health)
kwic_df_diversity <- as.data.frame(kwic_results_diversity)

## Limit KWIC results to 5

kwic_resultsgreenhouse_5 <- head(kwic_df_greenhouse,5)
kwic_resultsdiversity_5 <- head(kwic_df_diversity, 5)
kwic_resultscustomer_5 <- head(kwic_df_customerwelfare, 5)
kwic_resultsemployee_5 <- head(kwic_df_employeehealth,5)
class(kwic_resultsemployee_5)

View(kwic_combined_greenhouse)

kwic_combined_greenhouse <- cbind(kwic_resultsgreenhouse_5$keyword, kwic_resultsgreenhouse_5$post)
kwic_combined_diversity<-greenhouse <- cbind(kwic_resultsdiversity_5$keyword, kwic_resultsdiversity_5$post)
kwic_combined_customer <- cbind(kwic_resultscustomer_5$keyword, kwic_resultscustomer_5$post)
kwic_combined_employee <- cbind(kwic_resultsemployee_5$keyword, kwic_resultsemployee_5$post)

kwic_combined <- rbind(
  kwic_combined_greenhouse,
  kwic_combined_diversity,
  kwic_combined_customer,
  kwic_combined_employee
)

column_names <- c("Keyword", "Context")

# Assign column names to the dataframe
colnames(kwic_combined) <- column_names

View(kwic_combined)

# Save the combined dataframe to a CSV file
write.csv(kwic_combined, "kwic_combined.csv", row.names = FALSE)



#Keyword combinations year wise---------------

#Keyword Frequency for year 2010

file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/2010"
#Data Loading
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files_2010 <- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
opinions_2010<-lapply(files_2010,pdf_text)
length(opinions_2010)
keyword_combinations <- c("greenhouse gas emissions","diversity", "employee health", "customer welfare")
opinions_char_2010<-unlist(opinions_2010)

corpus <- Corpus(VectorSource(opinions_char_2010))

# Perform text preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)             # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                 # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))   # Remove English stopwords
corpus <- tm_map(corpus, stripWhitespace)               # Remove extra whitespaces

# Extract cleaned text as a character vector
opinions_char_cleaned_2010 <- sapply(corpus, as.character)
class(opinions_char_cleaned_2010)


head(opinions_char_cleaned_2010)
count_combinations <- function(text, combinations) {
  occurrences <- sapply(combinations, function(comb) {
    sum(str_count(text, fixed(comb)))
  })
  return(occurrences)
}

# Count occurrences of each keyword combination
combination_counts <- count_combinations(opinions_char_cleaned_2010, keyword_combinations)

# Create a data frame for plotting
plot_data_2010 <- data.frame(Combination = keyword_combinations, Count = combination_counts,Year=2010)
view(plot_data_2010)

#Keyword Frequency for year 2011
file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/2011"
#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files_2011 <- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
opinions_2011<-lapply(files_2011,pdf_text)
length(opinions_2011)
keyword_combinations <- c("greenhouse gas emissions","diversity", "employee health", "customer welfare")
opinions_char_2011<-unlist(opinions_2011)


corpus <- Corpus(VectorSource(opinions_char_2011))
corpus_processed <- preprocess_corpus(corpus)
opinions_char_cleaned_2012 <- sapply(corpus_processed, as.character)
class(opinions_char_cleaned_2011)

# Count occurrences of each keyword combination
combination_counts <- count_combinations(opinions_char_cleaned_2011, keyword_combinations)

# Create a data frame for plotting
plot_data_2011 <- data.frame(Combination = keyword_combinations, Count = combination_counts,Year=2011)
view(plot_data_2011)

#Keyword Frequency for year 2012
file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/2012"
#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files_2012 <- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
opinions_2012<-lapply(files_2012,pdf_text)

length(opinions_2012)

opinions_char_2012<-unlist(opinions_2012)


corpus <- Corpus(VectorSource(opinions_char_2012))
corpus_processed <- preprocess_corpus(corpus)
opinions_char_cleaned_2012 <- sapply(corpus_processed, as.character)
class(opinions_char_cleaned_2012)


# Count occurrences of each keyword combination
combination_counts <- count_combinations(opinions_char_cleaned_2012, keyword_combinations)

# Create a data frame for plotting
plot_data_2012 <- data.frame(Combination = keyword_combinations, Count = combination_counts,Year=2012)
view(plot_data_2012)

#Keyword Frequency for year 2013
file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/2013"
#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files_2013 <- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
opinions_2013<-lapply(files_2013,pdf_text)

length(opinions_2013)

opinions_char_2013<-unlist(opinions_2013)


corpus <- Corpus(VectorSource(opinions_char_2013))
corpus_processed <- preprocess_corpus(corpus)
opinions_char_cleaned_2013 <- sapply(corpus_processed, as.character)
class(opinions_char_cleaned_2013)


# Count occurrences of each keyword combination
combination_counts <- count_combinations(opinions_char_cleaned_2013, keyword_combinations)

# Create a data frame for plotting
plot_data_2013 <- data.frame(Combination = keyword_combinations, Count = combination_counts,Year=2013)
view(plot_data_2013)


#Keyword Frequency for year 2014
file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/2014"
#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files_2014 <- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
opinions_2014<-lapply(files_2014,pdf_text)

length(opinions_2014)

opinions_char_2014<-unlist(opinions_2014)


corpus <- Corpus(VectorSource(opinions_char_2014))
corpus_processed <- preprocess_corpus(corpus)
opinions_char_cleaned_2014 <- sapply(corpus_processed, as.character)
class(opinions_char_cleaned_2014)


# Count occurrences of each keyword combination
combination_counts <- count_combinations(opinions_char_cleaned_2014, keyword_combinations)

# Create a data frame for plotting
plot_data_2014 <- data.frame(Combination = keyword_combinations, Count = combination_counts,Year=2014)
view(plot_data_2014)

#Keyword Frequency for year 2015
file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/2015"
#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files_2015 <- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
opinions_2015<-lapply(files_2015,pdf_text)

length(opinions_2015)

opinions_char_2015<-unlist(opinions_2015)


corpus <- Corpus(VectorSource(opinions_char_2015))
corpus_processed <- preprocess_corpus(corpus)
opinions_char_cleaned_2015 <- sapply(corpus_processed, as.character)
class(opinions_char_cleaned_2015)


# Count occurrences of each keyword combination
combination_counts <- count_combinations(opinions_char_cleaned_2015, keyword_combinations)

# Create a data frame for plotting
plot_data_2015 <- data.frame(Combination = keyword_combinations, Count = combination_counts,Year=2015)
view(plot_data_2015)

#Keyword Frequency for year 2016
file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/2016"
#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files_2016 <- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
opinions_2016<-lapply(files_2016,pdf_text)

length(opinions_2016)

opinions_char_2016<-unlist(opinions_2016)


corpus <- Corpus(VectorSource(opinions_char_2016))
corpus_processed <- preprocess_corpus(corpus)
opinions_char_cleaned_2016 <- sapply(corpus_processed, as.character)
class(opinions_char_cleaned_2016)


# Count occurrences of each keyword combination
combination_counts <- count_combinations(opinions_char_cleaned_2016, keyword_combinations)

# Create a data frame for plotting
plot_data_2016 <- data.frame(Combination = keyword_combinations, Count = combination_counts,Year=2016)
view(plot_data_2016)


#Keyword Frequency for year 2017
file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/2017"
#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
files_2017 <- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
opinions_2017<-lapply(files_2017,pdf_text)

length(opinions_2017)

opinions_char_2017<-unlist(opinions_2017)


corpus <- Corpus(VectorSource(opinions_char_2017))
corpus_processed <- preprocess_corpus(corpus)
opinions_char_cleaned_2017 <- sapply(corpus_processed, as.character)
class(opinions_char_cleaned_2017)


# Count occurrences of each keyword combination
combination_counts <- count_combinations(opinions_char_cleaned_2017, keyword_combinations)

# Create a data frame for plotting
plot_data_2017 <- data.frame(Combination = keyword_combinations, Count = combination_counts,Year=2017)
view(plot_data_2017)



#Plot the data
plot_data_2010$Year <- as.factor(plot_data_2010$Year)
plot_data_2011$Year <- as.factor(plot_data_2011$Year)
plot_data_2012$Year <- as.factor(plot_data_2012$Year)
plot_data_2013$Year<-as.factor(plot_data_2013$Year)
plot_data_2014$Year<-as.factor(plot_data_2014$Year)
plot_data_2015$Year<-as.factor(plot_data_2015$Year)
plot_data_2016$Year<-as.factor(plot_data_2016$Year)
plot_data_2017$Year<-as.factor(plot_data_2017$Year)
plot_data_year <- rbind(plot_data_2010, plot_data_2011,plot_data_2012,plot_data_2013,plot_data_2014,plot_data_2015,plot_data_2016,plot_data_2017)

view(plot_data_year)
plot_data_filtered <- plot_data_year[plot_data_year$Count != 0, ]
# Plot the stacked plot
# Calculate the total count for each combination
combination_total <- aggregate(Count ~ Combination, data = plot_data_filtered, FUN = sum)

# Reorder the combinations based on total count
plot_data_filtered$Combination <- factor(plot_data_filtered$Combination, 
                                         levels = combination_total$Combination[order(combination_total$Count, decreasing =  FALSE)])

# Plot with reordered Combination factor

keyword_year_plot <- ggplot(plot_data_filtered, aes(x = Year, y = Count, fill = Combination, label = Count)) +
  geom_bar(stat = "identity") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +  # Add data labels
  labs(title = "Keyword Frequency Over Years",
       x = "Year") +  # Removed y-axis label
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),  # Remove y-axis text
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "white"))

ggsave("keyword_year.png", plot = keyword_year_plot, width = 8, height = 4, dpi = 300)


#co-occurence network analysis
file_path <- "C:/Users/creat/OneDrive - Högskolan Dalarna/Region/Europe"

#Data Loading-----
#Loading of PDF Files,reading text from the pdf files and storing it in the list
#a list of files with names matching the "pdf$" pattern.
all_files<- list.files(path = file_path, pattern = "pdf$", full.names = TRUE)
#to read text from each PDF file using pdf_text and stored the results in the opinions list.
all_opinions<-lapply(all_files,pdf_text)
length(all_opinions)

#create corpus
corpus <- Corpus(VectorSource(all_opinions))
#pre process text
corpus_processed <- preprocess_corpus(corpus)


pdfdataframe<-data.frame(text=sapply(corpus_processed,as.character),stringsAsFactors = FALSE)
view(pdfdataframe)

#create Bigrams
New_bigrams<-pdfdataframe%>%
  unnest_tokens(bigram,text,token = "ngrams",n=2)
New_bigrams

#count bigram frequency
New_bigrams%>%
  count(bigram,sort=TRUE)

#seperate bigrams and remove stopwords
bigrams_seperated<-New_bigrams%>%
  separate(bigram,c("word1","word2"),sep=" ")

bigrams_filtered<-bigrams_seperated%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2%in% stop_words$word)

#newbigrams count
bigrams_counts<-bigrams_filtered%>%
  count(word1,word2,sort=TRUE)

#filtering for special words
bigrams_filtered%>%
  filter(word1=="diversity")%>%
  count(word2,sort=TRUE)

bigram_graph<-bigrams_counts%>%
  filter(n>5)%>%
  graph_from_data_frame()

# Create graph
bigram_graph <- graph_from_data_frame(edges = bigrams_filtered, directed = TRUE)



top_n_bigrams <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  head(100) # Change the number as needed
library(igraph)
# Create a vector of edge pairs (source and target)
edges <- cbind(top_n_bigrams$word1, top_n_bigrams$word2)

# Create an empty graph
top_n_bigram_graph <- graph_from_edgelist(as.matrix(edges), directed = TRUE)

# Plot the graph
set.seed(2017)
cooccur<-ggraph(top_n_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
ggsave("coocuur.png", plot = cooccur, width = 10, height = 6, dpi = 300)



#############################################################################################################################################################

#the total number of CSR reports in the GRI database for the energy sector-------
master <- as.data.frame(read_excel("C:/Users/creat/Desktop/Neethu-Resume/Dalarna-University/data visualisation/Home Exercise/list_all_pdfs.xlsx"))
View(master)
variable.names(master)

# Filter the data for the Financial Services sector
financial_services <- subset(master, Sector == "Financial Services")
View(financial_services)

# Plotting the count of reports in various years with data labels on top of bars
report_count<- ggplot(financial_services, aes(x = Year)) +
  geom_bar(stat = "count", fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Report Count for Finance Sector in Various Years",
       x = "Year",
       y = "Count") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust x-axis text
        axis.title.y = element_blank(),                      # Remove y-axis label
        axis.text.y = element_blank())
ggsave("reportcount.png", plot = report_count, width = 8, height = 4, dpi = 300)

# Load required packages
library(ggplot2)

# Count the number of reports for each combination of Region and Size
reports_by_region_size <- table(financial_services$Region, financial_services$Size)

# Convert the table to a data frame
reports_df <- as.data.frame(reports_by_region_size)
names(reports_df) <- c("Region", "Size", "Count")
View(reports_df)

# Calculate percentage
reports_df <- reports_df %>%
  group_by(Region) %>%
  mutate(Percentage = Count / sum(Count) * 100)


# Plot the stacked bar plot
sizecount <- ggplot(reports_df, aes(x = Percentage, y = Region, fill = Size)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5), size = 3) +  # Add text labels with correct position
  labs(title = "CSR Reports for Finance Sector by Region and Size",
       x = "Percentage of Reports", y = "Region",
       fill = "Size") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14)) +
  # Set custom colors for legend
  guides(fill = guide_legend(title = "Size"))  # Customize legend title

ggsave("sizecount.png", plot = sizecount, width = 8, height = 4, dpi = 300)



# Distribution of reports by Organization Type
org_type_distribution <- table(financial_services$Region,financial_services$Organization_type)
org_type_distribution <- as.data.frame(org_type_distribution)
colnames(org_type_distribution) <- c("Region","Organization Type", "Count")
view(org_type_distribution)

# Find the highest count for each region
highest_counts <- org_type_distribution %>%
  group_by(Region) %>%
  filter(Count == max(Count)) %>%
  ungroup()
org_type <- ggplot(org_type_distribution, aes(x = Region, y = Count, fill = `Organization Type`)) +
  geom_bar(stat = "identity", position = "dodge") +  # Separate bars by organization type
  geom_text(data = highest_counts, aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add data labels for highest counts
  labs(title = "Distribution of Reports by Organization Type and Region",
       x = "Region",
       y = "Count",
       fill = "Organization Type") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),  # Remove y-axis text labels
        axis.title.y = element_blank(),  # Remove y-axis title
        panel.border = element_blank())
ggsave("org_ype.png", plot = org_type, width = 8, height = 6, dpi = 300)


# Distribution of reports by Type
report_type_distribution <- table(master$Type,master$Region)
report_type_distribution <- as.data.frame(report_type_distribution)
view(report_type_distribution)
colnames(report_type_distribution) <- c("Report Type","Region", "Count")

# Calculate percentage values
report_type_distribution <- report_type_distribution %>%
  group_by(Region) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Filter out rows with 0% values
report_type_distribution_filtered <- report_type_distribution %>%
  filter(Percentage > 0)

# Plot
gri_type <- ggplot(report_type_distribution, aes(x = Region, y = Percentage, fill = `Report Type`)) +
  geom_bar(stat = "identity") +
  geom_text(data = report_type_distribution_filtered, aes(label = paste0(round(Percentage), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3) +  # Add text labels with percentage values
  labs(title = "Distribution of Report Types in Each Region",
       x = "Region",
       y = "Percentage",
       fill = "Type") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Format y-axis as percentage
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

ggsave("gri_type.png", plot = gri_type, width = 8, height = 4, dpi = 300)

