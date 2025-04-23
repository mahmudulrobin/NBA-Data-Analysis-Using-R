
#Class Work: 
#Now use Trump_Presidential_Campaign.txt data file to create word frequence and word cloud (Step 1-9)


# Install the necessary packages
install.packages("tidytext")
install.packages("dplyr")
install.packages("tidyverse") 


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidytext)
library(readr)  # For reading external files

# Step 1: Read the external file
#One way (2 lines)
file_path <- "Trump_Presidential_Campaign.txt"
text_data <- read_file(file_path)

#Another way - Both gives same result
text_data <- read_file("Trump_Presidential_Campaign.txt")

text_data

text_data <- data.frame(text_data)
text_data

View(text_data)

names(text_data)

# Step 2: Extract Sentences - (Already done since it is already a text file only - still stored in another variable for better understanding)
sentences <- text_data$text_data
print(sentences)

View(sentences)

sentences <- data.frame(sentences)

names(sentences) #Checking the column/variable names 


#Step 3: Use of Tokenization
words_token <- sentences %>% unnest_tokens(word, sentences)
print(words_token)

names(words_token)


##Step-4: Using Stop words


filtered_words <- anti_join(words_token, get_stopwords())
View(filtered_words)


#Step-5: Create a frequency table in descending order: One Way
word_frequencies <- data.frame(table(filtered_words)) %>%
  arrange(desc(Freq))

word_frequencies


#Create a table of word frequencies (Another way)

word_frequencies <- table(filtered_words)
print(word_frequencies)

#Descending Order sort
word_frequencies <- data.frame(word_frequencies)  %>% arrange(desc(Freq))
word_frequencies

#View the word frequencies
View(word_frequencies)

names(word_frequencies) #Very important to check the column names so that you can use it in visualizing graph



# Visualization:

# Step 6: Create a simple bar chart with colors

ggplot(word_frequencies, aes(x = word_frequencies$Freq, y = word_frequencies$word)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Basic bar chart with color
  labs(title = "Words with Frequency", 
       x = "Frequency", y = "Words") 


#Step 7: Word Cloud
install.packages("wordcloud2")
library(wordcloud2)


wordcloud2(word_frequencies, size=1)




#Step-8
#Filter words with a frequency of 25 or more
filtered_word_frequencies <- word_frequencies %>% filter(Freq >= 25)
print(filtered_word_frequencies)


#Step-9
#Create a barchart with words for frequency of 25 or more
#Create a simple bar chart with colors

ggplot(filtered_word_frequencies, aes(x = filtered_word_frequencies$word, y = filtered_word_frequencies$Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Basic bar chart with color
  labs(title = "Words with Frequency Greater Than or Equal to 25", x = "Words", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels









