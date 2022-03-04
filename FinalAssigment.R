##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#MY CODE

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

#Exploration of main information
head(edx)
summary(edx)



#Reviewing genre movie information
dataGR <- edx %>% separate_rows(genres, sep = "\\|")
GR_Rating <- dataGR %>% group_by(genres) %>% summarize(n=n()) %>% arrange(n)

##Convert genres to factor, to get order in graph
GR_Rating$genres <- factor(GR_Rating$genres, levels = GR_Rating$genres[order(GR_Rating$n)])


#Graph genre movies
graph_1<-ggplot(data=GR_Rating, aes(x=n, y=genres)) +
  geom_bar(stat="identity") + 
  ggtitle("Movie Quantity by Genre")
graph_1


##Detect top 10 rating movies
top10 <- edx %>% group_by(title) %>% summarize(ratingMean=mean(rating)) %>% arrange(desc(ratingMean))

##Convert title to factor, to get order in graph
top10$title <- factor(top10$title, levels = top10$title[order(top10$ratingMean)])
top10[1:10,]


##Graph
graph_2<- top10[1:10, ] %>% ggplot(aes(x=ratingMean, y=title)) +
  geom_bar(stat="identity") + 
  ggtitle("Top 10 rating movies")
graph_2 


##Detect top 10 rating genre
top10_rating <- GR_Rating %>% group_by(genres) %>% arrange(desc(n))

##Convert genres to factor, to get order in graph
top10_rating$genres <- factor(top10_rating$genres, levels = top10_rating$genres[order(top10_rating$n)])
top10_rating[1:10, ]


##Graph
graph_3<- top10_rating[1:10, ] %>% ggplot(aes(x=n, y=genres)) +
  geom_bar(stat="identity") + 
  ggtitle("Top 10 rating genres")
graph_3
##


#Define RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}

#SPLIT THE DATA. 
#it's not allowed use validations set to train, so i'm going to split edx dataset into train and test set
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
tempSet <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-tempSet,]
test_set <- edx[tempSet,]


#First Approach - just rating average
FirstApproach_Model <- mean(train_set$rating);
FirstApproach_Model

FirstApproach_Predict<- RMSE(test_set$rating,FirstApproach_Model)
FirstApproach_Predict

##Save in table the first approach
rmse_results <- tibble(method = "First Approach - Average", RMSE = FirstApproach_Predict)
rmse_results


#Second approach - using movie effect
movie_avgs <- train_set %>% group_by(movieId) %>% summarize(b_i = mean(rating - FirstApproach_Model))

SecondApproach_Model <- FirstApproach_Model + test_set %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)

SecondApproach_Predict<- RMSE(test_set$rating,SecondApproach_Model) 
SecondApproach_Predict


##Save in table the second approach
rmse_results <- bind_rows(rmse_results, data_frame(method="Second Approach - Movie Effect",  RMSE = SecondApproach_Predict))
rmse_results


##CHECK WITH VALIDATION SET
validation_Mean <- mean(validation$rating)
validation_Movie_avg <- validation %>% group_by(movieId) %>% summarize(b_i = mean(rating - validation_Mean))
validation_Model <- validation_Mean + validation %>% left_join(validation_Movie_avg, by='movieId') %>% pull(b_i)
validation_RMSE <- RMSE(validation$rating,validation_Model) 
validation_RMSE


##Save in table the comparision
rmse_results <- bind_rows(rmse_results, data_frame(method="Validation",  RMSE = validation_RMSE))
rmse_results
