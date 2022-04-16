# ===== Preparation of data ======


# Create edx set, validation set (final hold-out test set)
# Code copied from course (whole solution needs to be in a single R file)

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(dplyr)

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
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


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

# ==== Create a train/test set from edx ======

# Train set will be 90% of edx data, 10% set aside for testing (code as above)
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
edx_test <- edx[test_index,]
rm(test_index)

# ==== RMSE =====
# loss function to assess and compare models
# based on RMSE = root mean squared error
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}


# ==== Overall average rating =====

mu_overall <- mean(edx_train$rating)

# ==== Determine (regularised?) movie effect (as in course example) =====
# add regularisation as a refinement

movie_avg <- edx_train %>% group_by(movieId) %>%
    summarise(b_movie = mean(rating - mu_overall))

# is there an effect?
# movie_avg %>% ggplot(aes(b_movie)) + geom_histogram()

# ==== Determine (regularised?) user effect (as in course example) =====
# add regularisation as a refinement

# here I need to consider the movie effects established above

user_avg <- edx_train %>% 
    left_join(movie_avg, by='movieId') %>%
    group_by(userId) %>%
    summarise(b_user = mean(rating - mu_overall- b_movie))

# is there an effect?
# user_avg %>% ggplot(aes(b_user)) + geom_histogram()

# ===== RSME with movie and user effects =====

# predicted_ratings <- edx_test %>%
#     left_join(movie_avg, by='movieId') %>%
#     left_join(user_avg, by='userId') %>%
#     mutate(pred = mu_overall + b_movie + b_user) %>%
#     pull(pred)
# sum(is.na(predicted_ratings))
# # for some reason, there are NAs
# # replace NA-predictions with mean
# predicted_ratings <- replace_na(predicted_ratings, mu_overall)
# 
# RMSE(edx_test$rating, predicted_ratings)

# ===== Establish genre effect ====== 
# (main task, my addition to model)

# treat genre combinations as factors (determining each genres' bias would be
# too complicated)
edx_train$genres <- as.factor(edx_train$genres)
genre_avg <- edx_train %>% 
    left_join(movie_avg, by='movieId') %>%
    left_join(user_avg, by='userId') %>%
    group_by(genres) %>%
    summarise(b_genre = mean(rating - mu_overall- b_movie - b_user))

# there seems to be a small effect of these genre combinations
genre_avg %>% ggplot(aes(b_genre)) + geom_histogram()

# ===== RSME with movie, user and genre effects =====

predicted_ratings <- edx_test %>%
    left_join(movie_avg, by='movieId') %>%
    left_join(user_avg, by='userId') %>%
    left_join(genre_avg, by = 'genres') %>%
    mutate(pred = mu_overall + b_movie + b_user + b_genre) %>%
    pull(pred)
sum(is.na(predicted_ratings))
# for some reason, there are NAs
# replace NA-predictions with mean
predicted_ratings <- replace_na(predicted_ratings, mu_overall)

RMSE(edx_test$rating, predicted_ratings)


# schöne Tabelle kann man so ausgeben:
# tabellenresultat %>% knitr::kable()

# ==== Validate model with validation set ======

predicted_val_ratings <- validation %>%
    left_join(movie_avg, by='movieId') %>%
    left_join(user_avg, by='userId') %>%
    left_join(genre_avg, by='genres') %>%
    mutate(pred = mu_overall + b_movie + b_user + b_genre) %>%
    pull(pred)
sum(is.na(predicted_val_ratings))
# again, replace those few NAs with the training mean
predicted_val_ratings <- replace_na(predicted_val_ratings, mu_overall)

RMSE(validation$rating, predicted_val_ratings)
