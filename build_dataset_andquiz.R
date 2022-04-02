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

# Quiz about dataset

# Q1 rows and columns in edx dataset
dim(edx)

# Q2 how many 0 and 3 as ratings in edx
edx %>% filter(rating == 0) %>% summarise(n=n())
edx %>% filter(rating == 3) %>% tally()

# Q3 how many different movies are there in the edx dataset?
edx %>% group_by(movieId) %>% tally()

# bessere Buchlösung
n_distinct(edx$movieId)

# Q4 das gleiche mit users
n_distinct(edx$userId)

# Q5 how many ratings per genre
str_detect(edx$genres, "Drama") %>% sum(na.rm = TRUE)
str_detect(edx$genres, "Comedy") %>% sum(na.rm = TRUE)
str_detect(edx$genres, "Thriller") %>% sum(na.rm = TRUE)
str_detect(edx$genres, "Romance") %>% sum(na.rm = TRUE)

# Q6 which movie has the most ratings
ratings_n <- edx %>% group_by(movieId) %>% summarise(n=n())
edx %>% filter(movieId == ratings_n$movieId[which.max(ratings_n$n)])

# bessere Buchlösung
edx %>% group_by(movieId, title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))

# Q7 five ratings most given
edx %>% group_by(rating) %>% summarise(n = n()) %>% top_n(5) %>% arrange(desc(n))

# Q8 are half star ratings more common than full star ratings?
class(edx$rating)
edx$rating[1] %% 1
edx %>% filter(rating %% 1 == 0) %>% tally() > edx %>% filter(rating %% 1 != 0) %>% tally()

# Buchlösung
edx %>% group_by(rating) %>% summarize(count = n())
edx %>%
    group_by(rating) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = rating, y = count)) +
    geom_line()
