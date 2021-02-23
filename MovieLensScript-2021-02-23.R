##########################################################
## Following code written by Sari Inkila for the edX MovieLens Project
##########################################################

#Note to reader: SaveRDS and readRDS are used in the RMarkdown file, 
#but not needed when you run the plain Rscript, 
#unless you want to debug set by step

# Note: The process of loading the libraries could take a couple of minutes
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(xfun)) install.packages("xfun", repos = "http://cran.us.r-project.org")
if(!require(latexpdf)) install.packages("latexpdf", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")

#Loading libraries
library(dplyr)
library(dslabs)
library(tidyverse)
library(stringr)
library(lubridate)
library(tinytex)
library(xfun)
library(latexpdf)
library(rmarkdown)
library(ggplot2)
library(formatR)

options(digits = 7)
options(pillar.sigfig = 7)

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################
# Note: Loading of the libraries could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

#This code is directly from the assignment instructions
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# when using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use`set.seed(1)`
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

#New code - Save to file
#saveRDS(edx, file = "MovieLens_edx_2021-02-23.RDS") 
#saveRDS(validation, file = "MovieLens_validation_2021-02-23.RDS")

#read from file
#edx<- readRDS("MovieLens_edx_2021-02-23.RDS")
# Number of unique genres
n_distinct(edx$genres)

#read from file
#edx<- readRDS("MovieLens_edx_2021-02-23.RDS")

#Analyzing the genres -column data 
#Analyzing volumes and if there are any anomalies

temp1 <- edx %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(count)

# The least common
slice_head(temp1, n= 10)

#read from file
#edx<- readRDS("MovieLens_edx_2021-02-23.RDS")

#Analyzing the genres -column data 
#Analyzing volumes and if there are any anomalies

temp1 <- edx %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
# The most common
slice_tail(temp1, n= 10)

#read from file
#edx<- readRDS("MovieLens_edx_2021-02-23.RDS")

#Analyzing the genre types there were two genres from outside the standard 
#(IMAX and "no genres listed)
genre_types = c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", 
                "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", 
                "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western", 
                "IMAX", "(no genres listed)")

genres <- sapply(genre_types, function(g) {
  sum(str_detect(edx$genres, g))
})

genres <- as.data.frame(genres) %>%
  arrange(desc(genres))

slice_tail(genres , n= 20)

#read from file
#edx<- readRDS("MovieLens_edx_2021-02-23.RDS")

#Analyzing which movies have no genre listed or IMAX as genre in both data sets
temp2 <- edx %>%
  filter(genres == "(no genres listed)" | genres == "IMAX") %>%
  select(movieId, title, genres) %>%
  arrange(desc(movieId))

slice_tail(temp2 , n= 21)

#=> Conclusion: As there are very few "IMAX" and "(no genres listed)" and 
#both fall under the category "Short movies", they will be  updated to new 
#category "Short"

#read from file
#edx<- readRDS("MovieLens_edx_2021-02-23.RDS")

#creating the new fields for genres_mod
edx <- edx %>%
  mutate(genres_mod = if_else(genres == "IMAX" | genres =="(no genres listed)", "Short", genres))

#Save to file
#saveRDS(edx, file = "MovieLens_edx1_2021-02-23.RDS") 

#Read from file
#edx<- readRDS("MovieLens_edx1_2021-02-23.RDS")

#Analyzing the average rating per genre:
temp_genres <- edx %>%
  group_by(genres_mod) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres_mod = reorder(genres_mod, avg)) %>%
  ggplot(aes(x = genres_mod, y = avg, ymin = avg - 2*se, ymax = avg + 2*se))  + 
  geom_point() +
  geom_smooth()+
  geom_errorbar() + 
  ggtitle("Average rating per genre")+
  xlab("Genres") +
  theme(
    axis.text.x = element_blank(),
    panel.background = element_rect(fill = "lightblue",
                                colour = "lightblue",
                                size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "lightblue"))

temp_genres

#Read from file
#edx<- readRDS("MovieLens_edx1_2021-02-23.RDS")

# Analyzing the timestamp impact
temp_d <- edx %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(daily_rating = mean(rating), n = n(), se = sd(rating)/sqrt(n())) %>%
  ggplot(aes(x = date, y = daily_rating, ymin = daily_rating - 2*se, ymax = daily_rating + 2*se)) +
  geom_point() +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Trend of timestamp effect on rating")+
  xlab("Timestamp week") +
  theme(
  panel.background = element_rect(fill = "lightblue",
                                colour = "lightblue",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white")
  )

temp_d 

##Conclusion: There was only a weak impact on the timestamp of when the 
# rating was given. But we'll include it in to model still

##Creating a new date column (date_w) on the edx data set
edx<- edx %>% 
  mutate(date_w = as_datetime(timestamp)) %>% 
  mutate(date_w = round_date(date_w, unit = "week"))

#Save to file
#saveRDS(edx, file = "MovieLens_edx1_2021-02-23.RDS")

#Read from file
#edx<- readRDS("MovieLens_edx1_2021-02-23.RDS")

##Creating a new column for year a movie was published called year_pub

#Patterns for extracting the year of publishing
pattern1 <- "\\(\\d{4}\\)$"
pattern2 <- "\\d{4}"

#creating the new fields for year_pub
edx <- edx %>%
  mutate(year_pub = str_match(title, pattern1)) %>%
  mutate(year_pub = str_extract(year_pub, pattern2))

#Analyzing the average rating per year of publishing:
temp_year_pub <- edx %>%
  group_by(year_pub) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  mutate(year_pub = reorder(year_pub, avg)) %>%
  ggplot(aes(x = year_pub, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_smooth() +
  geom_errorbar() + 
  theme(axis.text.x = element_blank())+
  ggtitle("Average rating per year published")+
  xlab("Year published") +
  theme(
  panel.background = element_rect(fill = "lightblue",
                                colour = "lightblue",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white")
  )

temp_year_pub 

##Conclusion: There seems to be a trend that the older the movie the 
#lower the rating. We'll include the year_pub in the model development.

#Save to file
#saveRDS(edx, file = "MovieLens_edx2_2021-02-23.RDS")

#Read from file
#edx<- readRDS("MovieLens_edx2_2021-02-23.RDS")

##Building the training and test data sets from the edx data set

#Set the seed to 1
# set.seed(1) if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

#Movielens test and training sets
#Creating a 20% test set and 80% training set out of the edx data set
test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                   list = FALSE)
train_set <- edx[-test_index2,]
test_set <- edx[test_index2,]

#To make sure we don’t include users and movies in the test set that 
#do not appear in the training set, we remove these entries using 
#the semi_join function
test_set  <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(test_set, test_set)
train_set <- rbind(train_set, removed)

#Save to file
#saveRDS(train_set, file = "MovieLens_train_set_2021-02-23.RDS")
#saveRDS(test_set, file = "MovieLens_test_set_2021-02-23.RDS")

#We use the same function as used in the course 8 to compute 
#the RMSE for vectors of ratings and their corresponding predictors:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Read from file
#train_set<- readRDS("MovieLens_train_set_2021-02-23.RDS")
#test_set<- readRDS("MovieLens_test_set_2021-02-23.RDS")

## 1st model used in the course 8
#Predict the same rating for all movies regardless of user:
mu <- mean(train_set$rating)

##2nd model used in the course 8
#Modeling movie effects or movie bias by using least squares to estimate:
# I'll be using approximation of mu and biases as for performance reasons, 
#I cannot use the lm function: 
#fit <- lm(rating ~ as.factor(movieId), data = train_set)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

##3rd model used in the course 8:
#Computing an approximation of user effect or user bias
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#Save to file
#saveRDS(mu, file = "MovieLens_mu_2021-02-23.RDS")
#saveRDS(movie_avgs, file = "MovieLens_movie_avgs_2021-02-23.RDS")
#saveRDS(user_avgs, file = "MovieLens_user_avgs_2021-02-23.RDS")

#Read from file
#train_set<- readRDS("MovieLens_train_set_2021-02-23.RDS")
#test_set<- readRDS("MovieLens_test_set_2021-02-23.RDS")
#mu<- readRDS("MovieLens_mu_2021-02-23.RDS")
#movie_avgs <- readRDS("MovieLens_movie_avgs_2021-02-23.RDS")
#user_avgs <- readRDS("MovieLens_user_avgs_2021-02-23.RDS")

##Adding genre effect or genre bias to the model used in the course 8:
#Computing an approximation of movie + user + genre effects using the modified genres_mod -column
genre_avgs <-train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres_mod) %>%
  summarize(b_g = mean(rating - mu - b_i-b_u))

#Visualization of the results
genre_avgs %>%
  ggplot(aes(b_g)) +
  geom_histogram(bins = 10, color ="white")+
  ggtitle("Variation in the estimates", subtitle = "Genres effect") +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white")
  )
#We can see that these estimates vary.

#Save to file
#saveRDS(genre_avgs, file = "MovieLens_genre_avgs_2021-02-23.RDS")

#Read from file
#train_set<- readRDS("MovieLens_train_set_2021-02-23.RDS")
#test_set<- readRDS("MovieLens_test_set_2021-02-23.RDS")
#mu<- readRDS("MovieLens_mu_2021-02-23.RDS")
#movie_avgs <- readRDS("MovieLens_movie_avgs_2021-02-23.RDS")
#user_avgs <- readRDS("MovieLens_user_avgs_2021-02-23.RDS")
#genre_avgs <- readRDS("MovieLens_genre_avgs_2021-02-23.RDS")

#Computing genre, movie and user effects prediction and RMSE:
predicted_ratings_ge <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres_mod') %>%
  mutate(pred = mu + b_i + b_u +b_g) %>%
  pull(pred)
genres_effect_RMSE <- RMSE(predicted_ratings_ge, test_set$rating)

#Create a results table (following the convention used in the course 8) with target RMSE:
rmse_results <- tibble(method = "Target below", RMSE = 0.86490)

#Adding the results as a new row to the RMSE tibble
rmse_results <- bind_rows(rmse_results,
                          tibble(
                            method="Genres, Movie and User effect", 
                            RMSE = genres_effect_RMSE))
#The results
rmse_results
##=> Conclusion: Just using the Genres -column (modified) data 
#with LSE did not improve the results significantly

#Saving to file
#saveRDS(rmse_results, file = "MovieLens_rmse_results_2021-02-23.RDS")

#Read from file
#train_set<- readRDS("MovieLens_train_set_2021-02-23.RDS")
#test_set<- readRDS("MovieLens_test_set_2021-02-23.RDS")
#mu<- readRDS("MovieLens_mu_2021-02-23.RDS")
#movie_avgs <- readRDS("MovieLens_movie_avgs_2021-02-23.RDS")
#user_avgs <- readRDS("MovieLens_user_avgs_2021-02-23.RDS")
#genre_avgs <- readRDS("MovieLens_genre_avgs_2021-02-23.RDS")
#rmse_results <- readRDS("MovieLens_rmse_results_2021-02-23.RDS")

## Adding year of publishing the movie bias or effect
#Computing an approximation of year of publishing 
#the movie + movie + user + 
#genre effects using the modified year_pub -column
pub_year_avgs <-train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres_mod') %>%
  group_by(year_pub) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u - b_g))

#Visualization of the results
pub_year_avgs %>%
  ggplot(aes(b_y)) +
  geom_histogram(bins = 10, color ="white")+
  ggtitle("Variation in the estimates", subtitle = "Publising Year effect") +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                colour = "lightblue",
                                size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white")
  )

#Conclusion: Again there seems to be variation in the average ratings

#Saving to file
#saveRDS(pub_year_avgs , file = "MovieLens_pub_year_avgs_2021-02-23.RDS")

#Read from file
#train_set<- readRDS("MovieLens_train_set_2021-02-23.RDS")
#test_set<- readRDS("MovieLens_test_set_2021-02-23.RDS")
#mu<- readRDS("MovieLens_mu_2021-02-23.RDS")
#movie_avgs <- readRDS("MovieLens_movie_avgs_2021-02-23.RDS")
#user_avgs <- readRDS("MovieLens_user_avgs_2021-02-23.RDS")
#genre_avgs <- readRDS("MovieLens_genre_avgs_2021-02-23.RDS")
#pub_year_avgs <- readRDS("MovieLens_pub_year_avgs_2021-02-23.RDS")
#rmse_results <- readRDS("MovieLens_rmse_results_2021-02-23.RDS")

#Computing year of publishing the movie, genre, movie and user effects 
#prediction and RMSE:
predicted_ratings_ye <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres_mod') %>%
  left_join(pub_year_avgs, by='year_pub') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  pull(pred)

pub_year_effect_RMSE <- RMSE(predicted_ratings_ye, test_set$rating)

#Adding the results as a new row to the RMSE tibble
rmse_results <- bind_rows(rmse_results,
                          tibble(
                            method="Publishing Year, Genres, Movie and User effect", 
                            RMSE = pub_year_effect_RMSE))
rmse_results

#Saving to file
#saveRDS(rmse_results, file = "MovieLens_rmse_results_2021-02-23.RDS")

#Read from file
#train_set<- readRDS("MovieLens_train_set_2021-02-23.RDS")
#test_set<- readRDS("MovieLens_test_set_2021-02-23.RDS")
#mu<- readRDS("MovieLens_mu_2021-02-23.RDS")
#movie_avgs <- readRDS("MovieLens_movie_avgs_2021-02-23.RDS")
#user_avgs <- readRDS("MovieLens_user_avgs_2021-02-23.RDS")
#genre_avgs <- readRDS("MovieLens_genre_avgs_2021-02-23.RDS")
#pub_year_avgs <- readRDS("MovieLens_pub_year_avgs_2021-02-23.RDS")
#rmse_results <- readRDS("MovieLens_rmse_results_2021-02-23.RDS")

## Adding timestamp rounded into weeks bias or effect
#Computing an approximation of year of publishing 
#the movie + movie + user + genre effects +
#timestamp effects using the modified date_w -column
#Note! The timestamp rounded into weeks 
timestamp_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres_mod') %>%
  left_join(pub_year_avgs, by='year_pub') %>%
  group_by(date_w) %>%
  summarize(b_d = mean(rating - mu - b_i - b_u - b_g - b_y))

#Visualization of the results
timestamp_avgs  %>%
  ggplot(aes(b_d)) +
  geom_histogram(bins = 10, color ="white")+
  ggtitle("Variation in the estimates", subtitle = "Time Stamp effect") +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                colour = "lightblue",
                                size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white")
  )

#Conclusion: Again there seems to be variation in the average ratings

#Saving to file
#saveRDS(timestamp_avgs , file = "MovieLens_timestamp_avgs_2021-02-23.RDS")


#Read from file
#train_set<- readRDS("MovieLens_train_set_2021-02-23.RDS")
#test_set<- readRDS("MovieLens_test_set_2021-02-23.RDS")
#mu<- readRDS("MovieLens_mu_2021-02-23.RDS")
#movie_avgs <- readRDS("MovieLens_movie_avgs_2021-02-23.RDS")
#user_avgs <- readRDS("MovieLens_user_avgs_2021-02-23.RDS")
#genre_avgs <- readRDS("MovieLens_genre_avgs_2021-02-23.RDS")
#pub_year_avgs <- readRDS("MovieLens_pub_year_avgs_2021-02-23.RDS")
#timestamp_avgs <- readRDS("MovieLens_timestamp_avgs_2021-02-23.RDS")
#rmse_results <- readRDS("MovieLens_rmse_results_2021-02-23.RDS")

#Computing year of publishing the movie, genre, movie and user effects 
#prediction and RMSE:
predicted_ratings_ts <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres_mod') %>%
  left_join(pub_year_avgs, by='year_pub') %>%
  left_join(timestamp_avgs, by = 'date_w') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y + b_d) %>%
  pull(pred)

pub_timestamp_effect_RMSE <- RMSE(predicted_ratings_ts, test_set$rating)

#Adding the results as a new row to the RMSE tibble
rmse_results <- bind_rows(rmse_results,
                          tibble(
                            method=  
                              "Timestamp, Publishing Year, Genres, Movie and User effect", 
                            RMSE = pub_timestamp_effect_RMSE))
rmse_results

#Saving to file
#saveRDS(rmse_results, file = "MovieLens_rmse_results_2021-02-23.RDS")


#Read from file
#train_set<- readRDS("MovieLens_train_set_2021-02-18.RDS")
#test_set<- readRDS("MovieLens_test_set_2021-02-18.RDS")

##Regularization was used to improve the model performance in the course 8. 
#Regularization permits us to penalize large estimates that are formed 
#using small sample sizes
#Testing out how penalized least squares behaves in the projects data set
#with the model including timestamp, publishing year, genre biases, 
# movie bias, user bias and average rating 

#Choosing the penalty terms using cross validation
lambdas <- seq(0, 10, 0.25)

#Using the same structure for the sapply as used in the course 8 to help 
#follow the code
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i_tab <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u_tab <- train_set %>% 
    left_join(b_i_tab, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  b_g_tab<- train_set %>%
    left_join(b_i_tab, by='movieId') %>%
    left_join(b_u_tab, by='userId') %>%
    group_by(genres_mod) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  b_y_tab <-train_set %>% 
    left_join(b_i_tab, by='movieId') %>%
    left_join(b_u_tab, by='userId') %>%
    left_join(b_g_tab, by='genres_mod') %>%
    group_by(year_pub) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u - b_g)/(n()+l))
  b_d_tab <- train_set %>%
    left_join(b_i_tab, by='movieId') %>%
    left_join(b_u_tab, by='userId') %>%
    left_join(b_g_tab, by='genres_mod') %>%
    left_join(b_y_tab, by='year_pub') %>%
    group_by(date_w) %>%
    summarize(b_d = sum(rating - mu - b_i - b_u - b_g-b_y)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i_tab, by = 'movieId') %>%
    left_join(b_u_tab, by = 'userId') %>%
    left_join(b_g_tab, by='genres_mod') %>%
    left_join(b_y_tab, by='year_pub') %>%
    left_join(b_d_tab, by='date_w') %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y + b_d) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

#Visualizing lamdas
qplot(lambdas, rmses)

#Extracting the minimum value for the lambda
lambda <- lambdas[which.min(rmses)]

#Save to file
#saveRDS(min(rmses), file = "MovieLens_rmses_2021-02-23.RDS")
#saveRDS(lambda, file = "MovieLens_lambda_2021-02-23.RDS")

lambda

#Read from file
#rmse_results <- readRDS("MovieLens_rmse_results_2021-02-23.RDS")

#Adding the results as a new row to the RMSE tibble
#RMSE <- readRDS("MovieLens_rmses_2021-02-23.RDS")
rmse_results <- bind_rows(rmse_results,
                          tibble(
                            method= "Regularized combined effects Model", 
                            RMSE = min(rmses)))
rmse_results

#Saving to file
#saveRDS(rmse_results, file = "MovieLens_rmse_results_2021-02-23.RDS")


##########################################################
## Verifying the model with validation data set
##########################################################

#Read from file
#edx<- readRDS("MovieLens_edx2_2021-02-23.RDS")
#validation<- readRDS("MovieLens_validation_2021-02-23.RDS")
#l <- readRDS("MovieLens_lambda_2021-02-23.RDS")
#rmse_results <- readRDS("MovieLens_rmse_results_2021-02-23.RDS")

#creating the new field for genres_modin validation data set
validation <- validation %>%
  mutate(genres_mod = if_else(genres == "IMAX" | 
                                genres =="(no genres listed)",
                              "Short", genres))
##Creating a new date column on the validation data set
validation <- validation %>%
  mutate(date_w = as_datetime(timestamp)) %>% 
  mutate(date_w = round_date(date_w, unit = "week"))

#Creating a new column for year movie was published
#Patterns for extracting the year of publishing
pattern1 <- "\\(\\d{4}\\)$"
pattern2 <- "\\d{4}"

#creating the new field for year_pub
validation1 <- validation %>%
  mutate(year_pub = str_match(title, pattern1)) %>%
  mutate(year_pub = str_extract(year_pub, pattern2))

#save to file
#saveRDS(validation1, file = "MovieLens_validation1_results_2021-02-23.RDS")

#Calculating mu
mu <- mean(edx$rating)

#Regularized averages for Movie effect
b_i_tab <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i_reg = sum(rating - mu)/(n()+l))

#Regularized averages for User effect
b_u_tab <- edx %>% 
  left_join(b_i_tab, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_reg = sum(rating - b_i_reg - mu)/(n()+l))

#Regularized averages for the movie, user and genres effects
b_g_tab <- edx %>%
  left_join(b_i_tab, by='movieId') %>%
  left_join(b_u_tab, by='userId') %>%
  group_by(genres_mod) %>%
  summarize(b_g_reg = sum(rating - mu- b_i_reg - b_u_reg)/(n()+l))

#Regularized averages for the publishing year, movie, user and genres effects
b_y_tab <-edx %>% 
  left_join(b_i_tab, by='movieId') %>%
  left_join(b_u_tab, by='userId') %>%
  left_join(b_g_tab, by='genres_mod') %>%
  group_by(year_pub) %>%
  summarize(b_y_reg = sum(rating - mu - b_i_reg - b_u_reg - b_g_reg)/(n()+l))

#Regularized averages for the timestamp rounded into weeks, publishing year, movie, user and genres effects
b_d_tab <- edx %>%
  left_join(b_i_tab, by='movieId') %>%
  left_join(b_u_tab, by='userId') %>%
  left_join(b_g_tab, by='genres_mod') %>%
  left_join(b_y_tab, by='year_pub') %>%
  group_by(date_w) %>%
  summarize(b_d_reg = 
              sum(rating - mu - b_i_reg - b_u_reg - b_g_reg - b_y_reg)/
              (n()+l))


## Verifying the model with validation data sets
#Computing timestamp, year of publishing the movie, genre, movie 
#and user effects and average rating
#prediction and RMSE:
predicted_ratings_valid <- validation1 %>% 
  left_join(b_i_tab, by = 'movieId') %>%
  left_join(b_u_tab, by = 'userId') %>%
  left_join(b_g_tab, by='genres_mod') %>%
  left_join(b_y_tab, by='year_pub') %>%   
  left_join(b_d_tab, by='date_w') %>%
  mutate(pred = 
           mu + b_i_reg  + b_u_reg  + b_g_reg  + b_y_reg + b_d_reg) %>%
  pull(pred)


validation_RMSE <- RMSE(predicted_ratings_valid, validation1$rating)

#Adding the results as a new row to the RMSE tibble
rmse_results <- bind_rows(rmse_results,
                          tibble(
                            method = "Final model on validation", 
                            RMSE = validation_RMSE))

rmse_results 

#Saving to file
#saveRDS(rmse_results, file = "MovieLens_rmse_results2_2021-02-23.RDS")
#saveRDS(validation_RMSE, file = "MovieLens_validation_RMSE2_2021-02-23.RDS")

#Now we have achieved the projects target of developing a model that performs better on the validation data set than a residual mean squared error (RMSE) lower than 0.86490 as the achieved RMSE was: 
validation_RMSE

