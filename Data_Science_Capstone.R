
##################################################################################

#             Welcome to Data Science: Capstone

##################################################################################

# This, the final course in the HarvardX Professional Certificate in Data Science, is very 
# different from the previous courses in the series. Unlike the rest the courses in the 
# Professional Certificate Program, in this course you will receive much less guidance from 
# the instructors. In this capstone course, you will show what you’ve learned so far by 
# working independently on data science projects of your own.

# To become an expert data scientist, you need practice and experience. By completing this 
# capstone course, you will get an opportunity to apply the knowledge base and skills in R 
# data analysis you have gained throughout the series, including data visualization, 
# probability, inference and modeling, data wrangling, data organization, regression, and 
# machine learning. If you complete the capstone, you will have a data product to show off 
# to potential employers or educational programs.

# This capstone course assumes that you have completed the previous eight courses in the 
# series or have gained the equivalent experience elsewhere.
# The class notes for this course series can be found in Professor Irizarry's freely 
# available Introduction to Data Science book , which can also be found in Spanish on LeanPub .
# In this course, you will learn:
#         ◦ How to apply the knowledge base and skills learned throughout the series to 
            # real-world problems
#         ◦ How to independently work on a data analysis project
# Course overview
# In this course, you will prepare and submit your own data analysis project for peer review
# (all learners) and prepare and submit your own data analysis project for peer and TA review 
# (Verified track learners only).
# Are you new to edX (although at this point in the course series, you shouldn't be!)? 
# Check out edx's Demo Course!
# Need help? Visit edX Support via the Support tab or visit the Help Center.

# Essential Course Information

# Course Syllabus
# The course syllabus contains a more comprehensive version of the information below - but 
# if you don't read the syllabus, please read this!
#   Course Structure
# This is a self-paced course. You can work on it with your own timeline in mind. We 
# anticipate that you could complete each project in this course in a couple of weeks by 
# working on them a little bit at a time each day or you could complete each project in a 
# week by spending a large amounts of concentrated time on them. It's up to you!
# Check the course home page for important dates. If you are interested in pursuing a 
# Verified Certificate, you will need to upgrade before the date listed on the home page and
# finish all assignments before their due dates. Note that because this course includes a
# peer grading component, you will need to submit your project before the final course close
# date in order to allow for grading.
# Course Textbook
# There is a free PDF textbook available here in English  and here in Spanish . (Note: The 
# book is "free" in that you can slide the "YOU PAY" scale to $0. You are welcome to pay 
# what you can afford, and there is no advantage in the course to anyone that "purchases" 
# the book for more money.)
# There is also an HTML version of the textbook here .
# Grading
# This course is very different from previous courses in the series in terms of grading. 
# There are three graded components to this course: the Movielens prep quiz (10% of your 
# grade), the Movielens project (40% of your grade), and the choose-your-own project (50% of
# your grade, available to Verified learners only).
# Passing Rate
# The passing rate is 70%. You must sign up for a Verified Certificate and earn a grade of 70% 
# of higher in order to obtain a certificate for this course.
# Where to Get Answers to Questions About Course Structure
# First, check the Syllabus and FAQs.
# If you still can't find your answer:
#   ◦ Click on "Discussion" (at the top of this page).
# ◦ Post your question in the "General" discussion forum.
# Where to Get Answers to Questions About Course Content
# You should first strive to find answers on your own. Follow these steps:
#   ◦ Review the course content from the course series and/or search for an answer on the
    # web. Think critically!
#   ◦ If you can't find an answer on your own, post to the topic discussion forum most 
#     appropriate for your question.
# The edX Platform
# If you are unfamiliar with the edX platform (although at this point in the course series,
# we expect you to be quite familiar with the platform!), you may wish to check out the edX 
# Demo Course before proceeding.
# 

# Project Overview: MovieLens

# For this project, you will be creating a movie recommendation system using the MovieLens 
# dataset. The version of movielens included in the dslabs package (which was used for some 
# of the exercises in PH125.8x: Data Science: Machine Learning) is just a small subset of a 
# much larger dataset with millions of ratings. You can find the entire latest MovieLens 
# dataset here. You will be creating your own recommendation system using all the tools we 
# have shown you throughout the courses in this series. We will use the 10M version of the 
# MovieLens dataset to make the computation a little easier.
# You will download the MovieLens data and run code we will provide to 
# generate your datasets.

# First, there will be a short quiz on the MovieLens data. You can view this quiz as an 
# opportunity to familiarize yourself with the data in order to prepare for your project 
# submission.
# Second, you will train a machine learning algorithm using the inputs in one subset to 
# predict movie ratings in the final hold-out test set. Your project itself will be assessed
# by peer grading.

###########################################################################################

# Create Train and Final Hold-out Test Sets
# 
# Introduction
# You will use the following code to generate your datasets. Develop your algorithm using 
# the edx set. For a final test of your final algorithm, predict movie ratings in the
# final_holdout_test set as if they were unknown. RMSE will be used to evaluate how close 
# your predictions are to the true values in the final_holdout_test set.

# Important: The final_holdout_test data should NOT be used for training, developing, or 
# selecting your algorithm and it should ONLY be used for evaluating the RMSE of your final 
# algorithm. The final_holdout_test set should only be used at the end of your project with 
# your final model. It may not be used to test the RMSE of multiple models during model 
# development. You should split the edx data into separate training and test sets and/or use
# cross-validation to design and test your algorithm.

# Also remember that by accessing this site, you are agreeing to the terms of the edX Honor 
# Code. This means you are expected to submit your own work and can be removed from the 
# course for substituting another student's work as your own.



# 2 Methods and Analysis
#2.1 Data Preparation

# Create edx and final_holdout_test sets

#########################################################
# Create edx and final_holdout_test sets 
########################################################

# Install Libraries (if needed)

# Install CRAN packages (if not already installed)
#.inst <- .packages %in% installed.packages()
#if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
#lapply(.packages, require, character.only=TRUE)





#if(!require(raster)) install.packages("raster", repos='https://rspatial.r-universe.dev')

if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

#tinytex::install_tinytex()

if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#library(raster)
library(tinytex)
library(knitr)
library(rmarkdown)
library(tidyverse)
library(caret)


# Customize knitr output

#Set Thousands Separator for inline output
knitr::knit_hooks$set(inline = function(x) { if(!is.numeric(x)){ x }else{ prettyNum(round(x,2), big.mark=",") } })



# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)
# DownLoad the data

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Case where userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Seeding for all randomness used in this script, hence results can be duplicated
set.seed(2023, sample.kind = "Rounding")

# Setting consistency and eloquently theme for all plots generated in this script
theme_set(theme_classic())






##############################################################

# Data exploration

### The following questions are part of the Quiz which is ###
### 10% of the project. I'll make sure no duplicate answers ######

# How many zeroes & 3s

zeros <- sum(edx$rating == 0)
threes <- sum(edx$rating == 3)
## Q2
# How many zeros were given as ratings in the edx dataset?
# How many threes were given as ratings in the edx dataset?
print(zeros)
# [1] 0
print(threes)
# [1] 2121240

# Q3
# How many different movies are in the edx dataset?
uniqueMovies <- length(unique(edx$movieId))
n_distinct(edx$movieId)
# Answer: [1] 10677

# Q4
# How many different users are in the edx dataset?
uniqueUsers <- n_distinct(edx$userId)
# Answer: 69878

# Q5
# How many movie ratings are in each of the 
# following genres in the edx dataset?
List_of_genre <- c('Drama', 'Comedy', 'Thriller', 'Romance')
Nbr_of_genres <- sapply(List_of_genre, function(g){
  edx %>% filter(str_detect(genres, g)) %>% tally()
})

edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n())

# Answer: Drama            3910127
# Answer: Comedy           3540930
# Answer: Thriller         2325899
# Answer: Romance          1712100
# Q6
# Which movie has the greatest number of ratings?
N_Ratings <- edx %>% group_by(movieId) %>% 
  summarize(N_Ratings = n(), movieTitle = first(title)) %>%
  arrange(desc(N_Ratings)) %>%
  top_n(10, N_Ratings)

print(N_Ratings)

# Answers:
#    movieId N_Ratings movieTitle                                    
#    296      31362 Pulp Fiction (1994)    (# Answer:)                                     
#    356      31079 Forrest Gump (1994)                                         
#    593      30382 Silence of the Lambs, The (1991)                            
#    480      29360 Jurassic Park (1993)                                        
#    318      28015 Shawshank Redemption, The (1994)                            
#    110      26212 Braveheart (1995)                                           
#    457      25998 Fugitive, The (1993)                                        
#    589      25984 Terminator 2: Judgment Day (1991)                           
#    260      25672 Star Wars: Episode IV - A New Hope (a.k.a. Star Wars) (1977)
#    150      24284 Apollo 13 (1995)

# Q7
# What are the five most given ratings in order from most to least?
N_Ratings <- edx %>% group_by(rating) %>% 
  summarize(number = n())

N_Ratings %>% top_n(5) %>% arrange(desc(number))
# Answer: A
# rating   number
# 1    4   2588430
# 2    3   2121240
# 3    5   1390114
# 4    3.5  791624
# 5    2    711422
# Q8
# True or False: In general, half star ratings are less common than whole star
# ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or
# 4, etc.).
N_Ratings %>%
  mutate(halfStar = rating %% 1 == 0.5) %>%
  group_by(halfStar) %>%
  summarize(number = sum(number))
# Answer:
# A tibble: 2 × 2
# halfStar  number
# <lgl>      <int>
# 1 FALSE    7156885
# 2 TRUE     1843170
##End Quiz############################## 



############################################################
# 2.2 Data Exploration
#Exploring the Data Set (EDA) #

# Before we go any further (build the model), it is important
# to understand the schema of the data, distribution of ratings
# and the relationship of the predictors. Hence my journey toward 
# a better model.
str(edx)
 
# 'data.frame':	9000055 obs. of  7 variables:
# $ userId    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId   : int  122 185 292 316 329 355 356 362 364 370 ...
# $ rating    : num  5 5 5 5 5 5 5 5 5 5 ...
# $ timestamp : int  838985046 838983525 838983421 838983392 838983392 838984474 838983653 838984885 838983707 838984596 ...
# $ title     : chr  "Boomerang (1992)" "Net, The (1995)" "Outbreak (1995)" "Stargate (1994)" ...
# $ genres    : chr  "Comedy|Romance" "Action|Crime|Thriller" "Action|Drama|Sci-Fi|Thriller" "Action|Adventure|Sci-Fi" ...
# $ year_rated: num  1996 1996 1996 1996 1996 ...


# From the initial of the EDA, we notice that edx has 7 columns
# described as follow:
# 
# •  "user_id : int" : a unique identifier of the user who made the rating

# • "movie_id : int ": a unique identifier of the rated movie

# •  “rating  : num “:  the score of the rating on a five-star scale  

# •  "timestamp : int": the timestamp of the ratings, represented in seconds since             midnight Coordinated Universal Time (UTC) of January 1, 1970

# •  “title : chr”: the title of the rated movie with the release year in parentheses 

# •  "genres : chr": a sequence of genres to which the rated movie belongs

# •  “year_rated: num”:  1996 1996 1996 1996 1996 …


# Number of rows and columns in the edx dataset?
###Check Dimensions (rows and columns ) of both final_holdout_test and train tables
cat("\nEdx (it contents):",dim(edx))

# Answer: Edx (it contents): 9000055 7

cat("\nfinal holdout test (it contents):",dim(final_holdout_test))

# Answer: final holdout test (it contents): 999999 6

# The following table shows the schema and content of edx dataset
head(edx)
# 
#   userId movieId rating timestamp              title
# 1      1     122      5 838985046              Boomerang (1992)
# 2      1     185      5 838983525              Net, The (1995)
# 4      1     292      5 838983421              Outbreak (1995)
# 5      1     316      5 838983392              Stargate (1994)
# 6      1     329      5 838983392              Star Trek: Generations (1994)
# 7      1     355      5 838984474              Flintstones, The (1994)
#                  genres               year_rated
# 1                Comedy|Romance       1996
# 2         Action|Crime|Thriller       1996
# 4  Action|Drama|Sci-Fi|Thriller       1996
# 5       Action|Adventure|Sci-Fi       1996
# 6 Action|Adventure|Drama|Sci-Fi       1996
# 7       Children|Comedy|Fantasy       1996


# Dissecting genres
# 2.2.1 No specific Genres(mix genres) and Genres
# The data set contains 797 different combinations of genres. 
# The following table contents the first list of genres.

edx %>% group_by(genres) %>% 
  summarise(n=n()) %>%
  head()

# genres                                                   n
# <chr>                                                <int>
#   1 (no genres listed)                                   7
# 2 Action                                             24482
# 3 Action|Adventure                                   68688
# 4 Action|Adventure|Animation|Children|Comedy          7467
# 5 Action|Adventure|Animation|Children|Comedy|Fantasy   187
# 6 Action|Adventure|Animation|Children|Comedy|IMAX       66

# Here is the second list of genre in an orderly fashions.
tibble(count = str_count(edx$genres, fixed("|")), genres = edx$genres) %>% 
  group_by(count, genres) %>%
  summarise(n = n()) %>%
  arrange(-count) %>% 
  head()

# count genres                                                      n
# <int> <chr>                                                 <int>
# 1     7 Action|Adventure|Comedy|Drama|Fantasy|Horror|Sci-Fi|…     256
# 2     6 Adventure|Animation|Children|Comedy|Crime|Fantasy|My…   10975
# 3     6 Adventure|Animation|Children|Comedy|Drama|Fantasy|My…     355
# 4     6 Adventure|Animation|Children|Comedy|Fantasy|Musical|…     515
# 5     5 Action|Adventure|Animation|Children|Comedy|Fantasy        187
# 6     5 Action|Adventure|Animation|Children|Comedy|IMAX            66

#Sui generis genre (unique genre)
unique_genre <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) 
print(unique_genre)
# 
# > print(unique_genre)
# # A tibble: 20 × 2
#   genres               count
# <chr>                <int>
#  1 Drama              3910127
#  2 Comedy             3540930
#  3 Action             2560545
#  4 Thriller           2325899
#  5 Adventure          1908892
#  6 Romance            1712100
#  7 Sci-Fi             1341183
#  8 Crime              1327715
#  9 Fantasy             925637
# 10 Children            737994
# 11 Horror              691485
# 12 Mystery             568332
# 13 War                 511147
# 14 Animation           467168
# 15 Musical             433080
# 16 Western             189394
# 17 Film-Noir           118541
# 18 Documentary          93066
# 19 IMAX                  8181
# 20 (no genres listed)       7

# Noticing that the dataset  displays 20 different genres & 
# 7 other movies that have not listed as genres whatsoever 
# (7 movies without genres)


# 2.2.2 Date conversion and rating period of time
#Convert Timestamp to year
edx <- mutate(edx, year_rated = year(as_datetime(timestamp)))
head(edx)


# The following Histogram shows the Rating Distribution Per Year
# Period of collecting rating that started over the years
if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")
edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "dark green") + 
  ggtitle("Annual Rating/Review Distribution") +
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma) + 
  theme_economist()

#The above plot shows the Rating Distribution Per Year, period
#which collect rating that started over the years


#Edx: In depth dissecting (period during which more rating
# took place)
edx %>% mutate(date = date(as_datetime(timestamp, origin="1970-01-01"))) %>%
  group_by(date, title) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head(15)

# A tibble: 15 × 3
# Groups:   date [4]
#   date       title                                             count
# 1 1998-05-22 Chasing Amy (1997)                                322
# 2 2000-11-20 American Beauty (1999)                            277
# 3 1999-12-11 Star Wars: Episode IV - A New Hope (a.k.a. Sta…   254
# 4 1999-12-11 Star Wars: Episode V - The Empire Strikes Back…   251
# 5 1999-12-11 Star Wars: Episode VI - Return of the Jedi (19…   241
# 6 2005-03-22 Lord of the Rings: The Two Towers, The (2002)     239
# 7 2005-03-22 Lord of the Rings: The Fellowship of the Ring,…   227
# 8 2000-11-20 Terminator 2: Judgment Day (1991)                 221
# 9 1999-12-11 Matrix, The (1999)                                210
# 10 2000-11-20 Jurassic Park (1993)                             201
# 11 2000-11-20 Braveheart (1995)                                197
# 12 2000-11-20 Star Wars: Episode VI - Return of the Jedi (19…  194
# 13 2000-11-20 Star Wars: Episode IV - A New Hope (a.k.a. Sta…  193
# 14 2000-11-20 Star Wars: Episode V - The Empire Strikes Back…  192
# 15 2005-03-22 Shrek (2001)                                     192


#Rating Frequency
  # edx %>%
  #   group_by(rating) %>%
  #   summarize(count = n()) %>%
  #   ggplot(aes(x = rating, y = count)) +
  #   geom_line() +
  #   ggtitle("Number of frequency/occurence for each rating")

#Graphically speaking let’s see which movies have more ratings
#than the average mu
mu <- mean(edx$rating)
edx %>% group_by(title) %>%
  summarize(b_i = mean(rating - mu), n = n()) %>% filter(b_i > 0.5, n > 10000) %>%
  ggplot(aes(reorder(title, b_i), b_i, fill = n)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = "PuBuGn") +
  ggtitle("") + xlab("Movie Title") +
  ggtitle("Movie rating - mu,\nfor highest-rated movies that at least 10000 ratings \nor more, wow!!!") +
  theme_classic()

# The above graph/plot shows the  highest rated movies

#########We begin: Training and Testing Sets:###################

#Training and Testing Sets:
set.seed(2023, sample.kind = "Rounding")
test_index <-createDataPartition(y = edx$rating, times = 1, p = 0.1, list = F)

train_data <-edx[-test_index,]
edx_temp <-edx[test_index,]

# Case where userId and movieId are in both the training and testing sets
test_data <-edx_temp %>% semi_join(train_data, by = "movieId") %>%
  semi_join(train_data, by = "userId")

#Adding removed Rows from the edx_test back into train_data
add_rows_back <-anti_join(edx_temp, test_data)
train_data <-rbind(train_data, add_rows_back)
rm(train_data, test_index, add_rows_back)


#Cleaning and Analyzing the Data
##Data sui generis (unique). Ensure that data like userIds, movieIds, and genres are not duplicated
edx %>% as_tibble()


# Ensure that data are not duplicated (userIds, movieIds, and 
# genres are: sui generis (unique))
edx %>% summarize(unique_users = length(unique(userId)),
                  unique_movies = length(unique(movieId)),
                  unique_genres = length(unique(genres)))

#   unique_users unique_movies unique_genres
# 1        69878         10677           797


########More Ratings: #######################################################

#Here we go again for more ratings
#Extracting the first date and calculate the age of the movie. 
# Find out if the age of the movie effects predicted ratings.

#Bring the first date to light
first_date <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric()


#Adding the first date
title_dates <- edx %>% mutate(first_date = first_date)
head(title_dates)

#Get rid of timestamp
title_dates <- title_dates %>% select(-timestamp)

head(title_dates)

# 
# userId movieId rating                 title
# 1      1     122      5              Boomerang (1992)
# 2      1     185      5               Net, The (1995)
# 4      1     292      5               Outbreak (1995)
# 5      1     316      5               Stargate (1994)
# 6      1     329      5 Star Trek: Generations (1994)
# 7      1     355      5       Flintstones, The (1994)
#           genres                year_rated first_date
# 1                Comedy|Romance       1996       1992
# 2         Action|Crime|Thriller       1996       1995
# 4  Action|Drama|Sci-Fi|Thriller       1996       1995
# 5       Action|Adventure|Sci-Fi       1996       1994
# 6 Action|Adventure|Drama|Sci-Fi       1996       1994
# 7       Children|Comedy|Fantasy       1996       1994
# 

#What is the overall mean rating? Here it is:
overall_mean <- mean(edx$rating)
print(overall_mean)
#Answer: 3.512465

#Now let see if dates are correct 
title_dates %>% filter(first_date > 2018) %>% group_by(movieId, title, first_date) %>% summarize(n = n())


title_dates %>% filter(first_date < 1900) %>% group_by(movieId, title, first_date) %>% summarize(n = n())


#Now let find the age of the movies
title_dates <- title_dates %>% mutate(age_of_movie = 2018 - first_date, 
                                                        rating_date_range = year_rated - first_date)
head(title_dates)


# Skip the graph here.....

#Rating average: movies, users, average rating by age of movie, average rating by year

#Movie rating averages
movie_avgs <- title_dates %>% group_by(movieId) %>% summarize(avg_movie_rating = mean(rating))
user_avgs <- title_dates %>% group_by(userId) %>% summarize(avg_user_rating = mean(rating))
year_avgs <- title_dates%>% group_by(year_rated) %>% summarize(avg_rating_by_year = mean(rating)) #year the movie was rated
age_avgs <- title_dates %>% group_by(age_of_movie) %>% summarize(avg_rating_by_age = mean(rating)) #age of movie
head(age_avgs)

head(user_avgs)

# A tibble: 6 × 2
#age_of_movie avg_rating_by_age
# 1        -6982              2.80
# 2        -2982              2.30
# 3         -982              3.48
# 4          -28              3.73
# 5            8              3.37
# 6           10              3.46


#head(user_avgs)
# A tibble: 6 × 2
# userId  avg_user_rating
#1      1            5   
#2      2            3.29
#3      3            3.94
#4      4            4.06
#5      5            3.92
#6      6            3.95


# Up to here is good'

# Glakge is h

# EDA (exploratory Data Analysis)
#cat("\nTrain set dimension :",dim(edx))
#cat("\nNumber of unique movies :",edx$movieId %>% unique() %>% length())
#cat("\nNumber of unique users :",edx$userId %>% unique() %>% length())


#Different movies for different genres
cat("\nDifferent movies for different genres :\n")
genres <- c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

#   Drama   Comedy Thriller  Romance 
# 3910127  3540930  2325899  1712100 

#Most rated movies 
edx %>% group_by(movieId) %>%
  summarise(n_ratings=n(), title=first(title)) %>%
  top_n(5, n_ratings)

# A tibble: 3 × 3
#movieId n_ratings title   
#  296     31362   Pulp Fiction (1994)             
#  318     28015   Shawshank Redemption, The (1994)
#  356     31079   Forrest Gump (1994)             
#  480     29360   Jurassic Park (1993)            
#  593     30382   Silence of the Lambs, The (1991)
#> 

#Most often ratings (10 top ones)
edx %>% group_by(rating) %>%
  summarise(n_ratings=n()) %>%
  top_n(10, n_ratings) %>%
  arrange(desc(n_ratings))

# A tibble: 10 × 2
# rating    n_ratings
# 1    4     2588430
# 2    3     2121240
# 3    5     1390114
# 4    3.5    791624
# 5    2      711422
# 6    4.5    526736
# 7    1      345679
# 8    2.5    333010
# 9    1.5    106426
# 10   0.5    85374


#Rating Frequency
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line() +
  ggtitle("Number of frequency/occurence for each rating")

# Noticing that the most common rating is 4, and the least 
# common is 0. 


######OKAY###########


# Way of ratings (number of stars)
way <-  ifelse((edx$rating == 1 |edx$rating == 2 | edx$rating == 3 | 
                  edx$rating == 4 | edx$rating == 5) ,
               "Full_Star", 
               "Half_Star") 

ratings_way <- data.frame(edx$rating, way)

head(ratings_way)
#print(ratings_way)


#Plotting/Histogram of ratings
ggplot(ratings_way, aes(x= edx.rating, fill = way)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  scale_fill_manual(values = c("half_star"="yellow", "full_star"="green")) +
  labs(x="rating", y="number of ratings", caption = " According to edx data: set") +
  ggtitle("Histogram : Ratings of rating (number of ratings for each rating)")

#Plotting/Histogram shows that no zero (0) rating, most 
#ratings are: 4, 3, 5, 3.5 and 2 and the half star ratings
#are less likely than whole star ratings.


#Top Title
top_title <- edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(25,count) %>%
  arrange(desc(count))

#Bar plot of top 25 title
top_title %>% 
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat='identity', fill="dark green") + coord_flip(y=c(0, 40000)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top 25 movies title based \n on number of ratings" , caption = "According to edx: data set")

#We notice that movies with  the highest number of ratings are
#in the top genres categories  such as  Jurrasic Park(1993),
#Pulp fiction (1994), Forrest Gump(1994) which are in the top 
#of movie’s ratings number, are part of the Drama, Comedy or 
#Action genres. This is what we call blockbusters movies

########OKAY##########

# Histogram/Plotting for number of ratings by movieId
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = "green") +
  scale_x_log10() + 
  ggtitle("Visualization: Movies") +
  labs(subtitle  ="movies ratings (by movieId)", 
       x="movieId" , 
       y="Number of Ratings", 
       caption ="According to data collection from: edx set") +
  theme(panel.border = element_rect(colour="black", fill=NA)) 
# Noticing that "Reviews" between movies are either less than
#1000 or more than 10k. Yes indeed some of them are rated more 
#than others, because many movies are watched by few users and 
#movies like blockbusters have a big impact when it comes to ratings.


# The following table is example of how most users rate few movies.
# few users rate more than a thousand movies.
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  head()

# A tibble: 6 × 2
#       userId        n
#      62516         10
#      22170         12
#      15719         13
#      50608         13
#        901         14
#       1833         14


# Histogram/Plotting for number of ratings by userId
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = "blue") +
  scale_x_log10() + 
  ggtitle("Visualization: Users") +
  labs(subtitle ="users ratings (by UserId)", 
       x="Users" , 
       y="Number of Ratings") +
  theme(panel.border = element_rect(colour="black", fill=NA)) 

# Noticing that some users wrote 100 reviews or less, some
# 1k or more.

# Working with timestamp

#Noticing that the edx set contains the timestamp variable
#which represents the time and data in which the rating was
#provided. The units (seconds) are important dated back 
#January 1, 1970..

edx %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Timestamp, time unit : week")+
  labs(subtitle = "average ratings",
       caption = "According to edx: data set")



### DATA WRANGLING ###

#Splitting EDX to train and test data
set.seed(1998, sample.kind="Rounding")
B <- 100000
edx_sample <- edx[sample(nrow(edx), B, replace = FALSE),]

# Splitting of edx sample on a 80/20 ratio for training purpose
train_index <- createDataPartition(edx_sample$rating, times=1, p=0.8,list=FALSE)
train <- edx_sample[train_index,]
test <- edx_sample[-train_index,]


# edx <- mutate(edx, year_rated = year(as_datetime(timestamp)))

#Wrangling

train$timestamp <- year(as_datetime(train$timestamp))


#extracting release year from title
p <- "(?<=\\()\\d{4}(?=\\))"
train$release_year <- train$title %>% str_extract(p) %>% as.integer()

#Encode the genres column
# str_split(string, p, n = Inf, simplify = FALSE)
# simplify = TRUE
train$genres <- str_split(train$genres, p="\\|")
genre_of_genres <- enframe(train$genres) %>%
  unnest(value) %>%
  mutate(temp = 1) %>%
  pivot_wider(names_from = value, values_from = temp, values_fill = list(temp = 0))
train <- cbind(train, genre_of_genres) %>% select(-name)
train$genres <- NULL

#Adding average rating  for each movie by subtracting the total average rating
avg_rating <- mean(train$rating)
movie_score <- train %>% group_by(movieId) %>%
  summarise(movie_score = mean(rating-avg_rating))

#Adding  average rating for each user by subtracting the total average rating and movie score
user_score <- train %>% left_join(movie_score, by="movieId") %>%
  mutate(movie_score = ifelse(is.na(movie_score), 0, movie_score)) %>%
  group_by(userId) %>%
  summarise(user_score = mean(rating-avg_rating-movie_score)) 

train <- train %>% left_join(user_score) %>% left_join(movie_score)

head(train)


#Apply same scenario(wrangling) to the test set

#Convert timestamp to datetime by using only the year
test$timestamp <- year(as_datetime(test$timestamp))

#extracting release year from title
p <- "(?<=\\()\\d{4}(?=\\))"
test$release_year <- test$title %>% str_extract(p) %>% as.integer()

#Encoding genres column
test$genres <- str_split(test$genres, p="\\|")
genre_of_genres <- enframe(test$genres) %>%
  unnest(value) %>%
  mutate(temp = 1) %>%
  pivot_wider(names_from = value, values_from = temp, values_fill = list(temp = 0))
test <- cbind(test, genre_of_genres) %>% select(-name)
train$genres <- NULL

#Adding & removing data (add missing columns of genres that are absent  in test set, and 
#get rid of those that are not in the train set)
for(col in names(train)){
  if(!col %in% names(test)){
    test$newcol <- 0
    names(test)[names(test)=="newcol"] <- col
  }
}
for(col in names(test)){
  if(!col %in% names(train)){
    test[,col] <- NULL
  }
}

#Average scores on the train set of each movie and user
test$user_score <- NULL
test$movie_score <- NULL
test <- test %>% left_join(user_score, by="userId") %>% left_join(movie_score, by="movieId")


test <- test %>% mutate(user_score = ifelse(is.na(user_score), 0, user_score)) %>% mutate(movie_score = ifelse(is.na(movie_score), 0, movie_score))

#Reordering .....
test <- test %>% select(names(train))
head(test)


# BUILDING of ML MODELS

# Baseline comparison .......
y_hat <- mean(train$rating)
result <- RMSE(test$rating, y_hat)
cat("RMSE :", result)

# RMSE : 1.051226

# RMSE is 1.051226 meaning on average the prediction is 
# OBOE (off-by-one error) which is not so good


#Building of ML (Machine Learning) Models

#Linear Model

#?
#Linear Model using timestamp, user_score, release_year, and
#movie_score

control <- trainControl(method = "none")
fit_lm <- train(rating~user_score+movie_score+timestamp+release_year, data=train, method="lm", trControl=control)
print(fit_lm$finalModel)


y_hat <- predict(fit_lm, test)
result2 <- RMSE(test$rating, y_hat)
cat("RMSE :", result2)

# # RMSE : 1.042983

# LM without movieId, userId & title
t2 <- train %>% select(-c("userId", "movieId", "title"))
control <- trainControl(method = "none")
fit_lm <- train(rating~., data=t2, method="lm", trControl=control)
print(fit_lm$finalModel)

# 
# Call:
#   lm(formula = .outcome ~ ., data = dat)

#Coefficients:
#(Intercept)    user_score   movie_score     timestamp  release_year
# 4.7574378     1.0003619     0.9757425    -0.0001211   -0.0005009


y_hat <- predict(fit_lm, test)
result3 <- RMSE(test$rating, y_hat)
cat("RMSE :", result3)

## RMSE : 1.042491

# LM without movieId, userId & title
t2 <- train %>% select(-c("userId", "movieId", "title"))
control <- trainControl(method = "none")
fit_lm <- train(rating~., data=t2, method="lm", trControl=control)
print(fit_lm$finalModel)

#Call:
#  lm(formula = .outcome ~ ., data = dat)

#Coefficients:
#   (Intercept)                   timestamp  
#     5.1836910                  -0.0002859  
#     year_rated                  release_year  
#          NA                    -0.0005509  
#      Action                     Adventure  
#      -0.0151763                  -0.0023558  
#  `\\`Sci-Fi\\``                   Documentary  
#      -0.0009051                   0.0479658  
#       Crime                       Drama  
#       0.0141191                   0.0158736  
#       Thriller                    Comedy  
#       0.0005888                   0.0010364  
#       Mystery                     Animation  
#       0.0048191                   0.0219648  
#       Children                    Fantasy  
#      -0.0339020                   0.0136872  
#       Romance                      Horror  
#      -0.0126802                   0.0049235  
#       War                         Musical  
#      -0.0043971                   0.0082115  
#         Western           `\\`Film-Noir\\``  
#      -0.0054717                   0.0134549  
#            IMAX  `\\`(no genres listed)\\``  
#      -0.0417882                  -0.0287400  
#      user_score                 movie_score  
#      1.0008362                   0.9683767  

y_hat <- predict(fit_lm, test)
result3 <- RMSE(test$rating, y_hat)
cat("RMSE :", result3)

# RMSE : 1.042491
#RMSE = 1.042491. It is slightly better than the baseline score, but 
#not good enough.


#Decision Tree
fit_tree <- train(rating~user_score+movie_score+timestamp+release_year, data=train, method="rpart")
print(fit_tree$results)
plot(fit_tree$finalModel)
text(fit_tree$finalModel)

# Warning message:
#   In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
#                             There were missing values in resampled performance measures.
#  > print(fit_tree$results)
#          cp      RMSE  Rsquared       MAE     RMSESD  RsquaredSD
# 1 0.07660190 0.8592939 0.3396628 0.6671140 0.02221348 0.033506481
# 2 0.08038463 0.8912972 0.2895112 0.6783504 0.02648971 0.040752855
# 3 0.24239825 0.9599536 0.2404171 0.7353353 0.06253436 0.003464677
       # MAESD
# 1 0.01108212
# 2 0.01016251
# 3 0.07529535

# Noticing that prediction using Decision Tree is way too simple, with 
# only 3 predicted ratings and few conditions.

y_hat <- predict(fit_tree, test)
result4 <- RMSE(test$rating, y_hat)
cat("RMSE :", result4)

# RMSE : 1.062504

#The outcome is even worse than the baseline model. So let
#try the linear model with regularization 


#Linear Model with regularization
# with only user_score and movie_score

#splitting the train set into 2 to calculate the best of the two
indx <- createDataPartition(train$rating, times=1, p=0.8, list=FALSE)
tpart_1 <- train[indx, ]
tpart_2 <- train[-indx, ]

#calculating the best ones
best_ones <- seq(0, 10, 0.25)
rmses <- sapply(best_ones,  function(l){
  avg_rating <- mean(tpart_1$rating)
  movie_score <- tpart_1 %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - avg_rating)/(n()+l))
  user_score <- tpart_1 %>% 
    left_join(movie_score, by="movieId") %>%
    mutate(b_m = ifelse(is.na(b_m), 0, b_m)) %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - avg_rating)/(n()+l))
  predicted_ratings <- 
    tpart_2 %>% 
    left_join(movie_score, by = "movieId") %>%
    left_join(user_score, by = "userId") %>%
    mutate(b_m = ifelse(is.na(b_m), 0, b_m)) %>%
    mutate(b_u = ifelse(is.na(b_u), 0, b_u)) %>%
    mutate(pred = avg_rating + b_m + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, tpart_2$rating))
})

b1 <- best_ones[which.min(rmses)]
qplot(best_ones, rmses)


print(b1)

# b1 = 3.75

# The b1 which minimizes the RMSE is 3.75, so let use it to
#train the model and predict the test set


#Prediction 
b1 <- 3.75
avg_rating <- mean(train$rating)
movie_score <- train %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - avg_rating)/(n()+b1))
user_score <- train %>% 
  left_join(movie_score, by="movieId") %>%
  mutate(b_m = ifelse(is.na(b_m), 0, b_m)) %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - avg_rating)/(n()+b1))
predicted_ratings <- 
  test %>% 
  left_join(movie_score, by = "movieId") %>%
  left_join(user_score, by = "userId") %>%
  mutate(b_m = ifelse(is.na(b_m), 0, b_m)) %>%
  mutate(b_u = ifelse(is.na(b_u), 0, b_u)) %>%
  mutate(pred = avg_rating + b_m + b_u) %>%
  .$pred

result5 <- RMSE(test$rating, predicted_ratings)
cat("RMSE :", result5)

# RMSE : 0.9383446

# The result of the RMSE < 1, which is much better than 
# the other models. Now let use the genres columns to see how
# the predictions will be. So what is the effect of 
# genre on the ratings?

not_genres <- c("userId", "movieId", "rating", "timestamp", "title", "release_year", "user_score", "movie_score")
genres <- colnames(train)[!colnames(train) %in% not_genres]
genres

# "year_rated"         "Action"             "Adventure"         
# "Sci-Fi"             "Documentary"        "Crime"             
# "Drama"              "Thriller"           "Comedy"            
# "Mystery"            "Animation"          "Children"          
# "Fantasy"            "Romance"            "Horror"            
# "War"                "Musical"            "Western"           
# "Film-Noir"          "IMAX"               "(no genres listed)"


#What is the average ratings for each genre? Let see.
genre_scores <- data.frame(genre="",m=0, sd=0)
for(genre in genres){
  results <- train %>% filter(train[colnames(train)==genre]==1) %>%
    summarise(m=mean(rating), sd=sd(rating))
  genre_scores <- genre_scores %>% add_row(genre=genre, m=results$m, sd=results$sd)
}
genre_scores <- genre_scores[-1,]
genre_scores[is.na(genre_scores)] <- 0
genre_scores

# 
# #           genre        m        sd
# 2          year_rated 0.000000 0.0000000
# 3              Action 3.433409 1.0619524
# 4           Adventure 3.496405 1.0548618
# 5              Sci-Fi 3.414611 1.0812908
# 6         Documentary 3.808537 0.9926102
# 7               Crime 3.658984 1.0023646
# 8               Drama 3.675710 0.9958227
# 9            Thriller 3.513454 1.0260652
# 10             Comedy 3.445359 1.0705303
# 11            Mystery 3.680841 1.0078153
# 12          Animation 3.613767 0.9945636
# 13           Children 3.423029 1.0861241
# 14            Fantasy 3.510395 1.0586236
# 15            Romance 3.549792 1.0307855
# 16             Horror 3.277596 1.1477909
# 17                War 3.781634 1.0147717
# 18            Musical 3.570237 1.0409776
# 19            Western 3.546312 1.0360351
# 20          Film-Noir 4.029081 0.8635001
# 21               IMAX 3.539683 1.1510920
# 22 (no genres listed) 2.500000 0.0000000

#Plotting genres
genre_scores %>% ggplot(aes(x=m, y=genre)) +
  geom_point() +
  xlab("Average Ratings") +
  geom_errorbarh(aes(xmin=m-sd, xmax=m+sd))

# In the plot we notice that different genres have different
# ratings average. So it is ideal to use the average of the genres
# of a movie to predict the ratings if the movie and the user 
# in the test are not seen in the training set. 

#The actual issue with the regularized model is if there 
# is a case in the test data that has new movie and new user, 
# the model can only predict with the average of all ratings.
# With the added feature of genres,  it will probably change the landscape for better
# Minimize the RMSE.

#Regularized model with genre feature
b1 <- 3.75
avg_rating <- mean(train$rating)
movie_score <- train %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - avg_rating)/(n()+b1))
user_score <- train %>% 
  left_join(movie_score, by="movieId") %>%
  mutate(b_m = ifelse(is.na(b_m), 0, b_m)) %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - avg_rating)/(n()+b1))
genre_score <- as.matrix(test[, genres]) %*% genre_scores$m
n_genres <- rowSums(test[,genres])
genre_score <- genre_score / n_genres

#What is the effect of using the genre_scores if the user and 
#movie are unknown?
predicted_ratings <- 
  test %>% 
  left_join(movie_score, by = "movieId") %>%
  left_join(user_score, by = "userId") %>%
  cbind(genre_score) %>%
  mutate(pred = genre_score) %>%
  mutate(pred = ifelse(!is.na(b_m)|!is.na(b_u), 
                       avg_rating + replace_na(b_m,0) + replace_na(b_u,0), 
                       pred))

result6 <- RMSE(test$rating, predicted_ratings$pred)
cat("RMSE :", result6)

# RMSE : 0.9491269

# We notice that the improvement is very slim in RMSE 
# when using genres for prediction purposes


#Put them together (table of RMSE Results)
data.frame(
  method=c("Naive Prediction", "Linear Model (with 4 features)", "Linear Model (with all features)", "Decision Tree", "Linear Model with Regularisation(only using movie and user scores)", "Linear Model with Regularisation(movie, user, and genre scores)"), 
  rmse=c(result, result2, result3, result4, result5, result6))



# method                                                                  rmse
#1 Naive Prediction                                                      1.0512258
#2 Linear Model (with only 4 features)                                   1.0424912
#3 Linear Model (with all features)                                      1.0424912
#4 Decision Tree                                                         1.0625044
#5 Linear Model with Regularisation(only using movie and user scores)    0.9383446
#6 Linear Model with Regularisation(movie, user, and genre scores)       0.9491269


# Now let apply the final model to validation set
# After dissecting to come up with different models, we can use 
# the best performing model in the previous section, which is the regularized model. 
# The validation data is set in a way so the users and movies in the data are all present in the 
# edx data.
# So, it is not  recommended using the following: 
#genre fech has both unknown user and unknown movie, not the best choice


#Training the final model

#Using the model on the final_holdout_test data
b1 <- 3.75
avg_rating <- mean(edx$rating)
movie_score <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - avg_rating)/(n()+b1))
user_score <- edx %>% 
  left_join(movie_score, by="movieId") %>%
  mutate(b_m = ifelse(is.na(b_m), 0, b_m)) %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - avg_rating)/(n()+b1))
predicted_ratings <- 
  final_holdout_test %>% 
  left_join(movie_score, by = "movieId") %>%
  left_join(user_score, by = "userId") %>%
  mutate(b_m = ifelse(is.na(b_m), 0, b_m)) %>%
  mutate(b_u = ifelse(is.na(b_u), 0, b_u)) %>%
  mutate(pred = avg_rating + b_m + b_u) %>%
  .$pred


final_result <- RMSE(final_holdout_test$rating, predicted_ratings)
cat("RMSE :", final_result)


# > cat("RMSE :", final_result)
# RMSE : 0.8648477

# The final RMSE is 0.8648477 which is the required RMSE (0.8649) 
# to get the maximum point for the EDX Capstone Movielens projects.


# 
# Conclusion
# 
# #This MovieLens dataset model provided by edx is a beautiful project to work on. Indeed outcomes can be
# quite different, close to perfect meaning RMSE could almost be second to none if average raters(users) don’t
# play bias, meaning user doesn’t rate a particularly good/popular movie with a large margin bi, and vice
# versa. Probabilistically speaking if my statement about RMSE is not impossible, then it has to be probable,
# even if the chance is infinitesimally small.
# #Polymorphically speaking, I used quite a few machine learning algorithms to come up with predictions
# movie ratings for this MovieLens dataset. As expected results are different from one another for RMSE.
# Among those steps (algorithms) used. The regularized model would be ideal with the users side effect to
# lower RMSE.
# #Is there a better way or much room for improvement in a nutshell? Off course yes, because life is poly-
# morphic but I haven’t come across it yet.
# #Hence this model can be improved by adding other accoutrement(age, year, genre,. . . ) and on how users
# should/could rate movies. Also we can apply different machine learning models to improve, hence a better
# polished outcome.