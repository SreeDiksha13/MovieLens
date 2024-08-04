library(tidyverse)
library(caret)
library(data.table)
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
library(lubridate)

library(DT)
library(wordcloud) 
library(RColorBrewer) 
library(ggthemes) 
library(recommenderlab)
library(irlba)
library(recosystem)




movies_path <- "C:/Users/sreed/OneDrive/Documents/Edx1/ml-10m/ml-10M100K/movies.dat"
ratings_path <- "C:/Users/sreed/OneDrive/Documents/Edx1/ml-10m/ml-10M100K/ratings.dat"


ratings <- fread(text = gsub("::", "\t", readLines(ratings_path)),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
head(ratings)

movies <- str_split_fixed(readLines(movies_path), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))
head(movies)
movielens <- left_join(ratings, movies, by = "movieId")
head(movielens)

# Our validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,] #temp is the validation set


#To ensure that the userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
head(validation)

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm( ratings, movies, test_index, temp, movielens, removed)


#a look at the datasets
glimpse(edx)
glimpse(validation)

#2. EDA

group <-  ifelse((edx$rating == 1 |edx$rating == 2 | edx$rating == 3 | 
                    edx$rating == 4 | edx$rating == 5) ,
                 "whole_star", 
                 "half_star")

explore_ratings<- data.frame(edx$rating, group)
head(explore_ratings)

#HISTOGRAM
ggplot(explore_ratings, aes(x= edx.rating, fill = group)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  scale_fill_manual(values = c("half_star"="purple", "whole_star"="brown")) +
  labs(x="rating", y="number of ratings", caption = "source data: edx set") +
  ggtitle("histogram : number of ratings for each rating")

#QUALITATIVE FEATURES

top_genre <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

datatable(top_genre, rownames = FALSE, filter="top", options = list(pageLength = 5, scrollX=T) ) %>%
  formatRound('count',digits=0, interval = 3, mark = ",")


layout(matrix(c(1,2), nrow =2) , heights = c(1,4))
par(mar=rep(0,4))
plot.new()
text(x=0.5,y=0.5, "top Genres by number of ratings")
wordcloud(words=top_genre$genres,freq=top_genre$count,min.freq=50,
          max.words = 20,random.order=FALSE,random.color=FALSE,
          rot.per=0.35,colors = brewer.pal(8,"Dark2"),scale=c(5,.2),
          family="plain",font=2,
          main = "Top genres by number of ratings")

##top title
top_title <- edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(20,count) %>%
  arrange(desc(count))

kable(head(edx %>%
             group_by(title,genres) %>%
             summarize(count=n()) %>%
             top_n(20,count) %>%
             arrange(desc(count)) ,
           5)) %>%
  kable_styling(bootstrap_options = "bordered", full_width = F , position ="center") %>%
  column_spec(1,bold = T ) %>%
  column_spec(2,bold =T) %>%
  column_spec(3,bold=T)


top_title %>% 
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat='identity', fill="lightpink") + coord_flip(y=c(0, 40000)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top 20 movies title based \n on number of ratings" , caption = "source data: edx set")

#Erros in genres
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "error bar plots by genres" , caption = "source data : edx set") +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

#userId, movieId

edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# histogram of number of ratings by movieId

edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = "lightpink") +
  scale_x_log10() + 
  ggtitle("Movies") +
  labs(subtitle  ="Number of Ratings by movieId", 
       x="movieId" , 
       y="Number of Ratings", 
       caption ="Source data : Edx set") +
  theme(panel.border = element_rect(colour="blue", fill=NA))
#Histogram of number of ratings by userId
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = "maroon") +
  scale_x_log10() + 
  ggtitle("Users") +
  labs(subtitle ="Number of Ratings by UserId", 
       x="userId" , 
       y="Number of Ratings") +
  theme(panel.border = element_rect(colour="black", fill=NA))

#The edx set contains the timestamp variable, representing the date and time when each rating was provided, measured in seconds since January 1, 1970. Using the as_datetime function from the lubridate package, I can convert each timestamp to the correct format. Then, I create a scatterplot with y as the average ratings and x as the date. Additionally, I use a smooth geom to help visualize patterns amidst any overplotting.
edx %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "Week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Timestamp, time unit : week")+
  labs(subtitle = "Average ratings",
       caption = "Source data : Edx set")

#Matrix

# As we said in the Data exploration step,  usersId and movieId should be treat as factors for some analysis purposes. To perform this transformation we make a copy of edx set, since we want to keep unchanged our original training set.(edx)

edx.copy <- edx

edx.copy$userId <- as.factor(edx.copy$userId)
edx.copy$movieId <- as.factor(edx.copy$movieId)

# Convertings userId and MovieId into numeric vectors

edx.copy$userId <- as.numeric(edx.copy$userId)
edx.copy$movieId <- as.numeric(edx.copy$movieId)

sparse_ratings <- sparseMatrix(i = edx.copy$userId,
                               j = edx.copy$movieId ,
                               x = edx.copy$rating, 
                               dims = c(length(unique(edx.copy$userId)),
                                        length(unique(edx.copy$movieId))),  
                               dimnames = list(paste("u", 1:length(unique(edx.copy$userId)), sep = ""), 
                                               paste("m", 1:length(unique(edx.copy$movieId)), sep = "")))
# i can remove the copy created
rm(edx.copy)

#give a look on the first 10 users
sparse_ratings[1:10,1:10]


#Convert rating matrix into a recommenderlab sparse matrix
ratingMat <- new("realRatingMatrix", data = sparse_ratings)
ratingMat

#i calculate the user similarity using the cosine similarity

similarity_users <- similarity(ratingMat[1:50,], 
                               method = "cosine", 
                               which = "users")

image(as.matrix(similarity_users), main = "User similarity")


set.seed(1)
Y <- irlba(sparse_ratings,tol=1e-4,verbose=TRUE,nv = 100, maxit = 1000)

# plot singular values

plot(Y$d, pch=20, col = "blue", cex = 1.5, xlab='Singular Value', ylab='Magnitude', 
     main = "Singular Values for User-Movie Matrix")


# calculate sum of squares of all singular values
all_sing_sq <- sum(Y$d^2)

# variability described by first 6, 12, and 20 singular values
first_six <- sum(Y$d[1:6]^2)
print(first_six/all_sing_sq)

first_12 <- sum(Y$d[1:12]^2)
print(first_12/all_sing_sq)

first_20 <- sum(Y$d[1:20]^2)
print(first_20/all_sing_sq)

perc_vec <- NULL
for (i in 1:length(Y$d)) {
  perc_vec[i] <- sum(Y$d[1:i]^2) / all_sing_sq
}

plot(perc_vec, pch=20, col = "blue", cex = 1.5, xlab='Singular Value', ylab='% of Sum of Squares of Singular Values', main = "Choosing k for Dimensionality Reduction")
lines(x = c(0,100), y = c(.90, .90))


#To find the exact value of k, i calculate  the length of the vector that remains from our running sum of squares after excluding any items within that vector that exceed 0.90.

k = length(perc_vec[perc_vec <= .90])
k

#I get the decomposition of Y ; matrices U, D, and V accordingly:

U_k <- Y$u[, 1:k]
dim(U_k)

D_k <- Diagonal(x = Y$d[1:k])
dim(D_k)

V_k <- t(Y$v)[1:k, ]
dim(V_k)


#a.
min_n_movies <- quantile(rowCounts(ratingMat), 0.9)
print(min_n_movies)
#b.
min_n_users <- quantile(colCounts(ratingMat), 0.9)
print(min_n_users)
#c.
ratings_movies <- ratingMat[rowCounts(ratingMat) > min_n_movies,
                            colCounts(ratingMat) > min_n_users]
ratings_movies

lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Final Results

#a.movie effect

# calculate the average of all ratings of the edx set
mu <- mean(edx$rating)

# calculate b_i on the training set
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


# predicted ratings
predicted_ratings_bi <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i


#b.movie + user effect

#calculate b_u using the training set 
user_avgs <- edx %>%  
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#predicted ratings
predicted_ratings_bu <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred


#c.movie + user + time effect

#create a copy of validation set , valid, and create the date feature which is the timestamp converted to a datetime object  and  rounded by week.

valid <- validation
valid <- valid %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) 

# i calculate time effects ( b_t) using the training set
temp_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u))

# predicted ratings
predicted_ratings_bt <- valid %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(temp_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  .$pred

#d.  i calculate the RMSE for movies, users and time effects 

rmse_model1 <- RMSE(validation$rating,predicted_ratings_bi)  
rmse_model1
rmse_model2 <- RMSE(validation$rating,predicted_ratings_bu)
rmse_model2
rmse_model3 <- RMSE(valid$rating,predicted_ratings_bt)
rmse_model3


#before to proceed with regularization, i just remove the object copy of validation, "valid"
rm(valid)

#e. regularization 

# remembering (5), $\lambda$ is a tuning parameter. We can use cross-validation to choose it

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu_reg <- mean(edx$rating)
  
  b_i_reg <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating - mu_reg)/(n()+l))
  
  b_u_reg <- edx %>% 
    left_join(b_i_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - b_i_reg - mu_reg)/(n()+l))
  
  predicted_ratings_b_i_u <- 
    validation %>% 
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(pred = mu_reg + b_i_reg + b_u_reg) %>%
    .$pred
  
  return(RMSE(validation$rating,predicted_ratings_b_i_u))
})

library(ggplot2)
qplot(lambdas, rmses)


#For the full model, the optimal  λ is:

lambda <- lambdas[which.min(rmses)]
lambda

rmse_model4 <- min(rmses)
rmse_model4


#summarize all the rmse on validation set for Linear regression models

rmse_results <- data.frame(Methods=c("Movie effect","Movie + user effects","Movie + user + time effects", "Regularized Movie + User Effect Model"),rmse = c(rmse_model1, rmse_model2,rmse_model3, rmse_model4))

library(kableExtra)

kable(rmse_results) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center") %>%
  kable_styling(bootstrap_options = "bordered", full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, bold = TRUE, color = "black", background = "pink")

## Recommender Systems
# a. POPULAR , UBCF and IBCF algorithms of the recommenderlab package

model_pop <- Recommender(ratings_movies, method = "POPULAR", 
                         param=list(normalize = "center"))

#prediction example on the first 10 users
pred_pop <- predict(model_pop, ratings_movies[1:10], type="ratings")
as(pred_pop, "matrix")[,1:10]

#Calculation of rmse for popular method 
set.seed(1)
e <- evaluationScheme(ratings_movies, method="split", train=0.7, given=-5)
#5 ratings of 30% of users are excluded for testing

model_pop <- Recommender(getData(e, "train"), "POPULAR")

prediction_pop <- predict(model_pop, getData(e, "known"), type="ratings")

rmse_popular <- calcPredictionAccuracy(prediction_pop, getData(e, "unknown"))[1]
rmse_popular

#Estimating rmse for UBCF using Cosine similarity and selected n as 50 based on cross-validation
set.seed(1)
model <- Recommender(getData(e, "train"), method = "UBCF", 
                     param=list(normalize = "center", method="Cosine", nn=50))

prediction <- predict(model, getData(e, "known"), type="ratings")

rmse_ubcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_ubcf

#Estimating rmse for IBCF using Cosine similarity and selected n as 350 based on cross-validation
set.seed(1)

model <- Recommender(getData(e, "train"), method = "IBCF", 
                     param=list(normalize = "center", method="Cosine", k=350))

prediction <- predict(model, getData(e, "known"), type="ratings")

rmse_ibcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_ibcf



#b. SlopeOne  

#createed a copy of training(edx) and validation sets where i retain only userId, movieId and rating
edx.copy <- edx %>%
  select(-c("genres","title","timestamp"))
head(edx.copy)

valid.copy <- validation %>%
  select(-c("genres","title","timestamp"))

# rename columns and convert them to characters  for edx.copy and valid.copy sets : item_id  is #seen as movie_id

names(edx.copy) <- c("user_id", "item_id", "rating")
library(data.table)

edx.copy <- data.table(edx.copy)

edx.copy[, user_id := as.character(user_id)]
edx.copy[, item_id := as.character(item_id)]


names(valid.copy) <- c("user_id", "item_id", "rating")

valid.copy <- data.table(valid.copy)

valid.copy[, user_id := as.character(user_id)]
valid.copy[, item_id := as.character(item_id)]


#setkey() sorts a data.table and marks it as sorted (with an attribute sorted). The sorted columns are the key. The key can be any columns in any order. The columns are sorted in ascending order always. The table is changed by reference and is therefore very memory efficient.
setkey(edx.copy, user_id, item_id)
setkey(valid.copy, user_id, item_id)


#split data to create a small training sample ( to face the RAM memory issue)
set.seed(1)
idx <- createDataPartition(y = edx.copy$rating, times = 1, p = 0.1, list = FALSE)
head(idx)
edx.copy_train <- edx.copy[idx,]
head(edx.copy_train)
normalize_ratings <- function(ratings_data) {
  # Calculate the mean rating for each user
  user_means <- ratings_data[, .(mean_rating = mean(rating, na.rm = TRUE)), by = user_id]
  
  # Merge with the original data to compute normalized ratings
  normalized_data <- merge(ratings_data, user_means, by = "user_id")
  
  # Normalize ratings: subtract the user's mean rating from each rating
  normalized_data[, normalized_rating := rating - mean_rating]
  
  return(normalized_data)
}

# Apply the function to your data
ratings_train_norm <- normalize_ratings(edx.copy_train)




#c. Matrix Factorization with parallel stochastic gradient descent

#i create a copy of training(edx) and validation sets where i retain only userId, movieId and rating features. i rename the three columns.

edx.copy <-  edx %>%
  select(-c("genres","title","timestamp"))

names(edx.copy) <- c("user", "item", "rating")


valid.copy <-  validation %>%
  select(-c("genres","title","timestamp"))

names(valid.copy) <- c("user", "item", "rating")


#as matrix
edx.copy <- as.matrix(edx.copy)
valid.copy <- as.matrix(valid.copy)

library(recosystem)
#write edx.copy and valid.copy tables on disk 
write.table(edx.copy , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(valid.copy, file = "validset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)


#  data_file(): Specifies a data set from a file in the hard disk. 

set.seed(123) # This is a randomized algorithm
train_set <- data_file("C:/Users/sreed/OneDrive/Documents/Edx1/trainset.txt")
valid_set <- data_file("C:/Users/sreed/OneDrive/Documents/Edx1/validset.txt")

#Next step is to build Recommender object
r = Reco()


# Matrix Factorization :  tuning training set

opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))
opts


# Matrix Factorization : trains the recommender model

r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

#Making prediction on validation set and calculating RMSE:

pred_file = tempfile()

r$predict(valid_set, out_file(pred_file))  

#Matrix Factorization : show first 10 predicted values
print(scan(pred_file, n = 10))

#valid_set
scores_real <- read.table("validset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(pred_file)

#remove copies of training and validation sets
rm(edx.copy, valid.copy)

rmse_mf <- RMSE(scores_real,scores_pred)
rmse_mf  


#summarize all the rmse for recommender algorithms

rmse_results <- data.frame(methods=c("SlopeOne","Matrix factorization with GD"),rmse = c(rmse_slopeone, rmse_mf))

kable(rmse_results) %>%
  kable_styling(bootstrap_options = "striped" , full_width = F , position = "center") %>%
  kable_styling(bootstrap_options = "bordered", full_width = F , position ="center") %>%
  column_spec(1,bold = T ) %>%
  column_spec(2,bold = T ,color = "white" , background ="#D7261E")


```{r,echo=TRUE,warning=FALSE,message=FALSE, comment=NA,null_prefix=TRUE}


#i create a copy of training(edx) and validation sets where i retain only userId, movieId and rating features. i rename the three columns.

edx.copy <-  edx %>%
  select(-c("genres","title","timestamp"))

names(edx.copy) <- c("user", "item", "rating")


valid.copy <-  validation %>%
  select(-c("genres","title","timestamp"))

names(valid.copy) <- c("user", "item", "rating")


#as matrix
edx.copy <- as.matrix(edx.copy)
valid.copy <- as.matrix(valid.copy)


#write edx.copy and valid.copy tables on disk 
write.table(edx.copy , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(valid.copy, file = "validset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)


#data_file(): Specifies a data set from a file in the hard disk. 

set.seed(123) # This is a randomized algorithm
train_set <- data_file("C:/Users/sreed/OneDrive/Documents/Edx1/trainset.txt")
valid_set <- data_file("C:/Users/sreed/OneDrive/Documents/Edx1/validset.txt")


#Next step is to build Recommender object
r = Reco()


#Optimizing/tuning the recommender model
opts <- r$tune(train_set , opts = list(dim = c(1:20), lrate = c(0.05),
                                       nthread = 4 , costp_l1=0, 
                                       costq_l1 = 0,
                                       niter = 50, nfold = 10,
                                       verbose = FALSE))

#trains the recommender model
r$train(train_set, opts = c(opts$min , nthread = 4, niter = 100,verbose=FALSE))


#Making prediction on validation set and calculating RMSE:
pred_file = tempfile()

r$predict(valid_set, out_file(pred_file))  

#Matrix Factorization : show first 10 predicted values
print(scan(pred_file, n = 10))


#valid_set
scores_real <- read.table("validset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(pred_file)

# remove edx.copy and valid.copy objects
rm(edx.copy, valid.copy)


rmse_mf_opt <- RMSE(scores_real,scores_pred)

rmse_mf_opt 
