#############################################################
# Getting Data
#############################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
 if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(rpart)
#We Read in the heart.csv file and set it to a data frame called data
data <- read.csv('heart.csv')
head(data)


head(data)
str(data)
summary(data)



##############################################
#Data Analysis
#############################################

 
## output distribution

data %>% group_by(output) %>% summarize(count = n())%>%
ggplot(aes(output , count)) +
  geom_col(  color = "black", fill = "#2e4057") +
  xlab("output") +
   ylab("Count") + ggtitle("output distribution")


 
 

# Split raw data set into train and test set: Validation set will be 10% of the Set
# Validation set will be 10% of  data
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = data$output, times = 1, p = 0.35, list = FALSE) 
train_set <- data[-test_index,] 
test_set <- data[test_index,]


#############################################################
#Model Building
#############################################################



#Logistic regression

train_set %>%
  filter(age < mean(age) ) %>%
  summarize(y_hat = mean(output == 1)) 


train_set %>%
   group_by(age) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(output == 1)) %>%
  ggplot(aes(age, prop)) +
  geom_point()


lm_fit <- lm(output ~ age, data = train_set)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, 1, 0)%>% factor()
confusionMatrix(y_hat, test_set$output%>%factor()) 

#Accuracy : 0.5806 
#We see this method does substantially better than guessing.


train_set %>%
   group_by(age) %>%
  filter(n() >= 5) %>%
  summarize(prop = mean(output == 1)) %>%
  ggplot(aes(age, prop)) +
  geom_point() +
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])


glm_fit <-  glm(output ~ age, data = train_set, family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor
confusionMatrix(y_hat_logit, test_set$output %>% factor)

 
#Logistic regression with more than one predictor
glm_fit_more_than_one <-  glm(output ~ age + trtbps + cp + chol +thalachh + oldpeak , data = train_set, family = "binomial")
p_hat_logit_more_than_one <- predict(glm_fit_more_than_one, newdata = test_set, type = "response")
y_hat_logit_more_than_one <- ifelse(p_hat_logit_more_than_one > 0.5, 1, 0) %>% factor
confusionMatrix(y_hat_logit_more_than_one, test_set$output %>% factor)



#k-nearest neighbors
train_knn <- train(output%>% as.factor() ~ ., method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(1, 15, 1)))
train_knn$bestTune

confusionMatrix(predict(train_knn, test_set, type = "raw"),
                test_set$output%>% as.factor())$overall["Accuracy"]

 
ggplot(train_knn)


#regression trees

fit <- rpart(output   ~ ., data = train_set)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

 

train_rpart <- train(output %>% as.factor() ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train_set)
plot(train_rpart)
train_rpart$bestTune
y_hat_rpart <- predict(train_rpart, test_set)
confusionMatrix(y_hat_rpart, test_set$output %>% as.factor())$overall["Accuracy"]


#Random forests
library(randomForest)
fit <- randomForest(output %>% as.factor() ~., data = train_set)
plot(fit)


confusionMatrix(predict(fit, test_set),
                test_set$output %>% as.factor())$overall["Accuracy"]
