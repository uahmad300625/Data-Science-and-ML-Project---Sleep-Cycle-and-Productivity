
library(dplyr)
library(randomForest)

#Loading and Cleaning data

cleaned_data <- sleep_cycle_productivity %>%
  na.omit() %>%
  select(-Person_ID, -Date, -Mood.Score, -Stress.Level) %>%
  mutate(
    # Convert gender to factor 
    Gender = as.factor(Gender),
    
    # Create a binary variable to do Logistic Regression.
    Productivity_Binary_Score = factor(
      ifelse(Productivity.Score >= 6, "High", "Low"), levels = c("Low", "High")
    )
    
  )

table(cleaned_data$Productivity_Binary_Score)


# Run logistic regression
initial_lm <- glm(Productivity_Binary_Score ~ Age + Gender + Total.Sleep.Hours + Exercise..mins.day. 
                  + Caffeine.Intake..mg. + Screen.Time.Before.Bed..mins. + Work.Hours..hrs.day.+ Sleep.Quality, 
                 family = binomial, 
                 data = cleaned_data)
summary(initial_lm)


#running step function to further confirm significant variables

full_model <- glm(Productivity_Binary_Score ~ Age + Gender + Total.Sleep.Hours + Exercise..mins.day. 
                  + Caffeine.Intake..mg. + Screen.Time.Before.Bed..mins. + Work.Hours..hrs.day.+ Sleep.Quality, 
                  family = binomial, 
                  data = cleaned_data)

step_model <- step(full_model, direction = "backward")

summary(step_model)




#refined model

refined_model <- glm(Productivity_Binary_Score ~ Total.Sleep.Hours, 
                     family = binomial, 
                     data = cleaned_data)
summary(refined_model)


#Finding MSE for the Refined Logistic Model

mse = rep(0,10)
for (i in 1:10){
  set.seed(i)
  
  train_index = sample(1:nrow(cleaned_data), size = 0.8 *nrow(cleaned_data))
  train_data = cleaned_data[train_index,]
  test_data = cleaned_data[-train_index,]
  
  refined_model <- glm(Productivity_Binary_Score ~ Total.Sleep.Hours, 
                       family = binomial, 
                       data = train_data)
  
  predictions = predict(refined_model, newdata = test_data, type = "response")
  actuals = ifelse(test_data$Productivity_Binary_Score == "High", 1, 0)
  
  mse[i] = mean((predictions - actuals)^2)
  print(mse[i])
}

mean_mse = mean(mse)
print(mean_mse)



#Random Forests


#Initial model
set.seed(42)

initial_rf = randomForest(Productivity.Score ~ Age + Gender + Total.Sleep.Hours + Exercise..mins.day. 
                          + Caffeine.Intake..mg. + Screen.Time.Before.Bed..mins. + Work.Hours..hrs.day.+ Sleep.Quality, 
                          mtry = sqrt(8),
                          importance = TRUE,
                          data = cleaned_data)

initial_rf

varImpPlot(initial_rf, sort = TRUE, main = NA)


#Refined Model

refined_rf = randomForest(Productivity.Score ~  Total.Sleep.Hours + Exercise..mins.day. 
                          + Caffeine.Intake..mg., 
                          mtry = sqrt(8),
                          importance = TRUE,
                          data = cleaned_data)

refined_rf

varImpPlot(refined_rf, sort = TRUE, main = NA)



#Linear Regression Model - not a good model, only one significant variable

#initial_lm <- lm(cleaned_data$Productivity_Binary_Score ~., data = cleaned_data)
#summary(initial_lm)
