data = read.csv("C:/Users/Alexei/SupervisedMl.csv")
lm = glm(Scores~Hours, data=data)
summary(lm)

#Residuals
data$prediction = predict(lm,data)
#Extracting Residuals
data$residuals = data$Hours - data$prediction
summary(data$residuals)

#initalize the object that stores the various coefficient values
samp_coeff = c()
#Repeat the iteration for 100 times 
for(i in 1:100){
  #Sample 50% of the total data
  samp = sample(nrow(data),0.5*nrow(data))
  data2 = data[samp,]
  #Fit a model on the sampled data
  lm = lm(Scores~Hours, data = data2)
  #Extract the coefficient of independent variable and store it
  samp_coeff = c(samp_coeff, lm$coefficients['Hours'])
}
sd(samp_coeff)

#The sum of squared error is always calculated as follows
data$residual = data$Hours - data$prediction
sum((data$prediction-data$Hours)^2)

#Null deviance
data$prediction = mean(data$Hours)
sum((data$prediction-data$Hours)^2)

#create a correlated variable
data$correlated = data$Hours*0.5+rnorm(nrow(data))*0.1
cor(data$Hours, data$correlated)