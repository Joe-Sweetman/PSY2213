###################Difference between significant effects vs. model fit#####################

#We fit a model with no significant "effects"/parameters but good fit

# we set a seed to make sure this example is exactly reproducible
set.seed(123)                 

# we generate some data for the varibale x1 and x2 from a uniform distribution
x1 = runif(100, min=-5, max=5)  
x2 = runif(100, min=-5, max=5)  #  between -5 and 5

# we generate some errors from a normal distribution
e  = rnorm(100, mean=0, sd=1)   

# since we are making up the example can set the ground truth for the true intercept and slopes as 0
y  = 0 + 0*x1 + 0*x2 + e    

# make a dataframe
df <- data.frame(x1, x2, y, e)

# fit the linear model
m1 <- lm(y~x1+x2)

# look at the model summary
summary(m1)

# we can plot the the ground truth (black), model fit (red), and the predicted and observed conditional means using LOWESS (black)
ggplot(df, aes(x = x1, y = y)) +
  geom_point() +
  stat_smooth(aes(col = "Model fit"),method = lm,fill = "red")+
  stat_smooth(aes(col = "LOWESS"),fill = "black")+
  geom_abline(aes(slope = 0, intercept = 0, col = "True function"),size =1)+
  scale_color_manual(name = "Line",values = c("black", "red", "blue"))

#We fit a model with significant "effects"/parameters but poor fit

# since we are making up the example we can set the ground truth for the true intercept and slopes, this time they are not 0 they show curvilinear 
y2 = 4.96 + 0.65*x1 + -0.16*x1^2 + 0.62*x2 + -0.19*x2^2 + e  

# make a dataframe
df2 <- data.frame(x1, x2, y2, e)

# fit the linear model
m2 <- lm(y2~x1+x2)

# look at the model summary
summary(m2)

# we can plot the the ground truth (black), model fit (red), and the predicted and observed conditional means using LOWESS (black)
ggplot(df2, aes(x = x1, y = y2)) +
  geom_point() +
  stat_smooth(aes(col = "Model fit"),method = lm,fill = "red")+
  stat_smooth(aes(col = "LOWESS"),fill = "black")+
  stat_smooth(aes(col = "True function"),method = "lm", formula=y ~ poly(x,2))+
  scale_color_manual(name = "Line",values = c("black", "red", "blue"))

# we can compare the AIC and BIC for model fit
compare_performance(m1,m2)

