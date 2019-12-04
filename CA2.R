
##############################################################################################################

#Consider a relational dataset and specify your input and output variables , then

# 1) (a)	Train the model using 80% of this dataset and suggest an appropriate 
#         GLM to model output to input variables. 

data <- mtcars  
head(data) 
nrow(data)

x1 <- data$cyl     
x2 <- data$hp 
x3 <- data$wt 
y  <- data$am 

dataset <- na.omit(data.frame(x1,x2,x3,y)) # remove missing values 

n = nrow(dataset) # Add all row to "n"
indexes = sample(n,n*(80/100)) # Generate random sample (80%)

trainset = dataset[indexes,] # 80% of the data
testset = dataset[-indexes,] # 20% of the data

dim(testset) # Displays Dimension of testset

trainset.glm <- glm(y ~., data = trainset, family='binomial')  # Using Logistic regressions as data is discrete
summary(trainset.glm) 

##############################################################################################################

# 1)  (b) Specify the significant variables on the output variable at the level of 𝛼=0.05
#        and explore the related hypotheses test. Estimate the parameters of your model. 

help(summary.glm) # Information on GLM
summary(trainset.glm)

# Based on the readings from "summary(trainset.glm)" x3 is statistically significant. p-value < 0.05

length(coef(trainset.glm)) # Number of parameters
coef(trainset.glm) # Coefficient

summary(trainset.glm)$coefficients[,4] # to get ap P values

anova(trainset.glm, test="LRT") # Hypothesis Test using Chi-Sq

##############################################################################################################

# 1) (c)	Predict the output of the test dataset using the trained model. 
#         Provide the functional form of the optimal predictive model

pred=predict(trainset.glm, trainset)
pred



