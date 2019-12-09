############################################################################################################################################################################################################################
# Prepare Data for Q1

rm(list = ls())                    # Remove environment variables


dataset <- read.csv("college.csv") # Read in data (file must be in current Directory)
head(dataset)                      # Display first five rows
str(dataset)                       # Dataset has 400 observations and 4 variable
nrow(dataset)                      # Number of rows / observations


gre <- dataset$gre                 # Assign column GRE to gre ( GRE - Graduate Record Examinations )
gpa <- dataset$gpa                 # Assign column GPA to GPA ( GPA - Grade Point Average )
rank <- as.factor(dataset$rank)    # Assign column Rank to rank ( Rank - Rank 1 - 4 )
admit  <- dataset$admit            # Assign column Admit to admit ( 1 = admitted, 0 = not admitted )

rank

dataset <- na.omit(data.frame(gre,gpa,rank,admit))     # remove all missing value from each column
sum(is.na(dataset))                                    # Check for NULL Values       

summary(dataset)                                       # Display min, 1st Q, median, mean, 3rd Qu, Max. Already we can see that
                                                       # the mean of admit is 0.3175 meaning there are more rejects then admits

############################################################################################################################################################################################################################

#Consider a relational dataset and specify your input and output variables , then

# 1) (a)	Train the model using 80% of this dataset and suggest an appropriate 
#         GLM to model output to input variables. 


n <- nrow(dataset)                     # Add all row to "n"
indexes <- sample(n,n*(80/100))        # Generate random sample (80%)

trainset <- dataset[indexes,]          # 80% of the data (Trainset)
testset <- dataset[-indexes,]          # 20% of the data (Testset)

dim(testset)                          # Displays Dimension of testset


model.fit <- glm(admit ~ gre+gpa+rank, data=trainset, family="binomial")            # Predict Admit - gre, gpa and rank. Using Logistic regressions as data is discrete

help(summary.glm)                                                                   # Information on GLM                                                                       
summary(model.fit)                                                                  # Display summary of model  

##############################################################################################################

# 1)  (b) Specify the significant variables on the output variable at the level of 𝛼=0.05
#         and explore the related hypotheses test. Estimate the parameters of your model. 

length(coef(model.fit))                       # Number of parameters
coef(model.fit)                               # Coefficients(Estimates) of model

summary(model.fit)$coefficients[,4]           # to get ap P values

summary(model.fit)$coeff[-1,4] < 0.05         # Displays which columns are statistically significant (< 0.05)


anova(model.fit, test="LRT")                  # Hypothesis Test using Chi-Sq

##############################################################################################################

# 1)  (c)	Predict the output of the test dataset using the trained model. 
#         Provide the functional form of the optimal predictive model


predtest <- predict(model.fit, testset)                          # Predict model using testset
predtest                                                         # Display predicted values


x <- data.frame(gre=600,gpa=3.6,rank=as.factor(1))                        # Create a dataframe x with values for gre, gpa and rank to be tested with trainset

pred <- predict(model.fit,x)                                     # predict dataframe with created model
pred                                                             # Displays predicted value, In this example the result is 0.32, Meaning there is a 32% chance that a student with these parmameters will be admitted. 

##############################################################################################################

# 1) (d) Provide the confusion matrix and obtain the probability of correctness of predictions. 

predictedvalues <- rep(0,length(predtest))                                          # Create a list predictedvalues and assign 0 to the length of the predicted model 

predictedvalues[predtest > 0.5] = 1                                                 # Probability of Admit being 1, if p > 0.5 then admit = 0

confusion_matrix <- table( predictedvalues, actualvalues=testset[,4])               # Create confusion matrix table for predicted values

confusion_matrix                                                                    # Display Confusion Matrix

accuracy <- mean(predictedvalues == testset[,4])                                    # Correctness of prediction 

accuracy                                                                            # Diplay correctness of prediction (Result 0.7)

##############################################################################################################


# Let x_1...,x_10 are identically independently distributed (iid) with Poisson(λ).

# 2) (a)	Compute the likelihood function (LF)


set.seed(100)
n <- 10
lambda <- 5
poisson <- rpois(20, lambda)


data2 <- rpois(n, lambda)                                       # Generate data from Poisson distribution with the parameter lambda = 5


likelihood <- function(x){ 
  Lk <- NULL
  for( l in 0:max(x)){
    
    p <- proPoi(x = x,lambda = l)
    Lk <- rbind(Lk,c(l,p))
  }
  Lk[,2] <- Lk[,2]/sum(Lk[,2])
  Lk <- data.frame(Lk)
  names(Lk)<-c("lambda","Likelihood")
  Lk
}

proPoi <- function(x,lambda){
  p <- 1
  for(i in 0:max(x)){
    
    p <- p*exp(-lambda)*lambda^i/factorial(i)
  }
  p
}

proPoi(x = data2,lambda=lambda)

likePoi <- likelihood(x=data2)

likePoi


##############################################################################################################

# 2) (b) Adopt the appropriate conjugate prior to the parameter λ  (Hint: Choose hyperparameters optionally within the support of distribution).                                                                                 

poisson <- rpois(20, lambda)

m <- max(c(poisson,data2))

prior <- dgamma(x = 0:m,shape = 5,rate=1)

which(prior==max(prior))-1
##############################################################################################################

# 2) (c) Using (a) and (b), find the posterior distribution of  λ


Posterior.func<-function(x,y){
  post <- NULL
  for (l in 0:max(x)){
    p<-1
    for(i in 0:max(x)){
      p <- p*dgamma(l, shape= y + i,rate = 2)
    }
    post <- rbind(post,c(l,p))
  }
  post <- data.frame(post)
  names(post) <- c("Lambda","posterior")
  sumpost <- sum(post$posterior)
  post$posterior <- post$posterior / sumpost
  post
}

post <- Posterior.func(x = data2,y = lambda)


##############################################################################################################

# 2) (d) Compute the minimum Bayesian risk estimator of  λ. 



###########################################################################################################################################################################################################################


# An opinion poll surveyed a simple random sample of 1000 students. Respondents were classified by gender (male or female) and by opinion (Reservation for women, No Reservation, or No Opinion). 
# Results are shown in the observed contingency table below.

# Does the gender and opinion on women reservation are independent?  Use a 0.05 level of significance. To do so, 


# Q3 a) State the hypotheses.


cat("H0 - Gender and Opionion on Women Reservation are independent.\nH1 - Gender and Opionion on Women Reservation are not independent")


##############################################################################################################

# Q2 b)	Find the statistic and critical values

fulltab <- matrix(c(200,150,50,400,250,300,50,600,450,450,100,1000),ncol=4,byrow=TRUE)            # Full table
colnames(fulltab) <- c("Yes","No","Cantsay","RowTotal")
rownames(fulltab) <- c("Male","Female","ColumnTotal")
fulltab <- as.table(fulltab)
fulltab


tab <- matrix(c(200,150,50,250,300,50),ncol=3,byrow=TRUE)                                         # Table for Chi-sq
colnames(tab) <- c("Yes","No","Cantsay")
rownames(tab) <- c("Male","Female")
tab <- as.table(tab)
tab

cat("For this sample this, I will use the chi-square test for independence.")

summary(tab)                                                                              # Displays Number of cases, factors and which test, In this case we use the chi-square test

chisq.test(tab)[1]                                                                        # Chi squared value (Test Statistic)
qchisq(1-0.0003029775, 2)

chisq.test(tab)$p.value                                                                   # p.value P(Χ2 > 16.2) = 0.0003.


chisq.test(tab)[2]                                                                        # Degree of freedom value


t.test(tab)[1]                                                                            # T- Value

qchisq(.95, df=2)                                                                         # Critical vales at 5% level of significance with degree of freedom = 2 for one tailed test is 5.991465
abs(qt(0.05, 2))

##############################################################################################################


# Q2 c)	Explain your decision and Interpret results

e11 = fulltab[1,4]*fulltab[3,1]/fulltab[3,4]                                          # Calculate expected frequency
e21 = fulltab[2,4]*fulltab[3,2]/fulltab[3,4] 
e12 = fulltab[1,4]*fulltab[3,2]/fulltab[3,4]
e22 = fulltab[2,4]*fulltab[3,1]/fulltab[3,4]
e13 = fulltab[1,4]*fulltab[3,3]/fulltab[3,4]
e33 = fulltab[2,4]*fulltab[3,3]/fulltab[3,4]

expectedtab <- matrix(c(e11,e12,e13,400,e21,e22,e33,600,450,450,100,1000),ncol=4,byrow=TRUE)                      # Table of expected frequencies
colnames(expectedtab) <- c("Yes","No","Cantsay","RowTotal")
rownames(expectedtab) <- c("Male","Female","ColumnTotal")
expectedtab <- as.table(expectedtab)
expectedtab
  
cat("The P-value",chisq.test(tab)$p.value,"is less than the significance level 0.05 so we cannot accept the null hypothesis. Therefore, In conclusion there is a relationship between gender and Opionion on Women Reservation ")

cat("The sampling method wasrandom sampling")

cat("The variables were categorical")








