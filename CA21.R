##############################################################################################################
# Prepare Data

dataset <- read.csv("college.csv") # Read in data (file must be in current Directory)
head(dataset)                      # Display first five rows
str(df)                            # Dataset has 400 observations and 4 variable
nrow(dataset)                      # Number of rows / observations


gre <- dataset$gre                 # Assign column GRE to gre ( GRE - Graduate Record Examinations )
gpa <- dataset$gpa                 # Assign column GPA to GPA ( GPA - Grade Point Average )
rank <- dataset$rank               # Assign column Rank to rank ( Rank - Rank 1 - 4 )
admit  <- dataset$admit            # Assign column Admint to admint ( Rank - Rank 1 - 4 )



dataset <- na.omit(data.frame(gre,gpa,rank,admit))     # remove all missing value from each column
sum(is.na(dataset))                                    # Check for NULL Values       

summary(dataset)                                       # Display min, 1st Q, median, mean, 3rd Qu, Max. Already we can see that
                                                       # the mean of admit is 0.3175 meaning there are more rejects then admits

##############################################################################################################

#Consider a relational dataset and specify your input and output variables , then

# 1) (a)	Train the model using 80% of this dataset and suggest an appropriate 
#         GLM to model output to input variables. 


n = nrow(dataset)                     # Add all row to "n"
indexes = sample(n,n*(80/100))        # Generate random sample (80%)

trainset = dataset[indexes,]          # 80% of the data
testset = dataset[-indexes,]          # 20% of the data

dim(testset)                          # Displays Dimension of testset


model.fit <- glm(admit ~ gre+gpa+rank, data=trainset, family="binomial")            # Predict Admit - gre, gpa and rank. Using Logistic regressions as data is discrete
                                                                       
summary(model.fit)                                                                  # Display summary of model  

##############################################################################################################

# 1)  (b) Specify the significant variables on the output variable at the level of 𝛼=0.05
#        and explore the related hypotheses test. Estimate the parameters of your model. 
