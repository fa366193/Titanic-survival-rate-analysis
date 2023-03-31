#Libraries needed
library(dplyr) 
library(Amelia) 
library(ggplot2) 
library(scales) 
library(caTools) 
library(car) 
library(ROCR) 
library(e1071) 
library(rpart) 
library(rpart.plot) 
library(randomForest) 
library(caret) 

#Read CSV and create table
titanic_train <- read.csv('~/Desktop/train.csv')
titanic_test <- read.csv('~/Desktop/test.csv')

#Creating table with combined data
titanic <- bind_rows(titanic_train, titanic_test)
str(titanic)

#Massaging data and dealing with missing data
# Checking missing values (missing values or empty values)
colSums(is.na(titanic)|titanic=='')
missmap(titanic, main="Titanic Data - Missings Map",
        col=c("red", "blue"), legend=FALSE)

#Missing Fare Data
# Extract the row which contains the missing Fare
filter(titanic, is.na(Fare)==TRUE|Fare=='')

#plotting fare distribution
ggplot(filter(titanic, Pclass==3 & Embarked=="S"), aes(Fare)) +                       
  geom_density(fill="blue", alpha=0.5) +
  geom_vline(aes(xintercept=median(Fare, na.rm=T)), colour='darkblue', linetype='dashed', size=2) +
  geom_vline(aes(xintercept=mean(Fare, na.rm=T)), colour='red', linetype='dashed', size=2) +
  ggtitle("Fare distribution of third class passengers \n embarked from Southampton port") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Imput the missing Fare value by the median fare of third class passengers embarked from Southampton port
titanic$Fare[is.na(titanic$Fare)==TRUE] = median(filter(titanic, Pclass==3 & Embarked=="S")$Fare, na.rm=TRUE)
# Checking missing values
colSums(is.na(titanic)|titanic=='')

#Missing embarked data imputation
# Extract the rows which contain the missing Embarked values
filter(titanic, is.na(Embarked)==TRUE|Embarked=='')

# Frequency of ports of embarkation of first class passengers
table(filter(titanic, Pclass==1)$Embarked)

#Plotting fare distribution in first class passengers
ggplot(filter(titanic, is.na(Embarked)==FALSE & Embarked!='' & Pclass==1), 
       aes(Embarked, Fare)) +     
  geom_boxplot(aes(colour = Embarked)) +
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', size=2) +
  ggtitle("Fare distribution of first class passengers") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
# Impute the missing Embarked values by the Cherbourg port
titanic$Embarked[titanic$Embarked==""] = "C"
# Checking missing values
colSums(is.na(titanic)|titanic=='')

#Missing age data imputation
#plotting age distribution
ggplot(titanic,aes(Pclass,Age)) +                                                  
  geom_boxplot(aes(fill=factor(Pclass)),alpha=0.5) +
  ggtitle("Age distribution based on Pclass")

# Imputation of Age based on Pclass
impute.age <- function(age,class){
  vector <- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i] == 1){
        vector[i] <- round(mean(filter(titanic,Pclass==1)$Age, na.rm=TRUE),0)
      }else if (class[i] == 2){
        vector[i] <- round(mean(filter(titanic,Pclass==2)$Age, na.rm=TRUE),0)
      }else{
        vector[i] <- round(mean(filter(titanic,Pclass==3)$Age, na.rm=TRUE),0)
      }
    }else{
      vector[i]<-age[i]
    }
  }
  return(vector)
}
imputed.age <- impute.age(titanic$Age,titanic$Pclass)
titanic$Age <- imputed.age
colSums(is.na(titanic)|titanic=='')

#Passenger Titles
head(titanic$Name)
# Grab passenger title from passenger name
titanic$Title <- gsub("^.*, (.*?)\\..*$", "\\1", titanic$Name)
# Frequency of each title by sex
table(titanic$Sex, titanic$Title)

# First, I reassign few categories 
titanic$Title[titanic$Title == 'Mlle' | titanic$Title == 'Ms'] <- 'Miss' 
titanic$Title[titanic$Title == 'Mme']  <- 'Mrs' 

# Then, I create a new category with low frequency of titles
Other <- c('Dona', 'Dr', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Jonkheer', 'Major', 'Rev', 'Sir')
titanic$Title[titanic$Title %in% Other]  <- 'Other'

# Let's see if it worked
table(titanic$Sex, titanic$Title)
FamilySize <- titanic$SibSp + titanic$Parch + 1

table(FamilySize)
# Create a family size feature with three categories
titanic$FamilySize <- sapply(1:nrow(titanic), function(x) 
  ifelse(FamilySize[x]==1, "Single", 
         ifelse(FamilySize[x]>4, "Large", "Small")))

table(titanic$FamilySize)

#Data analysis
titanic$Survived = factor(titanic$Survived)
titanic$Pclass = factor(titanic$Pclass)
titanic$Sex = factor(titanic$Sex)
titanic$Embarked = factor(titanic$Embarked)
titanic$Title = factor(titanic$Title)
titanic$FamilySize = factor(titanic$FamilySize, levels=c("Single","Small","Large"))

#Checking the structure of the data
str(titanic)

#Plotting survival rate based on class
ggplot(filter(titanic, is.na(Survived)==FALSE), aes(Pclass, fill=Survived)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9, position="dodge") +
  scale_fill_brewer(palette = "Dark", direction = -1) +
  scale_y_continuous(labels=percent, breaks=seq(0,0.6,0.05)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Pclass") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting survival rate based on class and sex
ggplot(filter(titanic, is.na(Survived)==FALSE), aes(Sex, fill=Survived)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~Pclass) + 
  scale_fill_brewer(direction = -1) +
  scale_y_continuous(labels=percent, breaks=seq(0,0.4,0.05)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Pclass and Sex") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
theme_bw() 

#Plot for survival rate based on class and age
ggplot(filter(titanic, is.na(Survived)==FALSE), aes(Pclass, Age)) + 
  geom_violin(aes(fill=Survived), alpha=0.9) +
  facet_wrap(~Survived) + 
  scale_fill_brewer(direction = -1) +
  ggtitle("Survival Rate based on Pclass and Age") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#data analysis on title and family size
#Creating mosaic plot of survival rate based on the title
mosaicplot(~ Title + Survived, data=titanic, main='Survival Rate based on Title', shade=TRUE)

#Plotting survival rate based on class and title
ggplot(filter(titanic, is.na(Survived)==FALSE), aes(Title)) + 
  geom_bar(aes(fill=Survived), alpha=0.9, position="fill") +
  facet_wrap(~Pclass) + 
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Pclass and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Mosaic plot on survival rate based on family size
mosaicplot(~ FamilySize + Survived, data=titanic, main='Survival Rate based on FamilySize', shade=TRUE)

#Plotting survivale rate based on family size and title
ggplot(filter(titanic, is.na(Survived)==FALSE), aes(Title)) + 
  geom_bar(aes(fill=Survived), alpha=0.9, position="fill") +
  facet_wrap(~FamilySize) + 
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on FamilySize and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Data analysis on fare and embarked
ggplot(filter(titanic, is.na(Survived)==FALSE), aes(Embarked, Fare)) + 
  geom_boxplot(aes(fill=Survived), alpha=0.9) +
  facet_wrap(~Survived) + 
  scale_fill_manual(values=c("#56B4E9", "#CC79A7")) +
  ggtitle("Survival Rate based on Embarked and Fare") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#splitting the dataset into training set and test set
train_original <- titanic[1:891, c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FamilySize")]
test_original <- titanic[892:1309, c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FamilySize")]

# Splitting the Training set into the Training set and Validation set
set.seed(789)
split = sample.split(train_original$Survived, SplitRatio = 0.8)
train = subset(train_original, split == TRUE)
test = subset(train_original, split == FALSE)

#logic regression
# Show the correlation of numeric features
cor(train[,unlist(lapply(train,is.numeric))])

# Show the p-value of Chi Square tests
ps = chisq.test(train$Pclass, train$Sex)$p.value
pe = chisq.test(train$Pclass, train$Embarked)$p.value
pt = chisq.test(train$Pclass, train$Title)$p.value
pf = chisq.test(train$Pclass, train$FamilySize)$p.value
se = chisq.test(train$Sex, train$Embarked)$p.value
st = chisq.test(train$Sex, train$Title)$p.value
sf = chisq.test(train$Sex, train$FamilySize)$p.value
et = chisq.test(train$Embarked, train$Title)$p.value
ef = chisq.test(train$Embarked, train$FamilySize)$p.value
tf = chisq.test(train$Title, train$FamilySize)$p.value
cormatrix = matrix(c(0, ps, pe, pt, pf,
                     ps, 0, se, st, sf,
                     pe, se, 0, et, ef,
                     pt, st, et, 0, tf,
                     pf, sf, ef, tf, 0), 
                   5, 5, byrow = TRUE)
row.names(cormatrix) = colnames(cormatrix) = c("Pclass", "Sex", "Embarked", "Title", "FamilySize")
cormatrix

# Fitting Logistic Regression to the Training set
classifier = glm(Survived ~ ., family = binomial(link='logit'), data = train)

# Choosing the best model by AIC in a Stepwise Algorithm
# The step() function iteratively removes insignificant predictor variables from the model.
classifier <- step(classifier)

summary(classifier)
vif(classifier)

# Fitting Logistic Regression to the Training set again without the factor Sex 
classifier = glm(Survived ~ . -Sex, family = binomial(link='logit'), data = train)

# Choosing the best model by AIC in a Stepwise Algorithm
# The step() function iteratively removes insignificant features from the model.
classifier <- step(classifier)
summary(classifier)

vif(classifier)
durbinWatsonTest(classifier)

# Predicting the Validation set results
prob_pred = predict(classifier, type = 'response', newdata = test)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Checking the prediction accuracy
table(test$Survived, y_pred > 0.5)
error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Use the predictions to build a ROC curve to assess the performance of our model
fitpred = prediction(prob_pred, test$Survived)
fitperf = performance(fitpred,"tpr","fpr")
plot(fitperf,col="green",lwd=2,main="ROC Curve")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# Checking the variance of numeric features
paste('Age variance: ',var(train$Age),', SibSp variance: ',var(train$SibSp),', Parch variance: ',var(train$Parch),', Fare variance: ',var(train$Fare))

# Feature Scaling - use scale() to standardize the feature columns
standardized.train = cbind(select(train, Survived, Pclass, Sex, SibSp, Parch, Embarked, Title, FamilySize), Age = scale(train$Age), Fare = scale(train$Fare))
paste('Age variance: ',var(standardized.train$Age),', Fare variance: ',var(standardized.train$Fare))

standardized.test = cbind(select(test, Survived, Pclass, Sex, SibSp, Parch, Embarked, Title, FamilySize), Age = scale(test$Age), Fare = scale(test$Fare))
paste('Age variance: ',var(standardized.test$Age),', Fare variance: ',var(standardized.test$Fare))

# Fitting Linear SVM to the Training set
classifier = svm(Survived ~ .,
                 data = standardized.train,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Validation set results
y_pred = predict(classifier, newdata = standardized.test[,-which(names(standardized.test)=="Survived")])

# Checking the prediction accuracy
table(test$Survived, y_pred)

error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Fitting Non-linear SVM to the Training set
classifier = svm(Survived ~ .,
                 data = standardized.train,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Validation set results
y_pred = predict(classifier, newdata = standardized.test[,-which(names(standardized.test)=="Survived")])

# Checking the prediction accuracy
table(test$Survived, y_pred)

error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Tuning the model
# Applying Grid Search to find the best parameters
tune.results <- tune(svm,
                     Survived ~ .,
                     data = standardized.train,
                     kernel='radial',
                     ranges=list(cost=2^(-2:2), gamma=2^(-6:-2)))
summary(tune.results)
# The best non-linear SVM performance occurs with cost=4 and gamma=0.125

# Fitting Non-linear SVM to the Training set
classifier = svm(Survived ~ .,
                 data = standardized.train,
                 type = 'C-classification',
                 kernel = 'radial',
                 cost = 4,
                 gamma = 0.125)

# Predicting the Validation set results
y_pred = predict(classifier, newdata = standardized.test[,-which(names(standardized.test)=="Survived")])

# Checking the prediction accuracy
table(test$Survived, y_pred)

error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Fitting Decision Tree Classification Model to the Training set
classifier = rpart(Survived ~ ., data = train, method = 'class')

# Tree Visualization
rpart.plot(classifier, extra=4)

# Predicting the Validation set results
y_pred = predict(classifier, newdata = test[,-which(names(test)=="Survived")], type='class')

# Checking the prediction accuracy
table(test$Survived, y_pred) # Confusion matrix

error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Applying k-Fold Cross Validation
set.seed(789)
folds = createMultiFolds(train$Survived, k = 10, times = 5)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Survived ~ ., data = train, method = "rpart", trControl = control)

# Tree Visualization
rpart.plot(classifier_cv$finalModel, extra=4)

# Predicting the Validation set results
y_pred = predict(classifier_cv, newdata = test[,-which(names(test)=="Survived")])

# Checking the prediction accuracy
table(test$Survived, y_pred)
error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Fitting Random Forest Classification to the Training set
set.seed(432)
classifier = randomForest(Survived ~ ., data = train)

# Choosing the number of trees
plot(classifier)

# Predicting the Validation set results
y_pred = predict(classifier, newdata = test[,-which(names(test)=="Survived")])

# Checking the prediction accuracy
table(test$Survived, y_pred)

error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Applying k-Fold Cross Validation
set.seed(651)
folds = createMultiFolds(train$Survived, k = 10)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Survived ~ ., data = train, method = "rf", trControl = control)

# Predicting the Validation set results
y_pred = predict(classifier_cv, newdata = test[,-which(names(test)=="Survived")])

# Checking the prediction accuracy
table(test$Survived, y_pred) 

error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Feature Importance
gini = as.data.frame(importance(classifier))
gini = data.frame(Feature = row.names(gini), 
                  MeanGini = round(gini[ ,'MeanDecreaseGini'], 2))
gini = gini[order(-gini[,"MeanGini"]),]

ggplot(gini,aes(reorder(Feature,MeanGini), MeanGini, group=1)) + 
  geom_point(color='red',shape=17,size=2) + 
  geom_line(color='blue',size=1) +
  scale_y_continuous(breaks=seq(0,60,10)) +
  xlab("Feature") + 
  ggtitle("Mean Gini Index of Features") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Fitting Naive Bayes to the Training set
classifier = naiveBayes(Survived ~ ., data = train)

# Predicting the Validation set results
y_pred = predict(classifier, newdata = test[,-which(names(test)=="Survived")])

# Checking the prediction accuracy
table(test$Survived, y_pred) 

error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))


















