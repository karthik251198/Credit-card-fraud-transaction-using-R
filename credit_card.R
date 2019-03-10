library(caTools)

df <- read.csv('creditcard.csv')
str(df)

any(is.na(df))
summary(df)

#converting Class attribute from int type to factor
df$Class <- as.factor(df$Class)

#Splitting data and training and testing it
sample <- sample.split(df$Class, SplitRatio = 0.7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)

model <- glm(formula=Class ~ . , family = binomial(link='logit'),data = train)
predicted.values <- predict(model,newdata=test,type='response')

results <- ifelse(predicted.values > 0.5,1,0)
head(results)
summary(results)
