library(car)

setwd("/Users/jacobmortensen/School/Winter 2015/Stat536/cases/5/")
crash <- read.csv("Crash.csv")

# Drop year because all crashes occurred in the same year
crash$Year <- NULL

# Remove missing values
crash <- crash[which(crash$Hour != 99),]
crash <- crash[which(crash$Mod_year != 9999),]
crash <- crash[which(crash$Light != "Unknown"),]

# Recode car type
crash$Car.Type <- recode(crash$Car.Type, "c('2-Door Sedan', '4-Door Sedan', 'Convertible', 'Station Wagon') = 'Car'")
crash$Car.Type <- recode(crash$Car.Type, "c('Large Van', 'Minivan') = 'Van'")
crash$Car.Type <- recode(crash$Car.Type, "c('Small Truck', 'Standard Truck') = 'Truck'")

# Recode one person with 3 DWI's
crash$DWI[which(crash$DWI == 3)] <- 2

# Dichotomize belt to account for low numbers of observations in categories.
crash$Belt <- recode(crash$Belt, "c('Shoulder Belt Only Used', 'Lap Belt Only Used', 'DOT-Helmet', 
                      'Other Helmet', 'Shoulder and Lap Belt Used')
                      = 'Safety Device Used'; 'Not Reported'='Unknown';
                     c('No Helmet', 'None Used-Motor Vehicle Occupant', 'Not Applicable') = 'None Used'")

# Recode these into sensible categories
crash$DOW <- recode(crash$DOW, "c(1,6,7) = 'Weekend'; c(2,3,4,5) = 'Weekday'")
crash$Hour <- recode(crash$Hour, "c(6,7,8,9,16,17,18) = 1; c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,19,20,21,22,23) = 0")

# Recode Speed.Limit
crash$Speed.Limit <- cut(crash$Speed.Limit, c(0, 30, 50, 60, 70), labels=c("30 or less", "35 to 50", "55 to 60", "65 or greater"))

# Split into test and training set to assess misclassification
set.seed(3)

train <- sample(1:nrow(crash), 0.7*nrow(crash))
test <- -c(train)
  
train.mod <- glm(Fatal ~ DWI + Age + Car.Type + Drugs + Drink + Light + Belt + Route + Speed.Related + Speed.Limit + Road.Type + Distracted, data=crash[train,], family=binomial)
train.probs <- predict(train.mod, crash[test,], type="response")

train.pred <- rep(0, length(train.probs))
train.pred[train.probs > 0.5] = 1
mean(train.pred == crash$Fatal[test])
stargazer(table(train.pred, crash$Fatal[test]))


# Fit full model and use backwards AIC selection to select variables.
# Note that we do not include interactions because there are two few observations in some of
# the interaction categories and because it makes interpretation much easier
mod <- glm(Fatal~., data=crash, family=binomial)
aic.mod <- step(mod, direction="backward")
the.glm <- summary(aic.mod)
upper <- the.glm$coef[,1] + qnorm(0.975)*the.glm$coef[,2] 
lower <- the.glm$coef[,1] - qnorm(0.975)*the.glm$coef[,2]
results <- cbind(the.glm$coef[,1], lower, upper, sapply(the.glm$coef[,1], exp), the.glm$coef[,4])
stargazer(results)
vif(aic.mod)














