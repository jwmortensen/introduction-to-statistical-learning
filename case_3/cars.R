library("splines")

cars <- read.csv("Cars.csv")

## Combine colors with small amounts into an "Other" category
new.levels <- c("Other", "Black", "Blue", "Green", "Grey", "Red", "Silver", "Other", "White", "Other")
cars$Color <- factor(new.levels[cars$Color])
cars$Mfg_Year <- as.factor(cars$Mfg_Year)

## Combine cc into two categories
cars$cc[which.max(cars$cc)] <- 1600
cars$cc <- cut(cars$cc, c(0, 1600, 2000), right=TRUE)

## Collapse values of HP with few observations
cars$HP <- ifelse(cars$HP == 71 | cars$HP == 73, 72, cars$HP)
cars$HP <- ifelse(cars$HP == 98, 97, cars$HP)
cars$HP <- ifelse(cars$HP == 116, 110, cars$HP)

## Combine values for doors
cars$Doors <- ifelse(cars$Doors == 3, 2, cars$Doors)
cars$Doors <- ifelse(cars$Doors == 5, 4, cars$Doors)

## Make sure all factors are treated as factors
factornames <- colnames(cars[,c(5,7:10,12:13,15:25)])
factornames
for (i in factornames) {
  cars[,i] <- as.factor(cars[,i])
  print(i)
  print(class(cars[,i]))
}

## Separate into training and test data sets
train <- sample(c(TRUE, FALSE), nrow(cars), replace=TRUE)
test <- !train
cars[train,]

## full model fit using training data
fullmod = lm(Price~ns(Miles, df=4) +
           Mfg_Year +
           Fuel_Type +
           HP +
           Color +
           Automatic +
           cc +
           Doors +
           Weight +
           Mfr_Guarantee +
           ABS +
           Airbag_1 +
           Airbag_2 +
           Airco +
           Automatic_airco +
           Boardcomputer +
           CD_Player +
           Powered_Windows +
           Power_Steering +
           Radio,
          data=cars[train,])

## Pick model with lowest AIC
selectmod <- step(fullmod)

## Test model with test data set
selectpred <- predict(selectmod, newdata=list(Miles=cars$Miles[test],
                                       Mfg_Year=cars$Mfg_Year[test],
                                       Fuel_Type=cars$Fuel_Type[test],
                                       HP=cars$HP[test],
                                       Color=cars$Color[test],
                                       Automatic=cars$Automatic[test],
                                       cc=cars$cc[test],
                                       Doors=cars$Doors[test],
                                       Weight=cars$Weight[test],
                                       Mfr_Guarantee=cars$Mfr_Guarantee[test],
                                       ABS=cars$ABS[test],
                                       Airbag_1=cars$Airbag_1[test],
                                       Airbag_2=cars$Airbag_2[test],
                                       Airco=cars$Airco[test],
                                       Automatic_airco=cars$Automatic_airco[test],
                                       Boardcomputer=cars$Boardcomputer[test],
                                       CD_Player=cars$CD_Player[test],
                                       Powered_Windows=cars$Powered_Windows[test],
                                       Power_Steering=cars$Power_Steering[test],
                                       Radio=cars$Radio[test]
                                       ), interval="prediction")

# Calculate coverage and mean PI width
coverage <- mean(selectpred[,2] <= cars$Price[test] & cars$Price[test] <= selectpred[,3])
coverage
mean(selectpred[,3]-selectpred[,2])

## Model with variables selected by the AIC criterion fitted on all the data
finalmod = lm(Price~ns(Miles, df=4) +
           Mfg_Year +
           Fuel_Type +
           HP +
           Automatic +
           Doors +
           Weight +
           Mfr_Guarantee +
           Airco +
           Automatic_airco +
           Boardcomputer +
           Powered_Windows,
          data=cars)

## Calculate residuals
resids <- rstudent(finalmod)

## Generate fitted vs residuals plot
pdf("fitted.pdf")
plot(fitted(finalmod), resids, pch=20, main="Fitted vs. Residuals", xlab="Fitted Values", ylab="Studentized Residuals")
abline(h=0)
dev.off()

## qq plot of the residuals
pdf("normal.pdf")
qqnorm(resids, pch=20)
qqline(resids)
dev.off()

## Index plot of the residuals
pdf("corr.pdf")
plot(resids, type="b", pch=20, main="Index Plot of the Residuals", xlab="Index", ylab="Residuals")
dev.off()

## Plot showing relationship between miles and price with the spline
fit <- lm(Price~ns(Miles,df=3), data=cars)
preds <- predict.lm(fit,newdata=list(Miles=seq(0,150000,length=10000)), se=TRUE)

pdf("miles.pdf")
plot(cars$Miles,cars$Price,pch=19,col="gray",cex=.5, main="Price vs. Miles", xlab="Miles", ylab="Price")
lines(seq(0,150000,length=10000),preds$fit,col="red",lwd=3)
dev.off()
