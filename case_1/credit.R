library(ggplot2)
library(leaps)

credit <- read.csv("Credit.csv")

### Exploratory Data Analysis
q0 <- qplot(Balance, data=credit, ylab="Count", 
            main="Distribution of Balance Amounts", binwidth=60)
q1 <- qplot(factor(Gender), Balance, data=credit,
            geom=c("boxplot"), main="Balance by Gender", xlab="Gender")
q2 <- qplot(factor(Ethnicity), Balance, data=credit, geom=c("boxplot"), 
            main="Balance by Ethnicity", xlab="Ethnicity")
q3 <- qplot(factor(Married), Balance, data=credit, geom=c("boxplot"), 
            main="Balance by Marital Status (all data)", 
            xlab="Marital Status") +
            theme_grey(base_size=9)
q4 <- qplot(factor(Married), Balance, data=subset(credit, Student == "Yes"), 
            geom=c("boxplot"),
             main="Balance by Marital Status (only students)", 
            xlab="Marital Status") +
            theme_grey(base_size=9)
q5 <- qplot(Income, Balance, data=credit, main="Balance by Income")
q6 <- qplot(Rating, Balance, data=credit, main="Balance by Rating")

## Cut out the limit variable due to high collinearity
credit <- credit[,!(names(credit) %in% c("Limit", "X"))]
set.seed(30)
train <- sample(c(TRUE, FALSE), nrow(credit), rep=TRUE)
test <- (!train)

## Do forward selection and select the model that minimizes BIC
regfit.full <- regsubsets(Balance~.^2, data=credit[train,], method="forward", nvmax=30)
regfit.sum <- summary(regfit.full)
which.min(regfit.sum$bic)
which.min(regfit.sum$cp)
which.max(regfit.sum$adjr2)
regfit.sum$outmat[which.min(regfit.sum$bic),]

## See which model produces the lowest predicted MSE
test.mat <- model.matrix(Balance~.^2, data=credit[test,])
val.errors <- rep(NA, 30)
se.errors <- rep(NA, 30)
for (i in 1:30) {
  coefi <- coef(regfit.full, id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((credit$Balance[test]-pred)^2)
}
which.min(val.errors)


fit1 <- lm(Balance~Rating+Income+Student+Income:Rating+Rating:Student, data=credit, subset=train)
sum1 <- summary(fit1)
agefit <- lm(Balance~Rating+Income+Student+Age+Income:Age+Income:Rating+Rating:Student, data=credit, subset=train)
summary(agefit)
plot(agefit)
pdf("assumption_plots.pdf")
par(mfrow=c(2,2))
plot(fit1, pch=20, which=c(1,2,5))
plot(rstudent(fit2), type="b", pch=20, ylab="Studentized Residuals", main="Correlation of Error Terms")
abline(h=0)
dev.off()

simplefit <- lm(Balance~Rating+Income+Student, data=credit, subset=train)

nolvg <- train
nolvg[262] <- FALSE


fit2 <- lm(Balance~Rating+Income+Student+Income:Rating+Rating:Student, data=credit, subset=nolvg)
sum2 <- summary(fit2)
confint(fit2)
par(mfrow=c(2,2))
plot(fit2, pch=20)
plot(hat(model.matrix(fit2)), type="h")
credit[nolvg,]
sum1$coefficients
sum2$coefficients

pdf("normality_check.pdf")
par(mfrow=c(2,2), ps=11)
qqnorm(credit$Balance, main="Untransformed Data", pch=20)
qqline(credit$Balance)
qqnorm(credit$Balance^(2/3), main="Transformation Suggested by Box-Cox", pch=20)
qqline(credit$Balance^(2/3))
qqnorm(log(1+credit$Balance), main="Log Transformation", pch=20)
qqline(log(1+credit$Balance))
dev.off()
shapiro.test(credit$Balance)
shapiro.test(credit$Balance^(2/3))
shapiro.test(log(1+credit$Balance))

shapiro.test(rstudent(fit2))
pdf("correlation_of_error_terms.pdf")
plot(rstudent(fit2), type="b", pch=20, ylab="Studentized Residuals", main="Correlation of Error Terms")
abline(h=0)
dev.off()

calc.coverage <- function(lmfit, test) {
  pred <- predict(lmfit, credit, interval="predict")
  misses <- sum(pred[test,2] >= credit$Balance[test]) + sum(credit$Balance[test] >= pred[test,3])
  print(misses)
  print(sum(test))
  return(1-misses/sum(test))
}

calc.coverage(fit1, test)
calc.coverage(agefit, test)
calc.coverage(simplefit, test)

pred <- predict(fit1, credit[test,], interval="predict")
predage <- predict(agefit, credit[test,], interval="predict")

plot(simplefit)
sum(pred[,2] <= credit$Balance[test])
sum(pred[,3] >= credit$Balance[test])
