## Get data set up
gdp <- read.csv("GDP_data.csv")
gdp <- gdp[,-c(1,2)]
x <- model.matrix(GR6096~., data=gdp)[,-1]
y <- gdp$GR6096

## Separate into training and test data sets (only 15 because so few observations)
test <- sample(nrow(gdp), 20)
train <- (-test)

## Set up lasso regression
grid <- 10^seq(10,-10, length=1000)
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y[test])^2)

out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:68,]
lasso.coef
lasso.coef[-1] <- (lasso.coef[-1]/apply(x, 2, sd))
lasso.pred <- predict(out, s=bestlam, newx=x)
length(lasso.coef[lasso.coef != 0])

resids <- as.numeric(lasso.pred - y)
class(y)
class(resids)
pdf("cs2plots.pdf")
par(mfrow=c(2,2))
qqnorm(resids)
qqline(resids)
plot(lasso.pred, resids, main="Residuals vs Fitted Values", xlab="Fitted Values", ylab="Residuals", pch=20)
acf(resids, main="Series Residuals")
dev.off()
str(lasso.mod)
plot(lasso.mod)
plot(lasso.pred-y)
qqnorm(lasso.pred-y)
qqline(lasso.pred-y)



boot <- function(nreps=100, index) {
  results <- matrix(nrow=nreps, ncol=68)
  for (i in 1:nreps) {
    lasso.mod <- glmnet(x[index,], y[index], alpha=1, lambda=grid)
    cv.out <- cv.glmnet(x[index,], y[index], alpha=1)
    bestlam <- cv.out$lambda.min
    out <- glmnet(x[index,], y[index], alpha=1, lambda=grid)
    tlasso.coef <- predict(out, type="coefficients", s=bestlam)[1:68,]
    tlasso.coef[-1] <- tlasso.coef[-1]/apply(x[index,], 2, sd)
    results[i, ] <- lasso.coef
  }
  colnames(results) <- names(lasso.coef)
  return(results)
}

boot.results <- boot(1000, sample(nrow(gdp), nrow(gdp), replace=T))

lower.ci <- lasso.coef - qt(0.975, df=51)*apply(boot.results, 2, sd)
upper.ci <- lasso.coef + qt(0.975, df=51)*apply(boot.results, 2, sd)
uapply(boot.results, 2, sd)
lasso.coef[lasso.coef!=0]
length(apply(boot.results, 2, sd)[apply(boot.results,2,sd) != 0])
