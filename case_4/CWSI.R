source("fitMaternGP.R")

water <- read.csv("CWSI.csv")
plot(water$CWSI, water$SWC, pch=20, xlab="CWSI", ylab="SWC", main="Soil Water Content and CWSI")
nu <- 4.5
X <- matrix(rep(1, length(water$SWC)), ncol=1)

params <- fit.Matern.GP(as.matrix(water$SWC), matrix(rep(1, length(water$SWC)), ncol=1), as.matrix(water$CWSI), nu)

phi <- params$phi
s2 <- params$sigma2
tau2 <- params$tau2
mu <- params$beta.hat

pred.seq <- seq(0,1,length=500)

N <- length(water$CWSI)
K <- length(pred.seq)

D <- rdist(c(pred.seq, water$CWSI))
V <- s2*Matern(D,alpha=phi, nu=nu) + tau2*diag(nrow(D))

EY <- mu + V[1:K, K+(1:N)]%*%solve(V[K+(1:N), K+(1:N)])%*%as.matrix((water$SWC-mu))
VarY <- diag((V[1:K,1:K])-V[1:K,K+(1:N)]%*%solve(V[K+(1:N),K+(1:N)])%*%t(V[1:K, K+(1:N)]))
upper <- qnorm(0.975, mean=EY, sd=sqrt(VarY))
lower <- qnorm(0.025, mean=EY, sd=sqrt(VarY))
plot(water$CWSI, water$SWC, pch=20, xlab="CWSI", ylab="SWC", main="Soil Water Content and CWSI")
lines(pred.seq, EY, col="firebrick4")
lines(pred.seq, upper, col="deepskyblue3", lty=4)
lines(pred.seq, lower, col="deepskyblue3", lty=4)

nu <- 4.5
predict.width <- numeric(nrow(water))
mse <- numeric(nrow(water))
coverage <- numeric(nrow(water))

for (i in 1:nrow(water)) {
  train <- water[-i, ]
  y <- water[i,]
  params <- fit.Matern.GP(as.matrix(train$SWC), matrix(rep(1, nrow(train)), ncol=1), as.matrix(train$CWSI), nu)
  
  phi <- params$phi
  s2 <- params$sigma2
  tau2 <- params$tau2
  mu <- params$beta.hat
  K <- 1
  N <- nrow(train)
    
  D <- rdist(c(y$CWSI, train$CWSI))
  V <- s2*Matern(D, alpha=phi, nu=nu) + tau2*diag(nrow(D))
  EY <- mu + V[1:K, K+(1:N)]%*%solve(V[K+(1:N), K+(1:N)])%*%as.matrix((train$SWC-mu))
  # Calculate mse for each point
  mse[i] <- (EY - y$SWC)^2
    
  VarY <- diag((V[1:K,1:K])-matrix(V[1:K,K+(1:N)],nrow=1)%*%solve(V[K+(1:N),K+(1:N)])%*%t(matrix(V[1:K, K+(1:N)], nrow=1)))
  upper <- qnorm(0.975, mean=EY, sd=sqrt(VarY))
  lower <- qnorm(0.025, mean=EY, sd=sqrt(VarY))
    
  coverage[i] <- lower <= y$SWC && y$SWC <= upper 
  predict.width[i] <- upper - lower
}

pdf("mse_for_nu.pdf")
plot(nu.vals, nu.mse, pch=19, main=bquote("MSE for different values of " ~ nu), xlab=bquote(nu), ylab="MSE", col="chartreuse4")
dev.off()

## Confidence intervals for mu
ci.mu <- mu + c(-1,1) * 1.96 * sqrt(solve(t(X)%*%solve(V[K:K+(1:N), K:K+(1:N)])%*%X))
