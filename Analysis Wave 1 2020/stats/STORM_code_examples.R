### Examples of estimating STORM models from synthetic data ###

# Logistic multilevel model

library(rstan)

# General simulation settings
N = 1000 # number of observations
L = 20 # number of groups
n_group <- N/L # number of observations per group
ll <- vector(length=N)
alpha_ll <- vector(length=N)
counter <- 1
for (ii in 1:L){
  ll[counter:(counter+n_group-1)] <- ii
  alpha_ll[counter:(counter+n_group-1)] <- rnorm(1)
  counter <- counter + n_group
}
D <- rbinom(N,1,0.6)

# Logistic multilevel model simulations
prob_logit <- exp(alpha_ll + 1*D)/(1+exp(alpha_ll + 1*D))
y <- rbinom(N,1,prob_logit)

Index <- !is.na(data$crisis_aware)
y <- data$crisis_aware[Index]
N <- length(y)
IDuni <- as.factor(data$Uni[Index])
tmp <- sort(unique(IDuni)) 

ll <- vector(length=N)
for (ii in 1:N){
  index2 <- IDuni[ii]==tmp
  ll[ii] <- which(index2 == TRUE)
}
L <- length(unique(ll))
D <- ifelse(data$UKRN[Index]=="UKRN",1,0)

# Estimate logistic multilevel model
data <- list(N=N, L=L, y=y, ll=ll, D=D)
warmup <- 1000
niter <- 2000
fit <- stan(file="C:/Users/marto05/OneDrive - Linköpings universitet/10. Side projects/2. STORM/STORM/Analysis Wave 1 2020/stats/STORM_logistic.stan",data=data, warmup=warmup,iter=niter,chains=4)


#Set2 - Awereness

Index <- !is.na(data$Awareness_Score_Total)
y <- data$Awareness_Score_Total[Index]
N <- length(y)
IDuni <- as.factor(data$Uni[Index])
tmp <- sort(unique(IDuni)) 

ll <- vector(length=N)
for (ii in 1:N){
  index2 <- IDuni[ii]==tmp
  ll[ii] <- which(index2 == TRUE)
}
L <- length(unique(ll))
D <- ifelse(data$UKRN[Index]=="UKRN",1,0)
U <- 8

data <- list(N=N, L=L, U=U, y=y, ll=ll, D=D)

# Truncated Poisson regression
U <- 50 #Upper truncation
y <- rpois(n=N,lambda=exp(alpha_ll + 1*D))

data <- list(N=N, L=L, U=U, y=y, ll=ll, D=D)
fit <- stan(file="C:/Users/marto05/OneDrive - Linköpings universitet/10. Side projects/2. STORM/STORM/Analysis Wave 1 2020/stats/STORM_truncPoisson.stan",data=data, warmup=warmup,iter=niter,chains=4)


ParDraws <- extract(fit)
 dim(ParDraws)

 ParDraws <- extract.parameters(fit)
 lambda_UKRN <- exp(ParDraws$mu + ParDraws$beta*1)
 lambda_NonUKRN <- exp(ParDraws$mu)
 plot(density(lambda_UKRN))
 lines(density(lambda_NonUKRN))
 plot(density(lambda_UKRN),col="blue")
 lines(density(lambda_NonUKRN),col="red")
 mean(lambda_UKRN > lambda_NonUKRN)
 
 #To see the magnitude of this difference. 
 #ParDraws <- extract(fit)
 
 #lambda_UKRN <- exp(ParDraws$mu + ParDraws$beta*1)
 #lambda_NonUKRN <- exp(ParDraws$mu)
 #plot(density(lambda_UKRN),col="blue")
 #lines(density(lambda_NonUKRN),col="red")
 #mean(lambda_UKRN > lambda_NonUKRN)
 

# Truncated linear regression
 
 
 
