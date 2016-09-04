ll <- function(mu, alpha, delta, beta) {
  -sum(log(alpha*delta*besselK(x = alpha*sqrt(delta^2 + (x - mu)^2), nu = 1)/(pi*sqrt(delta^2 + (x-mu)^2))*exp(delta*sqrt(alpha^2 - beta^2) + beta*(x - mu))))
}
#Estimate coef using MLE
getSymbols("GOOG", from = "2015-01-01", to = "2016-01-01")
goog <- GOOG[, 6]
goog.ret <- na.omit(Delt(goog))
goog.log <- log(goog)
x <- na.omit(diff(goog.log))
par <- mle(minuslogl = ll, start = list(mu = 0, alpha = 1, beta = 0, delta = 1))
par <- par@coef
mu <- par[1]
alpha <- par[2]
delta <- par[3]
beta <- par[4]
gamma <- sqrt(alpha^2 - beta^2)
#simulate 1000 variables following NIG distribution with estimated parameters
r1 <- rnig(n = 1000, mu = mu, delta = delta, alpha = alpha, beta = beta) 
plot(density(r1), main = "Density of Google log returns and NIG returns", lty = 1, lwd = 2) #plot density function of simulated distribution
lines(density(x), col = 2, lty = 2, lwd = 2) #add density of original Google log returns
legend("topright", c("Google log returns", "NIG returns"), lty = c(1, 2))
library(statmod)
sigma2 <- rinvgauss(n = 250, mean = delta/gamma, shape = delta^2) #simulate IG(delta, gamma)
sigma <- sqrt(sigma2)
epsilon <- rnorm(n = 250, mean = 0, sd = 1) #simulate N(0, 1)
r <- mu + beta*sigma2 +sigma*epsilon #simulate r_i
mean(r)
plot(density(r), main = "Density function of r_i") #plot simulated log returns
r.cumsum <- cumsum(r) #calculate cumulative log returns R_t
S0 <- 70
path <- S0*exp(r.cumsum) #S_t = S_0*exp(R_t)
plot(path, type = "l", main = "Simulated stock path", ylab = "Price", xlab = "Step")

g <- data.frame(Google = x)
head(g)
names(g) <- "Google"
nig <- data.frame(NIG = r)
names(nig) <- "NIG"

pl1 <- ggplot(data = g, aes(x = Google, colour = "Google")) + geom_density(size = 1)
pl2 <- pl1 + geom_density(data = nig, aes(x = NIG, colour = "r_i"), size = 1)
pl2 <- pl2 + ggtitle("Density of log returns") + ylab("Density")
pl2 <- pl2 + theme(plot.title = element_text(lineheight=.8, face="bold"), legend.text = element_text(size = 12))
pl2 

######################################################
##FUNCTION FOR NIG DISTRIBUTION
#Download daily stock data
setwd("D:/R")
goog.1H <- read.csv("GOOG.1H.csv", header = TRUE, sep = ";", dec = ".")
goog.1H <- goog.1H$X.CLOSE.
goog.1H.log <- log(goog.1H)
x <- diff(goog.1H.log)
#Estimate and extract parameters
par <- mle(minuslogl = ll, start = list(mu = 0, alpha = 1, beta = 0, delta = 1))
summary(par)
par <- par@coef
mu <- par[1]
alpha <- par[2]
delta <- par[3]
beta <- par[4]
gamma <- sqrt(alpha^2 - beta^2)

#NIG function
S0 <- 70
K <- 70
TTM <- 100
Ncol <- 5000
trade_days <- 252
rf <- 0.05
hours <- round(x = length(x)/trade_days, digits = 0)
mc_nig <- function(S0, K, TTM, Ncol = 1000, rf, hours = 7, mu, alpha, delta, beta, type = "Call") {
  gamma <- sqrt(alpha^2 - beta^2)
  T <- 1/(7*252) #7 working hours, 252 working days
  drift_GBM <- (rf - 0.5*sigma^2)*T 
  drift_NIG <- mu + beta*delta/gamma
  k <- as.numeric(drift_GBM/drift_NIG) #drift correction term
  sigma <- rinvgauss(n = TTM*hours*Ncol, mean = delta/gamma, shape = delta^2)
  m1 <- matrix(mu*k + beta*sigma*k + sqrt(sigma)*rnorm(TTM*hours*Ncol), ncol = Ncol)
  m2 <- apply(m1, 2, cumsum)
  m3 <<- S0*exp(m2)
  m4 <- tail(m3)
  if (type == "Call") {
    m5 <- mean(pmax(m4 - K, 0))*exp(-rf*TTM/360)
  }
  else {m5 <- mean(pmax(K - m4, 0))*exp(-rf*TTM/360)}
  m6 <- as.data.frame(m3)
  m6$Step <- seq(0, nrow(m6)-1, 1)
  m6 <- gather(data = m6, key = Path, value = Price, -Step)
  plot_paths <<- ggplot(m6, aes(x = Step, y = Price, color = Path)) + geom_line() + theme(legend.position = "none")
  return(m5)
}

##NIG function for asian option
mc_nig_asian <- function(S0, K, TTM, Ncol, rf, hours, mu, alpha, delta, beta, type = "Call") {
  gamma <- sqrt(alpha^2 - beta^2)
  T <- 1/(7*252) #7 working hours, 252 working days
  drift_GBM <- (rf - 0.5*sigma^2)*T 
  drift_NIG <- mu + beta*delta/gamma
  k <- as.numeric(drift_GBM/drift_NIG) #drift correction term
  sigma <- rinvgauss(n = TTM*hours*Ncol, mean = delta/gamma, shape = delta^2)
  m1 <- matrix(mu*k + beta*sigma*k + sqrt(sigma)*rnorm(TTM*hours*Ncol), ncol = Ncol)
  m2 <- apply(m1, 2, cumsum)
  m3 <<- S0*exp(m2)
  m4 <- apply(m3, 2, mean)
  if (type == "Call") {
    m5 <- mean(pmax(m4 - K, 0))*exp(-rf*TTM/360)
  }
  else {m5 <- mean(pmax(K - m4, 0))*exp(-rf*TTM/360)}
  m6 <- as.data.frame(m3)
  m6$Step <- seq(0, nrow(m6)-1, 1)
  m6 <- gather(data = m6, key = Path, value = Price, -Step)
  plot_paths <<- ggplot(m6, aes(x = Step, y = Price, color = Path)) + geom_line() + theme(legend.position = "none")
  return(m5)
}


#Расчет цен опционов по BSM, GBM и NIG (без корректировки) моделям и сравнение математического ожидания доходностей
bs.opm(S = S0, K = K, T = TTM/360, riskfree = rf, sigma = sigma, type = "Call")
mc(S_0 = S0, Nsim = Ncol, step = 1/7, rf = rf, sigma = sigma, TTM = TTM, K = K)
mc_nig(S0 = S0, K = K, TTM = TTM, Ncol = Ncol, rf = rf, hours = 7, mu = mu, alpha = alpha, delta = delta, beta = beta)
dt <- 1/7/252
r1 <- (exp((rf - 0.5*sigma^2)*dt))^700
r2 <- (exp(mu + beta*delta/gamma))^700

#Расчет цен азиатских опционов по GBM и NIG моделям и сравнение результатов
S0 <- 70
K <- 70
TTM <- 100
rf <- 0.05
sigma <- 0.3
Ncol <- 1000

gbm <- vector()
nig <- vector()
for (i in 1:30) {
  gbm[i] <- mc_asian(S_0 = S0, Nsim = 1000, step = 1/7, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
}
for (i in 1:30) {
  nig[i] <- mc_nig_asian(S0 = S0, TTM = TTM, Ncol = 1000, rf = rf, mu = mu, alpha = alpha, delta = delta, beta = beta, hours = 7)
}
mean(gbm)
mean(nig)
data <- data.frame(gbm, nig)
names(data) <- c("GBM", "NIG")
data$Sim <- seq(1, 30, 1)
m1 <- mean(gbm)
m2 <- mean(nig)
data$GBMmean <- m1
data$NIGmean <- m2
datam <- gather(data = data, key = "Model", value = "Price", -Sim)
p1 <- ggplot(data = datam, aes(y = Price, x = Sim, colour = Model)) + geom_line(size = 1)
p2 <- p1 + xlab("# of simulation")
p2
t.test(gbm, nig)

####Расчет цен опционов по разным моделям для различных страйков
#Опцион около денег, S0 = 70, K = 70
S0 <- 70
K <- 70
gbm <- vector()
nig <- vector()
for (i in 1:30) {
  gbm[i] <- mc_asian(S_0 = S0, Nsim = 1000, step = 1/7, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
}
for (i in 1:30) {
  nig[i] <- mc_nig_asian(S0 = S0, K = K, TTM = TTM, Ncol = 1000, rf = rf, mu = mu, alpha = alpha, delta = delta, beta = beta, hours = 7)
}
m1 <- mean(gbm)
m2 <- mean(nig)

#Опцион без денег, S0 = 70, K = 80, S0 <- 70
S0 <- 70
K <- 80
gbm <- vector()
nig <- vector()
for (i in 1:30) {
  gbm[i] <- mc_asian(S_0 = S0, Nsim = 1000, step = 1/7, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
}
for (i in 1:30) {
  nig[i] <- mc_nig_asian(S0 = S0, TTM = TTM, Ncol = 1000, rf = rf, mu = mu, alpha = alpha, delta = delta, beta = beta, hours = 7)
}
m3 <- mean(gbm)
m4 <- mean(nig)

#Опцион без денег, S0 = 70, K = 100, S0 <- 70
S0 <- 70
K <- 90
gbm <- vector()
nig <- vector()
for (i in 1:30) {
  gbm[i] <- mc_asian(S_0 = S0, Nsim = 1000, step = 1/7, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
}
for (i in 1:30) {
  nig[i] <- mc_nig_asian(S0 = S0, TTM = TTM, Ncol = 1000, rf = rf, mu = mu, alpha = alpha, delta = delta, beta = beta, hours = 7)
}
m5 <- mean(gbm)
m6 <- mean(nig)

#Опцион в деньгах, S0 = 70, K = 60, S0 <- 70
S0 <- 70
K <- 60
gbm <- vector()
nig <- vector()
for (i in 1:30) {
  gbm[i] <- mc_asian(S_0 = S0, Nsim = 1000, step = 1/7, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
}
for (i in 1:30) {
  nig[i] <- mc_nig_asian(S0 = S0, TTM = TTM, Ncol = 1000, rf = rf, mu = mu, alpha = alpha, delta = delta, beta = beta, hours = 7)
}
m7 <- mean(gbm)
m8 <- mean(nig)

#Опцион в деньгах, S0 = 70, K = 40, S0 <- 70
S0 <- 70
K <- 50
gbm <- vector()
nig <- vector()
for (i in 1:30) {
  gbm[i] <- mc_asian(S_0 = S0, Nsim = 1000, step = 1/7, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
}
for (i in 1:30) {
  nig[i] <- mc_nig_asian(S0 = S0, TTM = TTM, Ncol = 1000, rf = rf, mu = mu, alpha = alpha, delta = delta, beta = beta, hours = 7)
}
m9 <- mean(gbm)
m10 <- mean(nig)

t <- data.frame(GBM = c(m1, m3, m5, m7, m9), NIG = c(m2, m4, m6, m8, m10))
t$Strike <- c(70, 80, 90, 60, 50)
t <- t[order(t$Strike), ]
t <- gather(data = t, key = Model, value = Price, -Strike)
ggplot(data = t, aes(x = Strike, y = Price, colour = Model)) + geom_line(size = 1) + geom_point(size = 2.5)

