#Показать объем торгов деривативами
a <- read.csv("Derivatives stat.csv", header = T)
b <- a[a$Risk.category == "A:Total contracts", ]
b <- b[, -c(1:10)]
date <- seq.Date(from = as.Date("1998-06-30"), to = as.Date("2015-06-30"), by = "quarter")
for (i in seq(2, length(date), by = 2)) {
  date[i] <- NA
}
date <- na.omit(date)
colnames(b) <- date
c <- as.data.frame(t(b))
names(c) <- c("Gross", "Notional")
d <- apply(c, 2, as.numeric)
e <- d/1000
f <- as.data.frame(e)

#График объема торговли деривативами в трлн. долл.
plot <- ggplot(f, aes(x = date, y = Gross), color = Gross) + geom_line() + xlab("") + ylab("") +
   theme(axis.text = element_text(size = 13), panel.border = element_rect(fill = NA, colour = "black", size = 2))

my_ggplot <- function (data, x, y, xname = "", yname = "", mainname = "") {
  data <- as.data.frame(data)
  ggplot(data = data, aes(x = x, y = y)) + geom_line(color = "red") + xlab(xname) + ylab(yname) +
    theme(axis.text = element_text(size = 13), panel.border = element_rect(fill = NA, colour = "black", size = 2))
}
my_ggplot(f, date, f[, 1]) 



#Показать объем торговли фьючерсами и опционами
a <- read.csv("Futures and options volume.csv")
b <- a[a$Frequency == "Q:Quarterly" & a$Risk.category == "$C+B" & a$Location.of.trade..Exchange.or.country. == "8A:All exchanges", ]
c <- b[, -c(1:3, 5:7)]
d <- c[, 1:92]

date <- seq(from = as.Date("1993-03-01"), to = as.Date("2015-09-01"), by = "quarter")
names(d) <- c("Instrument", paste(date))
rownames(d) <- c("Futures", "Options")
d <- d[, -1]
e <- t(d)
e <- as.data.frame(e)
e <- apply(e, 2, as.numeric)
rownames(e) <- paste(date)

#Использование команды "gather" для последующего создания графика
f <- as.data.frame(date)
e <- cbind(e, f)
e <- gather(data = e, key = "Type", value = "Volume", -date)
e$Volume <- e$Volume/1000

#График объема торговли фьючерсами и опционами
ggplot(e, aes(x = date, y = Volume, linetype = Type, color = Type)) + geom_line(size = 1.2) + xlab("") + ylab("") + theme(axis.text = element_text(size = 13), panel.border = element_rect(fill = NA, colour = "black", size = 1.3))

#Показать прибыльность call опциона
S <- seq(0, 130, 1)
n <- length(S)
K <- rep(100, n)
a <- as.data.frame(cbind(S, K))
a$Option <- pmax(a$S - a$K, -5)
ggplot(a, aes(x = S, y = Option)) + geom_line(size = 1.2, color = "blue") + geom_hline(yintercept = 0, color = "red") + ylab("Profir, rub.") + xlab("Share price, rub.") + theme(axis.text = element_text(size = 13), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 16)) + scale_y_continuous(breaks = c(-5, 0 , 10, 20, max(a$Option)), limits = c(-10, 30))

S <- seq(0, 130, 1)
n <- length(S)
K <- rep(100, n)
a <- as.data.frame(cbind(S, K))
a$Option <- pmin(a$K - a$S, 5)
ggplot(a, aes(x = S, y = Option)) + geom_line(size = 1.2, color = "blue") + geom_hline(yintercept = 0, color = "red") + ylab("Profir, rub.") + xlab("Share price, rub.") + theme(axis.text = element_text(size = 13), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 16)) + scale_y_continuous(breaks = c(-5, 0, 20), limits = c(-10, 30))



#Показать прибыльность put опциона
S <- seq(0, 100, 1)
n <- length(S)
K <-rep(70, n)
a <- as.data.frame(cbind(S, K))
a$Option <- pmax(a$K - a$S, -7)
ggplot(a, aes(x = S, y = Option)) + geom_line(size = 1.2, color = "blue") + geom_hline(yintercept = 0, color = "red") + ylab("Profir, rub.") + xlab("Share price, rub.") + theme(scale_y_continuous(breaks = c(-7, 0 , 10, 20, max(a$Option)), limits = c(-10, 30)) + scale_x_continuous(limits = c(35, 100))


S <- seq(0, 100, 1)
n <- length(S)
K <-rep(70, n)
a <- as.data.frame(cbind(S, K))
a$Option <- pmin(a$S - a$K, 7)
ggplot(a, aes(x = S, y = Option)) + geom_line(size = 1.2, color = "blue") + geom_hline(yintercept = 0, color = "red") + ylab("Profir, rub.") + xlab("Share price, rub.") + theme(axis.text = element_text(size = 13), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 16)) + scale_y_continuous(breaks = c( 0 , 7, max(a$Option)), limits = c(-10, 30)) + scale_x_continuous(limits = c(55, 100))


#Показать влияние изменения безрисковой ставки на стоимость опциона колл 
rf <- seq(0, 8, 1)
price <-  6 + 0.1*rf
a <- as.data.frame(cbind(rf, price))
ggplot(a, aes(x = rf, y = price)) + geom_line(size = 1.2, color = "blue") + scale_y_continuous(limits = c(5, 7)) + xlab("Risk free rate, r (%)") + ylab("Price") + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20))


# Показать влияние зменения безрисковой ставки на стоимость опциона пут
rf <- seq(0, 8, 1)
price <-  6 + -0.1*rf
a <- as.data.frame(cbind(rf, price))
ggplot(a, aes(x = rf, y = price)) + geom_line(size = 1.2, color = "blue") + scale_y_continuous(limits = c(5, 7)) + xlab("Risk free rate, r (%)") + ylab("Price") + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20))

#График зависимости цены опциона колл от страйка
K <- seq(0, 100, 10)
S <- 50
price <- pmax(0, S - K)
a <- as.data.frame(cbind(K, price))
ggplot(a, aes(x = K, y = price)) + geom_line(size = 1.2, color = "blue") + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20)) + xlab("Strike, K") + ylab("Price")


#Зависимость цены опциона пут от страйка
K <- seq(0, 100, 10)
S <- 50
price <- pmax(0, K - S)
a <- as.data.frame(cbind(K, price))
ggplot(a, aes(x = K, y = price)) + geom_line(size = 1.2, color = "blue") + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20)) + xlab("Strike, K") + ylab("Price")


#Зависимость цены опциона колл от цены акции
S <- seq(0, 100, 10)
K <- 50
price <- pmax(0, S - K)
a <- as.data.frame(cbind(K, price))
ggplot(a, aes(x = S, y = price)) + geom_line(size = 1.2, color = "blue") + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20)) + xlab("Stock price, S") + ylab("Price")


#Зависимость цены опциона пут от цены акции
S <- seq(0, 100, 10)
K <- 50
price <- pmax(0, K - S)
a <- as.data.frame(cbind(K, price))
ggplot(a, aes(x = S, y = price)) + geom_line(size = 1.2, color = "blue") + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20)) + xlab("Stock price, S") + ylab("Price")


#Много Броуновского движение
S <- 100
K <- 100
time <- 1/360
rf <- 0.05
sigma <- 0.2
drift <- rf - 0.5*sigma^2
S_t <- matrix(nrow = 1/time, ncol = 20)
S_t[1, ] <- S
for (i in 1:ncol(S_t)) {
  for (j in 2:nrow(S_t)) {
  S_t[j, i] <- S_t[j-1, i]*exp(drift*time + sigma*sqrt(time)*rnorm(1))
  }
}
S_t <- as.data.frame(S_t)
S_t$number <- seq(1, nrow(S_t), 1)
S_T_plot <- gather(data = S_t, key = "Type", value = "Price", -number)
ggplot(S_T_plot, aes(x = number, y = Price, color = Type)) + geom_line() + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20), legend.position="none") + xlab("Time") + ylab("Price")
price_final <- as.numeric(S_t[nrow(S_t), -ncol(S_t)])
price_final_mean <- mean(price_final)
price_final_mean_adj <- (price_final_mean - K)*exp(-rf)
table <- xtable(S_t[1:20, 1:9])
print(table, booktabs = TRUE)



#Симуляция винеровского процесса с дрейфом и диффузией
Time <- 1
N <- 300
delta_t <- Time/N
S0 <- 0
W <- as.data.frame(rep(0, N))
W[1, ] <- S0
names(W) <- "Price"
W$Step <- seq(1, N, 1)
W$delta_t <- W$Step/N
for (i in 2:N) {
  W[i, 1] <- W[i-1, 1] + rnorm(1)*sqrt(W$delta_t[i])*1.5 + 0.2*W$delta_t[i]
}
ggplot(W, aes(x = Step, y = Price)) + geom_line() +xlab("Number of steps") + ylab("Price") + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20))



#Симуляция обычного стохастического процесса
N <- 200
Step <- 1
delta_t  <- Step/N
SO <- 10
a <- as.data.frame(rep(0, N))
names(a) <- "Price"
a$Step <- seq(1, N, 1)
for (i in 2:N) {
  a$Price[i] <- a$Price[i-1] + rnorm(1)*sqrt(a$Step[i])
}
ggplot(a, aes(x = Step, y = Price)) + geom_line(color = 178) + xlab("Number of steps") + ylab("Price") + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20))

#Иллюстрация подразумеваемой волатильности
chain <- getOptionChain("BAC", Exp = "2016-05-20")
chain <- chain$calls
chain <- chain[, 1:2]
chain$iv <- 0
time_remain <- as.numeric(as.Date("2016-05-20") - as.Date(Sys.time()))
time <- time_remain/360
rf <- 0.01
Spot <- getQuote("BAC")
Spot <- Spot$Last
chain <- as.data.frame(apply(chain, 2, as.numeric))
for (i in 1:nrow(chain)) {
  chain$iv[i] <- iv.opt(S = Spot, K = chain$Strike[i], T = time, riskfree = rf, price = chain$Last[i], type = "Call")
}
chain <- na.omit(chain)
ggplot(chain, aes(x = Strike, y = iv)) + geom_line(size = 1, color = "red") + ylab("Implied volatility") + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20))


#Показать сходимость CRR к BSM
steps = 50
CRROptionValue <- rep(NA, times = steps)
for (n in 3:steps) {
  CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "ce", S = 50,
                                            X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
  }
CRROptionValue <- as.data.frame(CRROptionValue)
names(CRROptionValue)[1] <- "CRRmodel"
CRROptionValue <- na.omit(CRROptionValue)
CRROptionValue$steps <- seq(3, steps, 1)
BSM_price <- GBSOption(TypeFlag = "c", S = 50,
          X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4)
CRROptionValue$BSMmodel <- BSM_price@price
data_2 <- gather(data = CRROptionValue, key = "Model", value = "Price", -steps)
ggplot(data_2, aes(y = Price, x = steps, col = Model, linetype = Model)) + geom_line(size = 1)  + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20))

######Сравнение результатов по методу Монте-Карло с разным количеством
######испытаний и BSM моделью
S_0 <- 70
K <- 67
sigma <- 0.2
rf <- 0.05
TTM <- 7

op_BSM <- bs.opm(S = S_0, K = K, T = TTM/360, riskfree = rf, sigma = sigma, type = "Call")

#Stock moves once a day, number of paths = 20
average_1 <- mc(S_0 = S_0, Nsim = 20, step = 1, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")

#Stock moves once a day, number of paths = 100
average_2 <- mc(S_0 = S_0, Nsim = 100, step = 1, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_2

#Stock moves once a day, number of paths = 1000
average_3 <- mc(S_0 = S_0, Nsim = 1000, step = 1, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_3

#Stock moves every hour, number of paths = 20
average_4 <- mc(S_0 = S_0, Nsim = 20, step = 1/24, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_4

#Stock moves every hour, number of paths = 100
average_5 <- mc(S_0 = S_0, Nsim = 100, step = 1/24, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_5

#Stock moves every hour, number of paths = 1000
average_6 <- mc(S_0 = S_0, Nsim = 1000, step = 1/24, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_6

#Stock moves every 4 hours, number of paths = 20
average_7 <- mc(S_0 = S_0, Nsim = 20, step = 1/6, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_7

#Stock moves every 4 hours, number of paths = 100
average_8 <- mc(S_0 = S_0, Nsim = 100, step = 1/6, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_8

#Stock moves every 4 hours, number of paths = 1000
average_9 <- mc(S_0 = S_0, Nsim = 1000, step = 1/6, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_9



#Stock moves every hour, nsim = 50000
average_10 <- mc(S_0 = S_0, Nsim = 5000, step = 1/24, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_10

#Stock moves every 4 hours, nsim = 50000
average_11 <- mc(S_0 = S_0, Nsim = 5000, step = 1/6, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_11

#Stock moves every day, nsim = 50000
average_12 <- mc(S_0 = S_0, Nsim = 5000, step = 1, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_12


oneday <- c(average_1, average_2, average_3, average_12)
fourhours <- c(average_7, average_8, average_9, average_11)
onehour<- c(average_4, average_5, average_6, average_10)
table <- rbind(oneday, fourhours, onehour)
bsprice <- bs.opm(S = S_0, K = K, T = TTM/360, riskfree = rf, sigma = sigma, type = "Call")
table <- as.data.frame(table)
error_1 <- oneday - bsprice
error_2 <- fourhours - bsprice
error_3 <- onehour - bsprice
table <- cbind(oneday, error_1, fourhours, error_2, onehour, error_3)
table
names(table) <- c("20 sim", "100 sim", "1000 sim", "50000 sim", "20 sim error", "100 sim error", "1000 sim error", "50000 sim error")
row.names(table) <- c("20 sim", "100 sim", "1000 sim", "50000 sim")
table
bsprice
table_2 <- xtable(table)
print(table_2, booktabs = T)

#То же самое для случае, когда до конца срока действия опиона 1 месяц

S_0 <- 70
K <- 67
sigma <- 0.2
rf <- 0.05
TTM <- 30

op_BSM <- bs.opm(S = S_0, K = K, T = TTM/360, riskfree = rf, sigma = sigma, type = "Call")

#Stock moves once a day, number of paths = 20
average_1 <- mc(S_0 = S_0, Nsim = 20, step = 1, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")

#Stock moves once a day, number of paths = 100
average_2 <- mc(S_0 = S_0, Nsim = 100, step = 1, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_2

#Stock moves once a day, number of paths = 1000
average_3 <- mc(S_0 = S_0, Nsim = 1000, step = 1, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_3

#Stock moves every hour, number of paths = 20
average_4 <- mc(S_0 = S_0, Nsim = 20, step = 1/24, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_4

#Stock moves every hour, number of paths = 100
average_5 <- mc(S_0 = S_0, Nsim = 100, step = 1/24, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_5

#Stock moves every hour, number of paths = 1000
average_6 <- mc(S_0 = S_0, Nsim = 1000, step = 1/24, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_6

#Stock moves every 4 hours, number of paths = 20
average_7 <- mc(S_0 = S_0, Nsim = 20, step = 1/6, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_7

#Stock moves every 4 hours, number of paths = 100
average_8 <- mc(S_0 = S_0, Nsim = 100, step = 1/6, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_8

#Stock moves every 4 hours, number of paths = 1000
average_9 <- mc(S_0 = S_0, Nsim = 1000, step = 1/6, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_9



#Stock moves every hour, nsim = 50000
average_10 <- mc(S_0 = S_0, Nsim = 5000, step = 1/24, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_10

#Stock moves every 4 hours, nsim = 50000
average_11 <- mc(S_0 = S_0, Nsim = 5000, step = 1/6, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_11

#Stock moves every day, nsim = 50000
average_12 <- mc(S_0 = S_0, Nsim = 5000, step = 1, rf = rf, sigma = sigma, TTM = TTM, K = K, type = "Call")
average_12

oneday <- c(average_1, average_2, average_3, average_12)
fourhours <- c(average_7, average_8, average_9, average_11)
onehour<- c(average_4, average_5, average_6, average_10)
table <- rbind(oneday, fourhours, onehour)
bsprice <- bs.opm(S = S_0, K = K, T = TTM/360, riskfree = rf, sigma = sigma, type = "Call")
table <- as.data.frame(table)
error_1 <- oneday - bsprice
error_2 <- fourhours - bsprice
error_3 <- onehour - bsprice
table <- cbind(oneday, error_1, fourhours, error_2, onehour, error_3)
table
names(table) <- c("20 sim", "100 sim", "1000 sim", "50000 sim", "20 sim error", "100 sim error", "1000 sim error", "50000 sim error")
row.names(table) <- c("20 sim", "100 sim", "1000 sim", "50000 sim")
table
bsprice
table_2 <- xtable(table)
print(table_2, booktabs = T)

#Моделирование стоимости азиатского опциона 
S0 <- 120
K <- 110
TTM <- 10
rf <- 0.1
sigma <- 0.3
a <- mc_asian(S_0 = S0, Nsim = 5000, step = 1, rf = rf, sigma = sigma, TTM = 10, K = K)
a
bs.opm(S = S0, K = K, T = TTM/360, riskfree = rf, sigma = sigma, type = "Call")
b <- xtable(data_3[, 1:9])
print(b, booktabs = T)

#Сравнение фактических и теоретических функций плотности доходностей акции
getSymbols("GOOG", from = "2015-01-01", to = "2016-01-01")
goog <- as.numeric(GOOG[, 6])
goog.ret <- na.omit(Delt(goog))
av <- mean(goog.ret)
s <- sd(goog.ret)
n <- nrow(goog.ret)

goog.ret <- as.data.frame(goog.ret)
names(goog.ret) <- "goog_return"
goog.ret$Norm <- rnorm(n, av, s)
goog.ret_2 <- gather(data = goog.ret, key = Distribution, value = Return)
ggplot(goog.ret_2, aes(x = Return, color = Distribution, linetype = Distribution)) + geom_density(size = 1) + theme(axis.text = element_text(size = 18), panel.border = element_rect(fill = NA, colour = "black", size = 2), axis.title = element_text(size = 20))


#График распределения вероятностей, полученных с помощью обычной симуляции
#и плотности нормального распределения
a <- mc(S_0 = S0, Nsim = 500, step = 1/6, rf = rf, sigma = sigma, TTM = TTM, K = K,type = "Call")
b <- data_3[, 23]
b <- na.omit(Delt(b))
m <- mean(b)
s <- sd(b)
b <- as.data.frame(b)
names(b) <- "SimulatedReturns"
b <- as.data.frame(b)
r1 <- rnorm(1000, m, s)
r1 <- as.data.frame(r1)
names(r1) <- "NormalReturns"
pl <- ggplot(data = b, aes(x = SimulatedReturns, color = "SimulatedReturns"))  + geom_density(data = r1, aes(x = NormalReturns, color = "NormalReturns"), size = 1) + geom_density(size = 1)
pl <- pl + scale_colour_discrete(labels = c("Normal returns", "Simulated returns"))
pl <- pl + ylab("Density") + xlab("Return")
pl


####Задание функции правдоподобия для NIG распределения
ll <- function(mu, alpha, delta, beta) {
  -sum(log(alpha*delta*besselK(x = alpha*sqrt(delta^2 + (x - mu)^2), nu = 1)/(pi*sqrt(delta^2 + (x-mu)^2))*exp(delta*sqrt(alpha^2 - beta^2) + beta*(x - mu))))
}

#Оценка коэффициентов NIG распределения с помощью MLE
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

#Моделирование 1000 переменных на основе оцененных параметров NIG распределения
r1 <- rnig(n = 1000, mu = mu, delta = delta, alpha = alpha, beta = beta) 
#график функции плотности смоделированного распределения
plot(density(r1), main = "Density of Google log returns and NIG returns", lty = 1, lwd = 2) 
#добавить плотность распределения логдоходностей Google
lines(density(x), col = 2, lty = 2, lwd = 2) 
legend("topright", c("Google log returns", "NIG returns"), lty = c(1, 2))
library(statmod)
sigma2 <- rinvgauss(n = 250, mean = delta/gamma, shape = delta^2) #моделирование IG(delta, gamma)
sigma <- sqrt(sigma2)
epsilon <- rnorm(n = 250, mean = 0, sd = 1) #моделирование N(0, 1)
r <- mu + beta*sigma2 +sigma*epsilon #моделирование r_i
mean(r)
plot(density(r), main = "Density function of r_i") #график смоделированных логдоходностей
r.cumsum <- cumsum(r) #расчет кумулятивных доходностей
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
##Создание функции для оценки опциона на основании NIG модели
#Загрузка дневных данных
setwd("D:/R")
goog.1H <- read.csv("GOOG.1H.csv", header = TRUE, sep = ";", dec = ".")
goog.1H <- goog.1H$X.CLOSE.
goog.1H.log <- log(goog.1H)
x <- diff(goog.1H.log)
#Оценка параметров NIG распределения
par <- mle(minuslogl = ll, start = list(mu = 0, alpha = 1, beta = 0, delta = 1))
summary(par)
par <- par@coef
mu <- par[1]
alpha <- par[2]
delta <- par[3]
beta <- par[4]
gamma <- sqrt(alpha^2 - beta^2)

#NIG функция
S0 <- 70
K <- 70
TTM <- 100
Ncol <- 5000
trade_days <- 252
rf <- 0.05
hours <- round(x = length(x)/trade_days, digits = 0)
mc_nig <- function(S0, K, TTM, Ncol = 1000, rf, hours = 7, mu, alpha, delta, beta, type = "Call") {
  gamma <- sqrt(alpha^2 - beta^2)
  T <- 1/(7*252) #7 рабочих часов, 252 рабочих дней
  drift_GBM <- (rf - 0.5*sigma^2)*T 
  drift_NIG <- mu + beta*delta/gamma
  k <- as.numeric(drift_GBM/drift_NIG) #коэффициент корректировки дрейфа
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

##NIG функция для азиатского опциона
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
K <- 60
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
  nig[i] <- mc_nig_asian(S0 = S0, K = K, TTM = TTM, Ncol = 1000, rf = rf, mu = mu, alpha = alpha, delta = delta, beta = beta, hours = 7)
}
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
m1
m2
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
