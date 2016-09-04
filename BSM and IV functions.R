#Функция расчета цены опциона на основе BSM
bs.opm <- function(S, K, T, riskfree, sigma, type) {
  d1 <- (log(S/K) + (riskfree +0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  if (type == "Call") {
    opt.val <- S*pnorm(d1) - K*exp(-riskfree*T)*pnorm(d2)
  }
  if (type=="Put") {
    opt.val <- K*exp(-riskfree*T)*pnorm(-d2) - S*pnorm(-d1)
  }
  opt.val
}

#Функция расчета подразумеваемой волатильности
iv.opt <- function(S, K, T, riskfree, price, type) {
  sigma <- 0.20
  sigma.up <- 1
  sigma.down <- 0.001
  count <- 0
  epsilon <- bs.opm(S, K, T, riskfree, sigma, type) - price
  while (abs(epsilon) > 0.00001 && count <1000) {
    if (epsilon < 0) {
      sigma.down < -sigma
      sigma <- (sigma.up + sigma)/2
    } else {
        sigma.up <- sigma
        sigma <- (sigma.down + sigma)/2
    }
    epsilon <- bs.opm(S, K, T, riskfree, sigma, type) - price
    count <- count+1
  }
  if (count == 1000) {
    return(NA)
  } else {return(sigma)}
}

#Расчет стоимости опциона на с помощью метода Монте-Карло
mc <- function(S_0, Nsim, step = 1, rf, sigma, TTM, K, type = "Call") {
  mu <- rf - 0.5*sigma^2
  dt <- step/360
  Nstep <- TTM/step
  data_1 <- matrix(exp(mu*dt + sigma*sqrt(dt)*rnorm(Nsim*Nstep)), ncol = Nsim)
  data_2 <- apply(data_1, 2, cumprod)
  data_2 <- S_0*data_2
  data_3 <<- rbind(rep(S_0, Nsim), data_2)
  data_4 <- tail(data_3, 1)
  if (type == "Call"){
    data_5 <- mean(pmax(data_4 - K, 0))*exp(-rf*TTM/360)
  }
  else {data_5 <- mean(pmax(K - data_4, 0))*exp(-rf*TTM/360)}
  data_6 <- as.data.frame(data_3)
  data_6$Step <- seq(0, nrow(data_6)-1, 1)
  data_6 <- gather(data = data_6, key = Path, value = Price, -Step)
  plot_paths <<- ggplot(data_6, aes(x = Step, y = Price, color = Path)) + geom_line() + theme(legend.position = "none")
  return(data_5)
}

#Расчет стоимости азиатского опциона на с помощью метода Монте-Карло
mc_asian <- function(S_0, Nsim, step = 1, rf, sigma, TTM, K, type = "Call") {
  mu <- rf - 0.5*sigma^2
  dt <- step/360
  Nstep <- TTM/step
  data_1 <- matrix(exp(mu*dt + sigma*sqrt(dt)*rnorm(Nsim*Nstep)), ncol = Nsim)
  data_2 <- apply(data_1, 2, cumprod)
  data_2 <- S_0*data_2
  data_3 <<- rbind(rep(S_0, Nsim), data_2)
  data_4 <- apply(data_3, 2, mean)
  if (type == "Call"){
    data_5 <- mean(pmax(data_4 - K, 0))*exp(-rf*TTM/360)
  }
  else {data_5 <- mean(pmax(K - data_4, 0))*exp(-rf*TTM/360)}
  data_6 <- as.data.frame(data_3)
  data_6$Step <- seq(0, nrow(data_6)-1, 1)
  data_6 <- gather(data = data_6, key = Path, value = Price, -Step)
  plot_paths <<- ggplot(data_6, aes(x = Step, y = Price, color = Path)) + geom_line() + theme(legend.position = "none")
  return(data_5)
}
