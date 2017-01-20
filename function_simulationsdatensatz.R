simulationsdatensatz<- function() {
dat <- eusilcA_pop
dat <- dat[,c(1:15)]
dat$gender <- as.numeric(dat$gender)-1

lm1 <- lm(eqIncome ~ ., data = dat)

dat$gender <- as.numeric(dat$gender)-1
dat$eqIncome <- NULL
 
cov_mat <- cov(dat)
mu <- colMeans(dat)

n <- 80000
df <- as.data.frame(mvrnorm(n, mu, cov_mat))
df$gender <- df$gender + abs(min(df$gender) )
df$gender <- df$gender/max(df$gender)
df$gender <- rbinom(n, 1, df$gender )

e <- rnorm(n, 0, 10000)
df$eqIncome <- predict(lm1, df) + e
df$eqIncome <- ifelse(df$eqIncome>=0, df$eqIncome,0)

df$e <- rnorm(n)
df$district <-  scale(df$cash) + scale(df$age_ben) + df$e
df$district <- 


df$district <- ((df$district + abs(min(df$district)) ))
df$district <- df$district *10
df$district <- round(df$district)
df$district <- ifelse(df$district<=40, 40, df$district)
df$district <- ifelse(df$district>=120, 120, df$district)
df$district <- as.factor(df$district)
simu <- df
return(simu)
}