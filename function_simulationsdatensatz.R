simulationsdatensatz<- function() {
dat <- eusilcA_pop
dat <- dat[,c(1:15)]
dat$gender <- as.numeric(dat$gender)-1

#basierend auf dem eusilc wird einkommen geschätzt
lm1 <- lm(eqIncome ~ ., data = dat)

cov_mat <- cov(dat)
mu <- colMeans(dat)

#pop groesse festlegen 
n_simu <- 80000

df <- as.data.frame(mvrnorm(n_simu, mu, cov_mat))
df$gender <- df$gender + abs(min(df$gender) )
df$gender <- df$gender/max(df$gender)
#gender wird wieder in dummy umgewandelt
df$gender <- rbinom(n_simu, 1, df$gender )

e <- rnorm(n_simu, 0, 10000)
#einkommen wird geschätzt, basierend auf obiger regression + fehler
df$eqIncome <- predict(lm1, df) + e
df$eqIncome <- ifelse(df$eqIncome>=0, df$eqIncome,0)

df$e <- rnorm(n_simu)

#die districte hängen vom cash ab(mit einkommen korreliert)
df$district <-  scale(df$cash) + scale(df$age_ben) + df$e

df$district <- ((df$district + abs(min(df$district)) ))
df$district <- df$district *10
df$district <- round(df$district)
df$district <- ifelse(df$district<=40, 40, df$district)
df$district <- ifelse(df$district>=120, 120, df$district)
df$district <- as.factor(df$district)
simu <- df
return(simu)
}