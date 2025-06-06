EPI_data <- read.csv("C:/D_Drive/Arya/NTU/exchange/study/DA stuff/epi2024results.csv")

attach(EPI_data)
EPI.new #prints out values
NAs <- is.na(EPI.new) #records true values is the value is NA
EPI.new.noNAs <- EPI.new[!NAs]

#exercise 1:
summary(EPI.new) #stats
fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new)
hist(EPI.new)
hist(EPI.new, seq(20, 80, 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw=1))
rug(EPI.new)

boxplot(EPI.new, APO.new)
View(APO.new)
hist(EPI.new, seq(20, 80, 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw=1))
rug(EPI.new)

#below is exact same as above except bw="SJ"
hist(EPI.new, seq(20, 80, 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw="SJ"))
rug(EPI.new)

x <- seq(20,80,1)
q<- dnorm(x, mean=42, sd=5, log=FALSE)
lines(x, q)
lines(x,0.4*q)
q<- dnorm(x, mean=65, sd=5, log=FALSE)
lines(x, 0.12*q)


#exercise 2:
#CDF
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)

#QQ plot
qqnorm(EPI.new); qqline(EPI.new)

#make a QQ plot against the generating distribution by
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)

qqplot(rt(250, df=5), EPI.new, xlab="Q-Q plot for t dsn")
qqline(EPI.new)


#exercise 2a - fitting a distribution on 2 primary variables, TKP.new and PAE.new

#exploring TKP.new
summary(TKP.new) #stats
stem(TKP.new)
hist(TKP.new, seq(0, 100, 1.0), prob=TRUE)
lines(density(TKP.new, na.rm=TRUE, bw=1))
rug(TKP.new)
boxplot(TKP.new)

#fitting TKP.new
#CDF
plot(ecdf(TKP.new), do.points=FALSE, verticals=TRUE, main="CDF plot of TKP.new")

#QQ plot
qqnorm(TKP.new); qqline(TKP.new)

#make a QQ plot against the generating distribution by
qqplot(rnorm(250), TKP.new, xlab = "Q-Q plot for norm dsn")
qqline(TKP.new)

qqplot(rt(250, df=5), TKP.new, xlab="Q-Q plot for t dsn")
qqline(TKP.new)


#exploring PAE.new
summary(PAE.new) #stats
stem(PAE.new)
hist(PAE.new, seq(0, 100, 1.0), prob=TRUE)
lines(density(PAE.new, na.rm=TRUE, bw=1))
rug(PAE.new)
boxplot(PAE.new)

#fitting PAE.new
#CDF
plot(ecdf(PAE.new), do.points=FALSE, verticals=TRUE, main="CDF plot of PAE.new")

#QQ plot
qqnorm(PAE.new); qqline(PAE.new)

#make a QQ plot against the generating distribution by
qqplot(rnorm(250), PAE.new, xlab = "Q-Q plot for norm dsn")
qqline(PAE.new)

qqplot(rt(250, df=5), PAE.new, xlab="Q-Q plot for t dsn")
qqline(PAE.new)




















