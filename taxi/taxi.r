library("rpart")
sngrwy_out=read.csv(file='taxiout.csv')

sngrwy_out$DURATION=as.factor(sngrwy_out$DURATION)
d0=sngrwy_out[sngrwy_out$DURATION=="0",]
d1=sngrwy_out[sngrwy_out$DURATION=="1",]
d2=sngrwy_out[sngrwy_out$DURATION=="2",]

fit_taxi_d0=rpart(TAXI.OUT~HOUR+TYPE,data=d0)
fit_taxi_d1=rpart(TAXI.OUT~HOUR,data=d1)
fit_taxi_d2=rpart(TAXI.OUT~HOUR,data=d2)

print(fit_taxi_d0)
print(fit_taxi_d1)
print(fit_taxi_d1)
plot(fit_taxi_d0)
text(fit_taxi_d0, use.n=TRUE)

ptaxiout=predict(fit_taxi_d0,newdata=sngrwy_out)
chisq.test(sngrwy_out$TAXI.OUT,ptaxiout)
dev=ptaxiout-sngrwy_out$TAXI.OUT
hist(dev)