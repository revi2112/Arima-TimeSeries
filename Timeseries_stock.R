install.packages("MLmetrics")
library('MLmetrics')
df<- read.csv("nifty_it_index.csv")
head(df)
str(df)
sum(is.na(df))
row<-nrow(df)
df['t']<-1:row
df['t2']<-'^'(df$t,2)
x<-data.frame(outer(rep(month.abb,length=row),(month.abb),'=='))
colnames(x) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
df<-cbind(df,x)
head(df) #months

set.seed(144)
spec = c(train = .8, test = .1, validate = .1)
g = sample(cut( seq_along(unique(df$Date)),
                length(unique(df$Date))*cumsum(c(0,spec)),
                labels = names(spec)))
res = split(df, g[as.factor(df$Date)])
head(res)
str(res)

#y=b0+b1*t
m1<-lm(Turnover~t,data=res$train)
m1
m1_pred<-data.frame(predict(m1,res$test))
m1_pred
RMSE(res$test$Turnover,m1_pred$predict.m1..res.test.)
resid1<-residuals(m1)
predict(arima(resid1/1000,order=c(1,0,0)),n.ahead=12)

#log(y)=b0+b1*t
m2<-lm(log(Turnover)~t,data=res$train)
m2
m2_pred<-data.frame(predict(m2,res$test))
m2_pred
RMSE(log(res$test$Turnover),m2_pred$predict.m2..res.test.)
resid2<-residuals(m2)
predict(arima(resid2/1000,order=c(1,0,0)),n.ahead=12)


#y=b0+b1*t+b2*t^2
m3<-lm(Turnover~t+t2,data=res$train)
m3
m3_pred<-data.frame(predict(m3,res$test))
m3_pred
RMSE(res$test$Turnover,m3_pred$predict.m3..res.test.)
resid3<-residuals(m3)
predict(arima(resid3/1000,order=c(1,0,0)),n.ahead=12)


#y=b0+b1*Jan+b2*Feb+...+b11*Nov
m4<-lm(Turnover~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=res$train)
m4
m4_pred<-data.frame(predict(m4,res$test))
m4_pred
RMSE(res$test$Turnover,m4_pred$predict.m4..res.test.)
resid4<-residuals(m4)
predict(arima(resid4/1000,order=c(1,0,0)),n.ahead=12)


#y=b0+b1*t+b2*t^2+b3*Jan+...+b13*Nov
m5<-lm(Turnover~t+t2+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=res$train)
m5
m5_pred<-data.frame(predict(m5,res$test))
m5_pred
RMSE(res$test$Turnover,m5_pred$predict.m5..res.test.)
resid5<-residuals(m5)
predict(arima(resid5/1000,order=c(1,0,0)),n.ahead=12)


#log(y)=b0+b1*Jan+b2*Feb+...+b11*Nov
m6<-lm(log(Turnover)~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=res$train)
m6
m6_pred<-data.frame(predict(m6,res$test))
m6_pred
RMSE(log(res$test$Turnover),m6_pred$predict.m6..res.test.)
resid6<-residuals(m6)
predict(arima(resid6/1000,order=c(1,0,0)),n.ahead=12)

