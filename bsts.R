
#-----------------例題1-------------------

library(bsts)

data(goog)

train <- as.vector(goog)[1:1372]
test <- as.vector(goog)[1372:1428]
ss <- AddLocalLevel(list(), train)
ss <- AddSeasonal(ss, train, nseasons = 7)
model <- bsts(train, ss, niter=1000)
# 略
pred <- predict(model, horizon=56, burn=100)
#, xlim=c(1,1428)
plot(pred, ylim=c(250,800))
par(new=T)
plot(test, ylim=c(250,800), xlim=c(-1372,56), type='l', lwd=3, col='red', axes=F)
legend('topleft', legend=c('Predicted','Actual'), col=c('blue','red'), lty=1, lwd=3, cex=1.5)

#-----------------例題2-------------------
d <- read.csv('https://raw.githubusercontent.com/ozt-ca/tjo.hatenablog.samples/master/r_samples/public_lib/jp/sample_dlm_season_trend.csv', sep='\t')
d_train <- d[1:80,]
d_test <- d[81:100,]


library(bsts)
ss <- AddLocalLinearTrend(list(),d_train$cv)
ss <- AddSeasonal(ss, d_train$cv, nseasons = 7)
model <- bsts(cv~., state.specification = ss, niter=1000, data=d_train)
# 略
pred <- predict(model, burn=100, newdata=d_test[,-1])
#, ylim=c(0, 1500), xlim=c(0,100)
plot(pred,ylim=c(0, 1500), xlab='', ylab='')
par(new=T)
plot(d_test$cv, ylim=c(0,1500), xlim=c(-80,20), axes=F, type='l', col='red', lwd=3, lty=1, xlab='', ylab='')
legend('topleft', legend=c('Predicted','Actual'), col=c('blue','red'), lty=1, lwd=3, cex=1.5)