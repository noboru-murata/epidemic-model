### 実験4
### 大規模なネットワークで
### 感染モデルのパラメタがどのような影響を与えるか調べる

## 乱数のシード (適宜変えてよい)
set.seed(sum(as.integer(charToRaw("COVID-19"))))

## グラフの特性 (Watts-Strogatz model)
n <- 10000   # 頂点数
d <- 25      # 初期近傍 50=25x2
p <- 0.05    # 張替確率
## 感染モデルの特性
prob <- 0.04 # 感染確率
deltaL <- 3  # 潜伏期間
deltaP <- 3  # 媒介期間
## シミュレーションの設定
Tmax <- 120  # 時間上限

## グラフの作成
myGraph <- sample_smallworld(1,n,d,p) 
myAdjlist <- get.adjlist(myGraph) 

### 潜伏期間の影響
myExp4.1 <- list()
dLvec <- 0:7
for(i in 1:length(dLvec)){
    tmpTrial <- myTrial(myAdjlist,
                        prob=prob,
                        deltaL=dLvec[i],
                        deltaP=deltaP,
                        Tmax=Tmax,
                        initRandom=FALSE)
    myExp4.1[[i]] <- vertexSummary(tmpTrial,dLvec[i],deltaP)
}

## 感染者の推移
par(family="HiraginoSans-W4")
tmp <- sapply(myExp4.1,function(x){x["infected",]})
matplot(tmp/n,
        ylim=c(0,1), type="l",
        col=rainbow(length(dLvec)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染率")
legend("bottomright", inset=0.05,
       legend=dLvec,
       col=rainbow(length(dLvec)),lty=1,lwd=3)
matplot(log10(tmp+0.1),
        type="l", 
        col=rainbow(length(dLvec)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染者数(常用対数)")
legend("bottomright", inset=0.05,
       legend=dLvec,
       col=rainbow(length(dLvec)),lty=1,lwd=3)

### 媒介期間の影響
myExp4.2 <- list()
dPvec <- 1:5
for(i in 1:length(dPvec)){
    tmpTrial <- myTrial(myAdjlist,
                        prob=prob,
                        deltaL=deltaL,
                        deltaP=dPvec[i],
                        Tmax=Tmax,
                        initRandom=FALSE)
    myExp4.2[[i]] <- vertexSummary(tmpTrial,deltaL,dPvec[i])
}

## 感染者の推移
par(family="HiraginoSans-W4")
tmp <- sapply(myExp4.2,function(x){x["infected",]})
matplot(tmp/n,
        ylim=c(0,1), type="l",
        col=rainbow(length(dPvec)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染率")
legend("bottomright", inset=0.05,
       legend=dPvec,
       col=rainbow(length(dPvec)),lty=1,lwd=3)
matplot(log10(tmp+0.1),
        type="l", 
        col=rainbow(length(dPvec)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染者数(常用対数)")
legend("bottomright", inset=0.05,
       legend=dPvec,
       col=rainbow(length(dPvec)),lty=1,lwd=3)

### 感染確率の影響
myExp4.3 <- list()
probvec <- (1:6)/100
for(i in 1:length(probvec)){
    tmpTrial <- myTrial(myAdjlist,
                        prob=probvec[i],
                        deltaL=deltaL,
                        deltaP=deltaP,
                        Tmax=Tmax,
                        initRandom=FALSE)
    myExp4.3[[i]] <- vertexSummary(tmpTrial,deltaL,deltaP)
}

## 感染者の推移
par(family="HiraginoSans-W4")
tmp <- sapply(myExp4.3,function(x){x["infected",]})
matplot(tmp/n,
        ylim=c(0,1), type="l",
        col=rainbow(length(probvec)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染率")
legend("topright", inset=c(0.05,0.1),
       legend=probvec,
       col=rainbow(length(probvec)),lty=1,lwd=3)
matplot(log10(tmp+0.1),
        type="l", 
        col=rainbow(length(probvec)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染者数(常用対数)")
legend("topright", inset=c(0.05,0.1),
       legend=probvec,
       col=rainbow(length(probvec)),lty=1,lwd=3)

### 感染確率の影響(詳細)
myExp4.4 <- list()
probvec <- 5/1000+(0:10)/1000
for(i in 1:length(probvec)){
    tmpTrial <- myTrial(myAdjlist,
                        prob=probvec[i],
                        deltaL=deltaL,
                        deltaP=deltaP,
                        Tmax=3*Tmax,
                        initRandom=FALSE)
    myExp4.4[[i]] <- vertexSummary(tmpTrial,deltaL,deltaP)
}

## 感染者の推移
par(family="HiraginoSans-W4")
tmp <- sapply(myExp4.4,function(x){x["infected",]})
matplot(tmp/n,
        ylim=c(0,1), type="l",
        col=rainbow(length(probvec)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染率")
legend("bottomright", inset=c(0.05,0.1),
       legend=probvec,
       col=rainbow(length(probvec)),lty=1,lwd=3)
## matplot(log10(tmp+0.1),
##         type="l", 
##         col=rainbow(length(probvec)), lty=1, lwd=3, 
##         xlab="時間経過",ylab="感染者数(常用対数)")
## legend("bottomright", inset=0.05,
##        legend=probvec,
##        col=rainbow(length(probvec)),lty=1,lwd=3)
plot(probvec,tmp[nrow(tmp),],
     ylim=c(0,n), type="b",
     col="blue", lwd=3,
     xlab="感染確率", ylab="感染者数")
