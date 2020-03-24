### 実験3
### 大規模なネットワークで
### グラフ構造のパラメタがどのような影響を与えるか調べる

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

### 近傍数の影響
myExp3.1 <- list()
dlist <- seq(5,40,by=5)
for(i in 1:length(dlist)){
    tmpGraph <- sample_smallworld(1,n,dlist[i],p) 
    ## シミュレーションごとにグラフを作成
    tmpAdjlist <- get.adjlist(tmpGraph) 
    tmpTrial <- myTrial(tmpAdjlist,
                        prob=prob,
                        deltaL=deltaL,
                        deltaP=deltaP,
                        Tmax=Tmax,
                        initRandom=FALSE)
    myExp3.1[[i]] <- vertexSummary(tmpTrial,deltaL,deltaP)
}

## 感染者の推移
par(family="HiraginoSans-W4")
tmp <- sapply(myExp3.1,function(x){x["infected",]})
matplot(tmp/n,
        ylim=c(0,1), type="l",
        col=rainbow(length(dlist)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染率")
legend("bottomright", inset=c(0.05,0.2),
       legend=dlist*2,
       col=rainbow(length(dlist)),lty=1,lwd=3)
matplot(log10(tmp+0.1),
        type="l", 
        col=rainbow(length(dlist)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染者数(常用対数)")
legend("bottomright", inset=c(0.05,0.2),
       legend=dlist*2,
       col=rainbow(length(dlist)),lty=1,lwd=3)

### 張替確率の影響
myExp3.2 <- list()
plist <- c(0,0.001,seq(0.01,0.05,by=0.02),
           seq(0.1,0.5,by=0.2),0.75,1)
for(i in 1:length(dlist)){
    tmpGraph <- sample_smallworld(1,n,d,plist[i]) 
    ## シミュレーションごとにグラフを作成
    tmpAdjlist <- get.adjlist(tmpGraph) 
    tmpTrial <- myTrial(tmpAdjlist,
                        prob=prob,
                        deltaL=deltaL,
                        deltaP=deltaP,
                        Tmax=Tmax,
                        initRandom=FALSE)
    myExp3.2[[i]] <- vertexSummary(tmpTrial,deltaL,deltaP)
}

## 感染者の推移
par(family="HiraginoSans-W4")
tmp <- sapply(myExp3.2,function(x){x["infected",]})
matplot(tmp/n,
        ylim=c(0,1), type="l",
        col=rainbow(length(plist)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染率")
legend("bottomright", inset=c(0.05,0.2),
       legend=plist,
       col=rainbow(length(plist)),lty=1,lwd=3)
matplot(log10(tmp+0.1),
        type="l", 
        col=rainbow(length(plist)), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染者数(常用対数)")
legend("bottomright", inset=c(0.05,0.2),
       legend=plist,
       col=rainbow(length(plist)),lty=1,lwd=3)

