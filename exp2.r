### 実験2
### 同じ構造の大規模なネットワークで
### 確率的な挙動がどのくらいばらつくかを調べる

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
Tmax <- 60  # 時間上限
mc <- 10     # シミュレーション回数

### 確率的な感染者の選択によるばらつきを調べる
### (同じグラフ，同じ初期値)
## グラフの作成
myGraph <- sample_smallworld(1,n,d,p) 
myAdjlist <- get.adjlist(myGraph) 
## シミュレーション
myExp2.1 <- replicate(mc,{
    tmpTrial <- myTrial(myAdjlist,
                        prob=prob,
                        deltaL=deltaL,
                        deltaP=deltaP,
                        Tmax=Tmax,
                        initRandom=FALSE)
    vertexSummary(tmpTrial,deltaL,deltaP)
})

## 感染者の推移
par(family="HiraginoSans-W4")
matplot(myExp2.1["infected",,]/n,
        ylim=c(0,1), type="l",
        col=rainbow(mc), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染率")
matplot(log10(myExp2.1["infected",,]+0.1),
        type="l", 
        col=rainbow(mc), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染者数(常用対数)")

### 初期値の違いによるばらつきを調べる
### (同じグラフ，異なる初期値)
myExp2.2 <- replicate(mc,{
    tmpTrial <- myTrial(myAdjlist,
                        prob=prob,
                        deltaL=deltaL,
                        deltaP=deltaP,
                        Tmax=Tmax,
                        initRandom=TRUE)
    vertexSummary(tmpTrial,deltaL,deltaP)
})

## 感染者の推移
par(family="HiraginoSans-W4")
matplot(myExp2.2["infected",,]/n,
        ylim=c(0,1), type="l",
        col=rainbow(mc), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染率")
matplot(log10(myExp2.2["infected",,]+0.1),
        type="l", 
        col=rainbow(mc), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染者数(常用対数)")

### グラフに違いによるばらつきを調べる
### (異なるグラフ)
myExp2.3 <- replicate(mc,{
    tmpGraph <- sample_smallworld(1,n,d,p) 
    ## シミュレーションごとにグラフを作成
    tmpAdjlist <- get.adjlist(tmpGraph) 
    tmpTrial <- myTrial(tmpAdjlist,
                        prob=prob,
                        deltaL=deltaL,
                        deltaP=deltaP,
                        Tmax=Tmax)
    vertexSummary(tmpTrial,deltaL,deltaP)
})

## 感染者の推移
par(family="HiraginoSans-W4")
matplot(myExp2.3["infected",,]/n,
        ylim=c(0,1), type="l",
        col=rainbow(mc), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染率")
matplot(log10(myExp2.3["infected",,]+0.1),
        type="l", 
        col=rainbow(mc), lty=1, lwd=3, 
        xlab="時間経過",ylab="感染者数(常用対数)")

