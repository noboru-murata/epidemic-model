### 実験5
### 格子グラフ上での実験によって
### 感染確率の影響を確認する

## 乱数のシード 
set.seed(sum(as.integer(charToRaw("COVID-19"))))

## グラフの作成
n <- 20 # 格子の大きさ
myGraph <- make_lattice(length=n,dim=2) # 2次元格子
myAdjlist <- get.adjlist(myGraph) # 隣接エッジ
## 感染モデルの特性
prob <- 0.6 # 感染確率
deltaL <- 0 # 潜伏期間
deltaP <- 1 # 発症期間 
## シミュレーションの設定
Tmax <- 60  # 時間上限

## 感染シミュレーション
myExp5.1 <- myTrial(myAdjlist,
                    prob=prob,
                    deltaL=deltaL,
                    deltaP=deltaP,
                    Tmax=Tmax,
                    initRandom=TRUE)

## 感染拡大の推移(グラフの視覚化)
orgpar <- par(mar=rep(0,4))
xyLayout <- layout_on_grid(myGraph) # 格子状に配置
for(t in 1:Tmax){
    plot(myGraph,
         layout=xyLayout,
         vertex.color=statCol(myExp5.1[[t]],
                              deltaL=deltaL,deltaP=deltaP),
         vertex.size=8,
         vertex.label.cex=.6)
}
par(orgpar)

## 感染者の推移
par(family="HiraginoSans-W4")
plot(vertexSummary(myExp5.1,deltaL,deltaP)["infected",]/(n*n),
     ylim=c(0,1), type="l", col="red", lwd=3, 
     xlab="時間経過",ylab="感染率")
## plot(vertexSummary(myExp5.1,deltaL,deltaP)["infected",],
##      type="l", col="red", lwd=3, log="y",
##      xlab="時間経過",ylab="感染率")
## plot(sqrt(vertexSummary(myExp5.1,deltaL,deltaP)["infected",]),
##      type="l", col="red", lwd=3,
##      xlab="時間経過",ylab="感染率")

### 感染確率の影響
myExp5.2 <- list()
probvec <- seq(0,1,by=0.05)
mc <- 100
for(i in 1:length(probvec)){
    myExp5.2[[i]] <- replicate(mc,{
        tmpTrial <- myTrial(myAdjlist,
                            prob=probvec[i],
                            deltaL=deltaL,
                            deltaP=deltaP,
                            Tmax=Tmax*2,
                            initRandom=TRUE)
        vertexSummary(tmpTrial,deltaL,deltaP)
    })
}

par(family="HiraginoSans-W4")
boxplot(sapply(myExp5.2,
               function(x){x["infected",dim(x)[2],]/(n*n)}),
        ylim=c(0,1), axes=FALSE,
        col="royalblue",
        xlab="感染確率",ylab="感染率")
box();axis(2)
axis(1,at=1:length(probvec),labels=probvec)
## lines(colMeans(myData),col="red")

## 感染シミュレーション
myExp5.3 <- list()
probtest <- c(0.3,0.5,0.7)
for(i in 1:length(probtest)){
    myExp5.3[[i]] <- myTrial(myAdjlist,
                             prob=probtest[i],
                             deltaL=deltaL,
                             deltaP=deltaP,
                             Tmax=Tmax*2,
                             initRandom=TRUE)
}

## 終息状態の比較
orgpar <- par(mar=rep(0,4))
xyLayout <- layout_on_grid(myGraph) # 格子状に配置
for(i in 1:length(probtest)){
    plot(myGraph,
         layout=xyLayout,
         vertex.color=statCol(myExp5.3[[i]][[Tmax*2]],
                              deltaL=deltaL,deltaP=deltaP),
         vertex.size=8,
         vertex.label.cex=.6)
}
par(orgpar)

