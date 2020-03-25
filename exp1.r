### 実験1
### 感染拡大の確率シミュレーションを行い
### 結果を視覚化する

## 乱数のシード (適宜変えてよい)
set.seed(sum(as.integer(charToRaw("COVID-19"))))

## 張替確率によるグラフの違い
orgpar <- par(mar=rep(0,4))
plist <- c(0,0.05,1)
for(i in 1:length(plist)){
    plot(sample_smallworld(1,30,3,plist[i]),
         vertex.color="lightgreen",
         vertex.size=8,
         vertex.label.cex=.6)
}
par(orgpar)

## グラフの特性 (Watts-Strogatz model)
n <- 100
d <- 3 # 初期グラフの近傍=2*d (以下では1次元を使う)
p <- 0.05 # 辺の張替え確率
## 感染モデルの特性
prob <- 0.5 # 感染確率
deltaL <- 3 # 潜伏期間
deltaP <- 3 # 発症期間 
## シミュレーションの設定
Tmax <- 30 # 時間上限

## グラフの作成
myGraph <- sample_smallworld(1,n,d,p) # グラフ
myAdjlist <- get.adjlist(myGraph) # 隣接エッジ
## 別のグラフの作成法 (簡単なランダムの場合)
## m <- d*n
## myGraph <- sample_gnm(n,m) 

## 感染シミュレーション
myExp1 <- myTrial(myAdjlist,
                  prob=0.5,
                  deltaL=deltaL,
                  deltaP=deltaP,
                  Tmax=Tmax,
                  initRandom=FALSE)

## 感染拡大の推移(グラフの視覚化)
orgpar <- par(mar=rep(0,4))
xyLayout <- layout_nicely(myGraph) # 配置を固定
for(t in 1:Tmax){
    plot(myGraph,
         layout=xyLayout,
         vertex.color=statCol(myExp1[[t]],
                              deltaL=deltaL,deltaP=deltaP),
         vertex.size=8,
         vertex.label.cex=.6)
}
par(orgpar)

## 感染者の推移
par(family="HiraginoSans-W4")
plot(vertexSummary(myExp1,deltaL,deltaP)["infected",]/n,
     ylim=c(0,1), type="l", col="red", lwd=3, 
     xlab="時間経過",ylab="感染率")
## その他の情報を見る例
## lines(vertexSummary(myExp1)['removed',]/n,col="grey")
## lines(vertexSummary(myExp1)['susceptible',]/n,col="blue")
## plot(vertexSummary(myExp1,deltaL,deltaP)["infected",],
##      type="l", col="red", lwd=3, log="y",
##      xlab="time",ylab="infected")
