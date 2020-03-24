### 実験6
### 格子モデルで相転移を(近似的に)説明する

## 条件のグラフを表示
par(family="HiraginoSans-W4")
plot(c(0,1),c(0,1),type="n",xlab="Q",ylab="R")
x <- seq(0,1,length=101)
p <- 0.25 
lines(x,(p*x+(1-p))^4,col="red",lwd=3)
lines(x,x,col="blue",lwd=3)
for(p in seq(0.1,0.5,by=0.1))
    lines(x,(p*x+(1-p))^4,col="orange",lwd=3)
text(0.1,0.75,labels="Sが小さい場合")
text(0.2,0.05,labels="Sが大きい場合")
title("Qの満たす条件")

## 解を計算する
pvec <- seq(0,1,by=0.01)
f <- function(x,y){x-(y*x+(1-y))^4}
qvec <- c(rep(1,sum(pvec<=0.25)), # 0.25で相転移
          sapply(pvec[pvec>0.25], # こちらは数値的に求める
                 function(p){uniroot(f,c(0,0.99),
                                     tol=0.0001,y=p)$root}))
par(family="HiraginoSans-W4")
plot(pvec, 1-qvec, type="l",
     col="blue", lwd=3,
     xlab="感染確率(P)",ylab="全感染率(1-Q)")
