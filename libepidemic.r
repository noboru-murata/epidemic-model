### 感染症のシミュレーション用
## statCol: 頂点の色を塗り分ける
## smapleInfect: 感染確率に従い感染者を選択する
## myTrial: 感染シミュレーション
## vertexSummary: 頂点の状態推移を集計

library(igraph)
statCol <- function(v,deltaL=10,deltaP=3){
    sapply(v,function(x){if(x>deltaL+deltaP) "grey"
                         else if(x>deltaL) "red"
                         else if(x>0) "yellow"
                         else "white"})
}

sampleInfect <- function(adjacent,susceptible,prob){
    tmp <- base::intersect(adjacent,susceptible) # 感染候補者
    return(tmp[as.logical(rbinom(length(tmp),1,prob))])
}

myTrial <- function(adjacencyList, # 隣接リスト
                    prob=0.01, # 感染確率
                    deltaL=10, # 潜伏期間
                    deltaP=3, # 発症期間
                    Tmax=60, # 模擬実験期間
                    initRandom=FALSE # 初期感染者をランダム
                    ){
    n <- length(adjacencyList)
    vertexState <- integer(n)
    if(initRandom){
        vertexState[sample(n,1)] <- 1 # 最初の感染者
    }else{
        vertexState[1] <- 1 # 最初の感染者
    }
    vertexStateList <- list()
    for(t in 1:Tmax){
        vertexStateList[[t]] <- vertexState
        susceptible <- which(vertexState==0) # 未感染
        latent <- which(vertexState>0 & vertexState<=deltaL) # 潜伏
        present <- which(vertexState>deltaL & vertexState<=deltaL+deltaP) # 発症
        removed <- which(vertexState>deltaL+deltaP) # 隔離
        infected <- which(vertexState>0) # 感染
        tmp <- lapply(adjacencyList[present], # 感染元
                      function(x){sampleInfect(x,susceptible,prob)}) # 確率的
        newInfected <- unique(sort(unlist(tmp)))
        vertexState[infected] <- vertexState[infected] + 1
        vertexState[newInfected] <- 1
    }
    return(vertexStateList)
}

vertexSummary <- function(vertexStateList,
                          deltaL=10,
                          deltaP=3){
    sapply(vertexStateList,
           function(x)
               c(infected=sum(x>0), # 非-未感染
                 susceptible=sum(x==0), # 未感染
                 latent=sum(x>0 & x<=deltaL), # 潜伏
                 present=sum(x>deltaL & x<=deltaL+deltaP), # 発症
                 removed=sum(x>deltaL+deltaP) # 隔離
                 )
           )
}
