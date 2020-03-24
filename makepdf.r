### 文書用PDFの作成
### 各実験を1つのファイルにまとめる

## 定義した関数の読み込み
source("libepidemic.r")

### 実験1
quartz(file="figs/exp1.pdf",type="pdf")
source("exp1.r")
dev.off()
save(myExp1,file="figs/exp1.RData")

### 実験2
quartz(file="figs/exp2.pdf",type="pdf")
source("exp2.r")
dev.off()
save(myExp2.1,myExp2.2,myExp2.3,file="figs/exp2.RData")

### 実験3
quartz(file="figs/exp3.pdf",type="pdf")
source("exp3.r")
dev.off()
save(myExp3.1,myExp3.2,file="figs/exp3.RData")

### 実験4
quartz(file="figs/exp4.pdf",type="pdf")
source("exp4.r")
dev.off()
save(myExp4.1,myExp4.2,myExp4.3,myExp4.4,file="figs/exp4.RData")

### 実験5
quartz(file="figs/exp5.pdf",type="pdf")
source("exp5.r")
dev.off()
save(myExp5.1,myExp5.2,myExp5.3,file="figs/exp5.RData")

### 実験6
quartz(file="figs/exp6.pdf",type="pdf")
source("exp6.r")
dev.off()
