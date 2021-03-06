# テストデータのインポート
test <- read.csv("data/test.csv")

# フィットデータのインポート
fit <- read.csv("data/fit.csv") # フィットデータ（処理なし）
fit.me <- read.csv("data/fit_me.csv") # 平均値挿入法
fit.n1 <- read.csv("data/fit_n1.csv") # 無欠損(P削除)
fit.n2 <- read.csv("data/fit_n2.csv") # 無欠損(S削除)
fit.n3 <- read.csv("data/fit_n3.csv") # 無欠損(P,S削除)
fit.kn <- read.csv("data/fit_kn.csv") # k-nn法
fit.cf <- read.csv("data/fit_cf.csv") # CF応用法

#----------#----------#----------#----------#----------#----------#----------#
# 補完・分析
library(randomForest)

#-----平均値挿入法-----
rF.me <- randomForest(SWE~.,fit.me) # randomForest実行
rF.me.pred <- predict(rF.me,test) # テストデータ適用

rF.me.pr.tmp <- rF.me.pred # 予測値
rF.me.ac.tmp <- test$SWE # 実測値

#-----無欠損データ作成法-----
rF.n1 <- randomForest(SWE~.,fit.n1)
rF.n2 <- randomForest(SWE~.,fit.n2)
rF.n3 <- randomForest(SWE~.,fit.n3)
rF.n1.pred <- predict(rF.n1,test)
rF.n2.pred <- predict(rF.n2,test)
rF.n3.pred <- predict(rF.n3,test)

rF.n1.pr.tmp <- rF.n1.pred
rF.n1.ac.tmp <- test$SWE
rF.n2.pr.tmp <- rF.n2.pred
rF.n2.ac.tmp <- test$SWE
rF.n3.pr.tmp <- rF.n3.pred
rF.n3.ac.tmp <- test$SWE

#-----k-nn法-----
rF.kn <- randomForest(SWE~.,fit.kn)
rF.kn.pred <- predict(rF.kn,test)

rF.kn.pr.tmp <- rF.kn.pred
rF.kn.ac.tmp <- test$SWE

#-----CF応用法-----
rF.cf <- randomForest(SWE~.,fit.cf)
rF.cf.pred <- predict(rF.cf,test)

rF.cf.pr.tmp <- rF.cf.pred
rF.cf.ac.tmp <- test$SWE

#-----missForest-----
library(missForest)

for(i in 1:10){ # ランダム性を含むので10回繰り返した．
  set.seed(i)
  fit.mF <- missForest(fit)$ximp
  
  rF.mF <- randomForest(SWE~.,fit.mF)
  rF.mF.pred <- predict(rF.mF,test)
  
  if(i==1){
    rF.mF.pr.tmp <- rF.mF.pred
    rF.mF.ac.tmp <- test$SWE
  }
  else{
    rF.mF.pr.tmp <- c(rF.mF.pred,rF.mF.pr.tmp)
    rF.mF.ac.tmp <- c(rF.mF.ac.tmp,test$SWE)
  }
}

#-----多重代入法-----
library(mice)

dupe <- 10 # 複数個数

for(i in 1:10){ # ランダム性を含むので10回繰り返した．
  fit.m <- mice(fit,seed = i,m = dupe)
  fit.m.rep <- complete(fit.m,action = "repeated")
  
  fit.mi <- fit
  for(j in 0:(ncol(fit)-1)){
    fit.mi[,j+1] <- apply(fit.m.rep[,(j*dupe+1):((j+1)*dupe)],1,median)
  }
  
  rF.mi <- randomForest(SWE~.,fit.mi)
  rF.mi.pred <- predict(rF.mi,test)
  
  if(i==1){
    rF.mi.pr.tmp <- rF.mi.pred
    rF.mi.ac.tmp <- test$SWE
  }
  else{
    rF.mi.pr.tmp <- c(rF.mi.pred,rF.mi.pr.tmp)
    rF.mi.ac.tmp <- c(rF.mi.ac.tmp,test$SWE)
  }
}

#----------#----------#----------#----------#----------#----------#----------#
# 各手法の評価

#-----平均値挿入法-----
# MAE
MAE.me.raw <- abs(rF.me.pr.tmp - rF.me.ac.tmp)
(MAE.me <- median(MAE.me.raw))
# MRE
MRE.me.raw <- MAE.me.raw/rF.me.ac.tmp
(MRE.me <- median(MRE.me.raw))
# MER
MER.me.raw <- MAE.me.raw/rF.me.pr.tmp
(MER.me <- median(MER.me.raw))
# Pred(25)
(Pred.me.MRE <- sum((MRE.me.raw <= 0.25)/length(rF.me.ac.tmp)))
(Pred.me.MER <- sum((MER.me.raw <= 0.25)/length(rF.me.ac.tmp)))

#-----無欠損データ作成法(P削除)-----
# MAE
MAE.n1.raw <- abs(rF.n1.pr.tmp - rF.n1.ac.tmp)
(MAE.n1 <- median(MAE.n1.raw))
# MRE
MRE.n1.raw <- MAE.n1.raw/rF.n1.ac.tmp
(MRE.n1 <- median(MRE.n1.raw))
# MER
MER.n1.raw <- MAE.n1.raw/rF.n1.pr.tmp
(MER.n1 <- median(MER.n1.raw))
# Pred(25)
(Pred.n1.MRE <- sum((MRE.n1.raw <= 0.25)/length(rF.n1.ac.tmp)))
(Pred.n1.MER <- sum((MER.n1.raw <= 0.25)/length(rF.n1.ac.tmp)))

#-----無欠損データ作成法(S削除)-----
# MAE
MAE.n2.raw <- abs(rF.n2.pr.tmp - rF.n2.ac.tmp)
(MAE.n2 <- median(MAE.n2.raw))
# MRE
MRE.n2.raw <- MAE.n2.raw/rF.n2.ac.tmp
(MRE.n2 <- median(MRE.n2.raw))
# MER
MER.n2.raw <- MAE.n2.raw/rF.n2.pr.tmp
(MER.n2 <- median(MER.n2.raw))
# Pred(25)
(Pred.n2.MRE <- sum((MRE.n2.raw <= 0.25)/length(rF.n2.ac.tmp)))
(Pred.n2.MER <- sum((MER.n2.raw <= 0.25)/length(rF.n2.ac.tmp)))

#-----無欠損データ作成法(P,S削除)-----
# MAE
MAE.n3.raw <- abs(rF.n3.pr.tmp - rF.n3.ac.tmp)
(MAE.n3 <- median(MAE.n3.raw))
# MRE
MRE.n3.raw <- MAE.n3.raw/rF.n3.ac.tmp
(MRE.n3 <- median(MRE.n3.raw))
# MER
MER.n3.raw <- MAE.n3.raw/rF.n3.pr.tmp
(MER.n3 <- median(MER.n3.raw))
# Pred(25)
(Pred.n3.MRE <- sum((MRE.n3.raw <= 0.25)/length(rF.n3.ac.tmp)))
(Pred.n3.MER <- sum((MER.n3.raw <= 0.25)/length(rF.n3.ac.tmp)))

#-----k-nn法-----
# MAE
MAE.kn.raw <- abs(rF.kn.pr.tmp - rF.kn.ac.tmp)
(MAE.kn <- median(MAE.kn.raw))
# MRE
MRE.kn.raw <- MAE.kn.raw/rF.kn.ac.tmp
(MRE.kn <- median(MRE.kn.raw))
# MER
MER.kn.raw <- MAE.kn.raw/rF.kn.pr.tmp
(MER.kn <- median(MER.kn.raw))
# Pred(25)
(Pred.kn.MRE <- sum((MRE.kn.raw <= 0.25)/length(rF.kn.ac.tmp)))
(Pred.kn.MER <- sum((MER.kn.raw <= 0.25)/length(rF.kn.ac.tmp)))

#-----CF応用法-----
# MAE
MAE.cf.raw <- abs(rF.cf.pr.tmp - rF.cf.ac.tmp)
(MAE.cf <- median(MAE.cf.raw))
# MRE
MRE.cf.raw <- MAE.cf.raw/rF.cf.ac.tmp
(MRE.cf <- median(MRE.cf.raw))
# MER
MER.cf.raw <- MAE.cf.raw/rF.cf.pr.tmp
(MER.cf <- median(MER.cf.raw))
# Pred(25)
(Pred.cf.MRE <- sum((MRE.cf.raw <= 0.25)/length(rF.cf.ac.tmp)))
(Pred.cf.MER <- sum((MER.cf.raw <= 0.25)/length(rF.cf.ac.tmp)))

#-----missForest-----
# MAE
MAE.mF.raw <- abs(rF.mF.pr.tmp - rF.mF.ac.tmp)
(MAE.mF <- median(MAE.mF.raw))
# MRE
MRE.mF.raw <- MAE.mF.raw/rF.mF.ac.tmp
(MRE.mF <- median(MRE.mF.raw))
# MER
MER.mF.raw <- MAE.mF.raw/rF.mF.pr.tmp
(MER.mF <- median(MER.mF.raw))
# Pred(25)
(Pred.mF.MRE <- sum((MRE.mF.raw <= 0.25)/length(rF.mF.ac.tmp)))
(Pred.mF.MER <- sum((MER.mF.raw <= 0.25)/length(rF.mF.ac.tmp)))

#-----多重代入法-----
# MAE
MAE.mi.raw <- abs(rF.mi.pr.tmp - rF.mi.ac.tmp)
(MAE.mi <- median(MAE.mi.raw))
# MRE
MRE.mi.raw <- MAE.mi.raw/rF.mi.ac.tmp
(MRE.mi <- median(MRE.mi.raw))
# MER
MER.mi.raw <- MAE.mi.raw/rF.mi.pr.tmp
(MER.mi <- median(MER.mi.raw))
# Pred(25)
(Pred.mi.MRE <- sum((MRE.mi.raw <= 0.25)/length(rF.mi.ac.tmp)))
(Pred.mi.MER <- sum((MER.mi.raw <= 0.25)/length(rF.mi.ac.tmp)))
#----------#----------#----------#----------#----------#----------#----------#
# 出力
#-----MAE-----
MAE <- list(
  me = MAE.me.raw,
  del.p = MAE.n1.raw,
  del.s = MAE.n2.raw,
  del.ps = MAE.n3.raw,
  kn = MAE.kn.raw,
  CF = MAE.cf.raw,
  mF = MAE.mF.raw,
  mi = MAE.mi.raw
  )
boxplot(MAE,boxwex = 0.9,ylim = c(0,10000),yaxp=c(0,10000,10),ylab = "MAE")

#-----MRE-----
MRE <- list(
  me = MRE.me.raw,
  del.p = MRE.n1.raw,
  del.s = MRE.n2.raw,
  del.ps = MRE.n3.raw,
  kn = MRE.kn.raw,
  CF = MRE.cf.raw,
  mF = MRE.mF.raw,
  mi = MRE.mi.raw
)
boxplot(MRE,boxwex = 0.9,ylim = c(0,1.6),yaxp=c(0,1.6,8),ylab = "MRE")

#-----MER-----
MER <- list(
  me = MER.me.raw,
  del.p = MER.n1.raw,
  del.s = MER.n2.raw,
  del.ps = MER.n3.raw,
  kn = MER.kn.raw,
  CF = MER.cf.raw,
  mF = MER.mF.raw,
  mi = MER.mi.raw
)
boxplot(MER,boxwex = 0.9,ylim = c(0,1.6),yaxp=c(0,1.6,8),ylab = "MER")

#-----
result <- data.frame(
  name <- c("me","del p","del s","del ps","kn","cf","mf","mi"),
  mae <- c(MAE.me,MAE.n1,MAE.n2,MAE.n3,MAE.kn,MAE.cf,MAE.mF,MAE.mi),
  mre <- c(MRE.me,MRE.n1,MRE.n2,MRE.n3,MRE.kn,MRE.cf,MRE.mF,MRE.mi),
  mer <- c(MER.me,MER.n1,MER.n2,MER.n3,MER.kn,MER.cf,MER.mF,MER.mi),
  predmre <- c(Pred.me.MRE,Pred.n1.MRE,Pred.n2.MRE,Pred.n3.MRE,Pred.kn.MRE,Pred.cf.MRE,Pred.mF.MRE,Pred.mi.MRE),
  predmer <- c(Pred.me.MER,Pred.n1.MER,Pred.n2.MER,Pred.n3.MER,Pred.kn.MER,Pred.cf.MER,Pred.mF.MER,Pred.mi.MER)
)
write.csv(result,"res.csv",quote=FALSE,row.names=FALSE)
