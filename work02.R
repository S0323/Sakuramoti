# ステップワイズ重回帰分析
#----------#----------#----------#----------#----------#----------#
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
#----------#----------#----------#----------#----------#----------#
library(MASS)

#-----平均値挿入法-----
SW.me.lm <- lm(SWE~.,fit.me) # 初期モデル構築
SW.me <- stepAIC(SW.me.lm,direction = "both") # ステップワイズ
SW.me.pred <- predict(SW.me,test)

SW.me.pr.tmp <- SW.me.pred   #予測値
SW.me.ac.tmp <- test$SWE     #実測値

#-----無欠損データ作成法-----
# P削除
SW.n1.lm <- lm(SWE~.,fit.n1)
SW.n1 <- stepAIC(SW.n1.lm,direction = "both")
SW.n1.pred <- predict(SW.n1,test)

SW.n1.pr.tmp <- SW.n1.pred
SW.n1.ac.tmp <- test$SWE
# S削除
SW.n2.lm <- lm(SWE~.,fit.n2)
SW.n2 <- stepAIC(SW.n2.lm,direction = "both")
SW.n2.pred <- predict(SW.n2,test)

SW.n2.pr.tmp <- SW.n2.pred
SW.n2.ac.tmp <- test$SWE
# P,S削除
SW.n3.lm <- lm(SWE~.,fit.n3)
SW.n3 <- stepAIC(SW.n3.lm,direction = "both")
SW.n3.pred <- predict(SW.n3,test)

SW.n3.pr.tmp <- SW.n3.pred
SW.n3.ac.tmp <- test$SWE

#-----k-nn法-----
SW.kn.lm <- lm(SWE~.,fit.kn)
SW.kn <- stepAIC(SW.kn.lm,direction = "both")
SW.kn.pred <- predict(SW.kn,test)

SW.kn.pr.tmp <- SW.kn.pred
SW.kn.ac.tmp <- test$SWE

#-----CF応用法-----
SW.cf.lm <- lm(SWE~.,fit.cf)
SW.cf <- stepAIC(SW.cf.lm,direction = "both")
SW.cf.pred <- predict(SW.cf,test)

SW.cf.pr.tmp <- SW.cf.pred
SW.cf.ac.tmp <- test$SWE

#-----missForest-----
library(missForest)

for(i in 1:10){
  set.seed(i)
  fit.mF <- missForest(fit)$ximp
  
  SW.mF.lm <- lm(SWE~.,fit.mF)
  SW.mF <- stepAIC(SW.mF.lm,direction = "both")
  SW.mF.pred <- predict(SW.mF,test)
  
  if(i==1){
    SW.mF.pr.tmp <- SW.mF.pred
    SW.mF.ac.tmp <- test$SWE
  }
  else{
    SW.mF.pr.tmp <- c(SW.mF.pred,SW.mF.pr.tmp)
    SW.mF.ac.tmp <- c(SW.mF.ac.tmp,test$SWE)
  }
}

#-----多重代入法-----
library(mice)

dupe <- 10 # 複数個数

for(i in 1:10){
  fit.m <- mice(fit,seed = i,m = dupe)
  fit.m.rep <- complete(fit.m,action = "repeated")
  
  fit.mi <- fit
  for(j in 0:(ncol(fit)-1)){
    fit.mi[,j+1] <- apply(fit.m.rep[,(j*dupe+1):((j+1)*dupe)],1,median)
  }
  
  SW.mi.lm <- lm(SWE~.,fit.mi)
  SW.mi <- stepAIC(SW.mi.lm,direction = "both")
  SW.mi.pred <- predict(SW.mi,test)
  
  if(i==1){
    SW.mi.pr.tmp <- SW.mi.pred
    SW.mi.ac.tmp <- test$SWE
  }
  else{
    SW.mi.pr.tmp <- c(SW.mi.pred,SW.mi.pr.tmp)
    SW.mi.ac.tmp <- c(SW.mi.ac.tmp,test$SWE)
  }
}

#----------#----------#----------#----------#----------#----------#
# 評価
#-----平均値挿入法-----
# MAE
SW.MAE.me.raw <- abs(SW.me.pr.tmp-SW.me.ac.tmp)
(SW.MAE.me <- median(SW.MAE.me.raw))
# MRE
SW.MRE.me.raw <- SW.MAE.me.raw/SW.me.ac.tmp
(SW.MRE.me <- median(SW.MRE.me.raw))
# MER
SW.MER.me.raw <- SW.MAE.me.raw/SW.me.pr.tmp
(SW.MER.me <- median(SW.MER.me.raw))
# Pred(25)
(SW.Pred.me.MRE <- sum((SW.MRE.me.raw <= 0.25)/length(SW.me.ac.tmp)))
(SW.Pred.me.MER <- sum((SW.MER.me.raw <= 0.25)/length(SW.me.ac.tmp)))

#-----無欠損データ作成法-----
# P削除
# MAE
SW.MAE.n1.raw <- abs(SW.n1.pr.tmp-SW.n1.ac.tmp)
(SW.MAE.n1 <- median(SW.MAE.n1.raw))
# MRE
SW.MRE.n1.raw <- SW.MAE.n1.raw/SW.n1.ac.tmp
(SW.MRE.n1 <- median(SW.MRE.n1.raw))
# MER
SW.MER.n1.raw <- SW.MAE.n1.raw/SW.n1.pr.tmp
(SW.MER.n1 <- median(SW.MER.n1.raw))
# Pred(25)
(SW.Pred.n1.MRE <- sum((SW.MRE.n1.raw <= 0.25)/length(SW.n1.ac.tmp)))
(SW.Pred.n1.MER <- sum((SW.MER.n1.raw <= 0.25)/length(SW.n1.ac.tmp)))

# S削除
# MAE
SW.MAE.n2.raw <- abs(SW.n2.pr.tmp-SW.n2.ac.tmp)
(SW.MAE.n2 <- median(SW.MAE.n2.raw))
# MRE
SW.MRE.n2.raw <- SW.MAE.n2.raw/SW.n2.ac.tmp
(SW.MRE.n2 <- median(SW.MRE.n2.raw))
# MER
SW.MER.n2.raw <- SW.MAE.n2.raw/SW.n2.pr.tmp
(SW.MER.n2 <- median(SW.MER.n2.raw))
# Pred(25)
(SW.Pred.n2.MRE <- sum((SW.MRE.n2.raw <= 0.25)/length(SW.n2.ac.tmp)))
(SW.Pred.n2.MER <- sum((SW.MER.n2.raw <= 0.25)/length(SW.n2.ac.tmp)))

# P,S削除
# MAE
SW.MAE.n3.raw <- abs(SW.n3.pr.tmp-SW.n3.ac.tmp)
(SW.MAE.n3 <- median(SW.MAE.n3.raw))
# MRE
SW.MRE.n3.raw <- SW.MAE.n3.raw/SW.n3.ac.tmp
(SW.MRE.n3 <- median(SW.MRE.n3.raw))
# MER
SW.MER.n3.raw <- SW.MAE.n3.raw/SW.n3.pr.tmp
(SW.MER.n3 <- median(SW.MER.n3.raw))
# Pred(25)
(SW.Pred.n3.MRE <- sum((SW.MRE.n3.raw <= 0.25)/length(SW.n3.ac.tmp)))
(SW.Pred.n3.MER <- sum((SW.MER.n3.raw <= 0.25)/length(SW.n3.ac.tmp)))

#-----k-nn法-----
# MAE
SW.MAE.kn.raw <- abs(SW.kn.pr.tmp-SW.kn.ac.tmp)
(SW.MAE.kn <- median(SW.MAE.kn.raw))
# MRE
SW.MRE.kn.raw <- SW.MAE.kn.raw/SW.kn.ac.tmp
(SW.MRE.kn <- median(SW.MRE.kn.raw))
# MER
SW.MER.kn.raw <- SW.MAE.kn.raw/SW.kn.pr.tmp
(SW.MER.kn <- median(SW.MER.kn.raw))
# Pred(25)
(SW.Pred.kn.MRE <- sum((SW.MRE.kn.raw <= 0.25)/length(SW.kn.ac.tmp)))
(SW.Pred.kn.MER <- sum((SW.MER.kn.raw <= 0.25)/length(SW.kn.ac.tmp)))

#-----CF応用法-----
# MAE
SW.MAE.cf.raw <- abs(SW.cf.pr.tmp-SW.cf.ac.tmp)
(SW.MAE.cf <- median(SW.MAE.cf.raw))
# MRE
SW.MRE.cf.raw <- SW.MAE.cf.raw/SW.cf.ac.tmp
(SW.MRE.cf <- median(SW.MRE.cf.raw))
# MER
SW.MER.cf.raw <- SW.MAE.cf.raw/SW.cf.pr.tmp
(SW.MER.cf <- median(SW.MER.cf.raw))
# Pred(25)
(SW.Pred.cf.MRE <- sum((SW.MRE.cf.raw <= 0.25)/length(SW.cf.ac.tmp)))
(SW.Pred.cf.MER <- sum((SW.MER.cf.raw <= 0.25)/length(SW.cf.ac.tmp)))

#-----missForest-----
# MAE
SW.MAE.mF.raw <- abs(SW.mF.pr.tmp-SW.mF.ac.tmp)
(SW.MAE.mF <- median(SW.MAE.mF.raw))
# MRE
SW.MRE.mF.raw <- SW.MAE.mF.raw/SW.mF.ac.tmp
(SW.MRE.mF <- median(SW.MRE.mF.raw))
# MER
SW.MER.mF.raw <- SW.MAE.mF.raw/SW.mF.pr.tmp
(SW.MER.mF <- median(SW.MER.mF.raw))
# Pred(25)
(SW.Pred.mF.MRE <- sum((SW.MRE.mF.raw <= 0.25)/length(SW.mF.ac.tmp)))
(SW.Pred.mF.MER <- sum((SW.MER.mF.raw <= 0.25)/length(SW.mF.ac.tmp)))

#-----多重代入法-----
# MAE
SW.MAE.mi.raw <- abs(SW.mi.pr.tmp-SW.mi.ac.tmp)
(SW.MAE.mi <- median(SW.MAE.mi.raw))
# MRE
SW.MRE.mi.raw <- SW.MAE.mi.raw/SW.mi.ac.tmp
(SW.MRE.mi <- median(SW.MRE.mi.raw))
# MER
SW.MER.mi.raw <- SW.MAE.mi.raw/SW.mi.pr.tmp
(SW.MER.mi <- median(SW.MER.mi.raw))
# Pred(25)
(SW.Pred.mi.MRE <- sum((SW.MRE.mi.raw <= 0.25)/length(SW.mi.ac.tmp)))
(SW.Pred.mi.MER <- sum((SW.MER.mi.raw <= 0.25)/length(SW.mi.ac.tmp)))
#----------#----------#----------#----------#----------#----------#
# 出力
#-----MAE-----
MAE <- list(
  me = SW.MAE.me.raw,
  del.p = SW.MAE.n1.raw,
  del.s = SW.MAE.n2.raw,
  del.ps = SW.MAE.n3.raw,
  kn = SW.MAE.kn.raw,
  CF = SW.MAE.cf.raw,
  mF = SW.MAE.mF.raw,
  mi = SW.MAE.mi.raw
)
boxplot(MAE,boxwex = 0.9,ylim = c(0,10000),yaxp=c(0,10000,10),ylab = "MAE")

#-----MRE-----
MRE <- list(
  me = SW.MRE.me.raw,
  del.p = SW.MRE.n1.raw,
  del.s = SW.MRE.n2.raw,
  del.ps = SW.MRE.n3.raw,
  kn = SW.MRE.kn.raw,
  CF = SW.MRE.cf.raw,
  mF = SW.MRE.mF.raw,
  mi = SW.MRE.mi.raw
)
boxplot(MRE,boxwex = 0.9,ylim = c(0,1.6),yaxp=c(0,1.6,8),ylab = "MRE")

#-----MER-----
MER <- list(
  me = SW.MER.me.raw,
  del.p = SW.MER.n1.raw,
  del.s = SW.MER.n2.raw,
  del.ps = SW.MER.n3.raw,
  kn = SW.MER.kn.raw,
  CF = SW.MER.cf.raw,
  mF = SW.MER.mF.raw,
  mi = SW.MER.mi.raw
)
boxplot(MER,boxwex = 0.9,ylim = c(0,1.6),yaxp=c(0,1.6,8),ylab = "MER")

#-----
result <- data.frame(
  name <- c("me","del p","del s","del ps","kn","cf","mf","mi"),
  mae <- c(SW.MAE.me,SW.MAE.n1,SW.MAE.n2,SW.MAE.n3,SW.MAE.kn,SW.MAE.cf,SW.MAE.mF,SW.MAE.mi),
  mre <- c(SW.MRE.me,SW.MRE.n1,SW.MRE.n2,SW.MRE.n3,SW.MRE.kn,SW.MRE.cf,SW.MRE.mF,SW.MRE.mi),
  mer <- c(SW.MER.me,SW.MER.n1,SW.MER.n2,SW.MER.n3,SW.MER.kn,SW.MER.cf,SW.MER.mF,SW.MER.mi),
  predmre <- c(SW.Pred.me.MRE,SW.Pred.n1.MRE,SW.Pred.n2.MRE,SW.Pred.n3.MRE,SW.Pred.kn.MRE,SW.Pred.cf.MRE,SW.Pred.mF.MRE,SW.Pred.mi.MRE),
  predmer <- c(SW.Pred.me.MER,SW.Pred.n1.MER,SW.Pred.n2.MER,SW.Pred.n3.MER,SW.Pred.kn.MER,SW.Pred.cf.MER,SW.Pred.mF.MER,SW.Pred.mi.MER)
)
write.csv(result,"res.csv",quote=FALSE,row.names=FALSE)
