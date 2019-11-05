library(frasyr)
# vignette の単純コピーです。ファイル名、設定は各資源で使っているものに置き換えてください。この例はチューニングなしのVPAですが、チューニングありの場合の設定は　https://ichimomo.github.io/frasyr/doc/vpa.html に解説があります
caa   <- read.csv("https://raw.githubusercontent.com/ichimomo/frasyr/dev/data-raw/ex2_caa.csv",  row.names=1)
waa   <- read.csv("https://raw.githubusercontent.com/ichimomo/frasyr/dev/data-raw/ex2_waa.csv",  row.names=1)
maa   <- read.csv("https://raw.githubusercontent.com/ichimomo/frasyr/dev/data-raw/ex2_maa.csv",  row.names=1)
dat <- data.handler(caa=caa, waa=waa, maa=maa, M=0.5)

# VPAによる資源量推定
res_vpa <- vpa(dat,fc.year=2015:2017,tf.year = 2015:2016,
               term.F="max",stat.tf="mean",Pope=TRUE,tune=FALSE,p.init=0.5)

# 結果のセーブ
save(res_vpa, file="res_vpa.rda")


