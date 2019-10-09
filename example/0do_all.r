##  環境設定（個々のPC環境によって設定を変えてください)

# 必要パッケージのインストール(最初に一回で大丈夫です。または、frasyrが更新されたら、いちおうfraysrはインストールしなおしてください）
# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("gridExtra")
## 現在ではfrasyrの開発ブランチ(dev)でのみ対応しているので、devのものをインストールしてください
# devtools::install_github("ichimomo/frasyr@dev")

# インストールしたパッケージの呼び出し
library(tidyverse)
library(gridExtra)
library(frasyr)
# devtools::load_all("../../frasyr") # ローカルフォルダのファイルからロードする場合

# 科学者会議で計算：MSY管理基準値の計算
source("1do_MSYest.r", encoding="UTF-8")
#-- 計算内容
#--- 1. 再生産関係のフィット => フィットの結果：res_SR_MSY
#--- 2. 再生産関係をもとにMSY管理基準値を計算　=> 推定結果：res_MSY
#--- その他、重要な数字には以下の名前がついています
#---    目標管理基準値＝Btarget0
#---    限界管理基準値＝Blimit0
#---    禁漁水準＝Bban0
#---    F_MSY=Fmsy0
#---    %SPR換算のF_SMY=SPR_MSY0

## 例データの実行結果
# > res_MSY$summary$SSB
# [1] 125050.363 489302.252  69517.272  58066.933  35242.288   5581.706  97857.378
# [8]  12205.995
#--- 3. グラフ
#------- * 再生産関係のグラフ：g1_SRplot
#------- * 漁獲量曲線のグラフ：g2_yield_curve
#------- * 2つのグラフのまとめ：graph_all

# 科学者会議で計算：推定した管理基準値で将来予測
source("2do_future_projection.r", encoding="UTF-8")
#-- 計算内容
#--- 1. currentFでの将来予測 => 将来予測結果; res_future_current
#--- 2. HCRを用いたときの将来予測 => 将来予測結果;res_future_0.8HCR
#--- 3. HCRを用いてβをいろいろ変えたときの将来予測行列 => 生データ; kobeII.data, サマリー; kobeII.table
#--- その他、重要な数字には以下の名前がついています（用いる管理基準値の基準を変えなければ、最初のプロセスと同じ値が入るはず）
#---    目標管理基準値＝Btarget_update
#---    限界管理基準値＝Blimit_update
#---    禁漁水準＝Bban_update
#---    F_MSY=Fmsy_update
#---    %SPR換算のF_SMY=SPR_MSY_update
#--- 4. グラフ
#------- * 神戸プロット：g3_kobe4 (神戸プロットの生データ; kobe.ratio)
#------- * HCR(beta=0.8)とcurrentFでの将来予測の比較：g4_future
#------- * 2つのグラフのまとめ：graph_all2

# 1年目の資源評価会議で計算：上記の"2do_future_projection.r"をコピーして、設定を変えて利用していく
## 主な変更ポイント
## - VPAの計算結果(最新のものに置き換える)
## - 将来予測の開始年(１年ずらす)
## - 体重・成熟率の設定
## - current Fの設定


