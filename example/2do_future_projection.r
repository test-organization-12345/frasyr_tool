#- HCRに従った将来予測の実施
#-- 1) 読み込みファイルの設定
#--- 将来予測で使うVPA結果を更新するか？
#---  (0: しないでMSY計算に使った結果を使う
#        （MSY推定結果が保存されているファイルに記録されているものを利用; 管理基準値を最初に計算する研究機関会議用）
#---   1: 更新された新しいVPA結果を使う <- 資源評価が更新される１年目以降用)
use_new_vpa_res <- 0
if(use_new_vpa_res==1){
    # 新しいVPA結果のパス名
    vpa_file_path_update <- "res_vpa2.rda"
    #--- VPAの結果のファイルの種類（1: Rオブジェクト, 2: csv形式)
    vpa_file_type <- 1
}
#--- MSY推定結果（res_MSY）が保存されているファイルの名前
MSY_file_path <- "res_MSY_HSL2A1.rda"
#--- 将来予測結果を保存するファイルの名前（以下のオブジェクトが入っています）
#---- res_future_0.8HCR ; β=0.8の将来予測の結果
#---- res_future_current; Fcurrentでの将来予測の結果
#---- kobeII.table      ; kobe II matrixの表
future_file_path <- "res_futures0.rda"
#--- グラフのファイル名
graph_file_future <- "future_graph0.png"
#--- 結果をcsvファイルで出力するときのファイル名
csv_file_future <- "future0.csv"
#--- 結果の簡単なグラフをpdfファイルで出力するときのファイル名(まだちゃんとした結果は出ないです)
pdf_file_future <- "future0.pdf"

#--- 実行したときの警告を表示するかどうか (-1: 表示しない, 0: 表示する)
warning_option <- -1

#-- 2) 将来予測の基本設定
#--- 漁獲量の計算方法（1:VPAと同じ設定を使う, 2:Popeの近似式を使う, 3:Bavanovの式を使う）
is_pope <- 1
#--- 乱数のシード
future_seed <- 1
#--- MSY計算時のシミュレーション回数(1000回以上推奨)
future_nsim <- 1000
#--- 計算した結果の簡単な図を示す（1:示す,1以外:しない）
future_est_plot <- 1

#-- 生物パラメータ
#--- 将来予測で仮定する年齢別体重(資源量計算用)の設定(1:年で指定, 2:直接指定, 3: MSY計算と同じ設定,
#---                                            4: 資源尾数に対する回帰モデルからの予測値を使う)
select_waa_in_future <- 3
if(select_waa_in_future==1){ # 1の場合にはこちらを設定
    waa_year_in_future <- 2015:2017
}
if(select_waa_in_future==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    waa_in_future <- c(100,200,300,400)
}
#--- 将来予測で仮定する年齢別体重(漁獲量計算用)の設定(0:資源量計算用と同じ, 1:年で指定, 2:直接指定, 3: MSY計算と同じ)
select_waa.catch_in_future <- 3
if(select_waa.catch_in_future==1){ # 1の場合にはこちらを設定
    waa.catch_year_in_future <- 2015:2017
}
if(select_waa.catch_in_future==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    waa.catch_in_future <- c(100,200,300,400)
}

#--- 将来予測で仮定する年齢別成熟率の設定 (1:年で指定, 2:直接指定, 3: MSY計算と同じ)
select_maa_in_future <- 3
if(select_maa_in_future==1){ # 1の場合にはこちらを設定
    maa_year_in_future <- 2015:2017
}
if(select_maa_in_future==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    maa_in_future <- c(0,0,0.5,1)
}

#--- 将来予測で仮定する自然死亡係数の設定 (1: 年で指定, 2: 直接指定, 3:MSY計算と同じ))
select_M_in_future <- 3
if(select_M_in_future==1){ # 1の場合にはこちらを設定
    M_year_in_future <- 2015:2017
}
if(select_M_in_future==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    M_in_future <- c(0,0,0.5,1)
}

#-- 3) 再生産関係の設定
#--- MSY計算とすべて同じ仮定を使うか (1: 使う, 0: 使わずすべて手動で設定する)
#--- クロスチェック（再生産関係Aを前提とした管理基準値のもとで、再生産関係が実はBだった場合のシミュレーション）などをする場合に使う
use_MSY_SR <- 1
if(use_MSY_SR==0){ # すべて手動で計算する場合、以下のオプションを設定
    #--- 将来予測で仮定する再生産関係の推定結果が保存されているファイルの名前
    SR_file_path <- "res_SR_HSL2.rda"    
    #---  バイアス補正（シミュレーションの平均と決定論的予測を一致させる）(1:する, 1以外: しない)
    bias_correction_future <- 1
    #--- 加入変動の誤差分布 (1: 対数正規誤差, 2: 残差リサンプリング)
    SR_error_future <- 2
    #---- SR_error_future=1（対数正規分布の誤差）の場合の設定
    if(SR_error_future==1){ # 対数正規分布の場合は自己相関のオプションも選ぶ
        #--- 加入変動のシグマ (-1: 推定値を使う, 0以上の実数: ここで指定した値に置き換える)
        SR_sigma_future <- 0.1
        #--- 自己相関の仮定 (-1: 推定結果どおりに設定する, 0: 推定結果にかかわらず自己相関を「なし」にする,
        #---               0以上の数字：推定結果にかかわらず自己相関係数をここで設定した値にする)
        select_AR_future <- -1
    }
    #---- SR_error_future=2（リサンプリング誤差）の場合の設定
    if(SR_error_future==2){ 
        #--- リサンプリングの年の範囲(0: 全年, それ以外の数字: 指定した加入年の残差のみリサンプリング）
        select_resample_year <- 0 # or 1990:2000
    }
}

#-- 4) 簡易MSEの設定
#--- 管理MSEを利用するかどうか (0: しない（通常の将来予測）, 1: する)
do_MSE <- 0
if(do_MSE==1){ # MSEをやる場合は以下の設定をする
    # MSE内でABC推定のための将来予測をする場合の繰り返し回数
    MSE_nsim <- 1000
    # MSE内で漁獲する場合の漁獲率の上限
    max_ER <- 0.8
    # MSE内で仮定する再生産関係式 (0: 真の個体群動態と同じ仮定を用いる, 1: 別の再生産関係を仮定)
    SR_assumption_MSE <- 0
    if(SR_assumption_MSE==1){ # 別の再生産関係式を仮定する場合
        #--- 将来予測で仮定する再生産関係の推定結果が保存されているファイルの名前
        SR_file_MSE_path <- "res_SR_HSL2.rda"        
        #---  バイアス補正（シミュレーションの平均と決定論的予測を一致させる）(1:する, 1以外: しない)
        bias_correction_MSE <- 1
        #--- 加入変動の誤差分布 (1: 対数正規誤差, 2: 残差リサンプリング)
        SR_error_MSE <- 1
        #---- SR_error_MSE=1（対数正規分布の誤差）の場合の設定
        if(SR_error_MSE==1){ # 対数正規分布の場合, sigmaと自己相関のオプションを選ぶ
            #--- 加入変動のシグマ (-1: 推定値を使う, 0以上の実数: ここで指定した値に置き換える)
            SR_sigma_MSE <- 1
            #--- 自己相関の仮定 (-1: 推定結果どおりに設定する, 0: 推定結果にかかわらず自己相関を「なし」にする,
            #---               0以上の数字：推定結果にかかわらず自己相関係数をここで設定した値にする)
            select_AR_MSE <- -1
        }
        #---- SR_error_MSE=2（リサンプリング誤差）の場合の設定
        if(SR_error_MSE==2){ 
            #--- リサンプリングの年の範囲(0: 全年, それ以外の数字: 指定した加入年の残差のみリサンプリング）
            select_resample_year_MSE <- 0 # or 1990:2000
        }
    }
}

#-- 5) 年数や回数などの設定
#--- 将来予測開始年
future_start_year <- 2018
#--- ABC計算年（この年からHCRに沿った漁獲を開始する）
future_ABC_year   <- 2020
#--- 将来予測の実施年数
future_nyear   <- 50
#--- シミュレーションの一回目は決定論的予測の結果とする (1: する, 0: しない)
det_run <- 1
#-- 直近の加入や漁獲の仮定
#--- 特定の年の加入を外部から与える (0: 設定しない, 1: 設定する)
select_specific_recruit <- 0
#---- 加入を外部から与える場合の設定
if(select_specific_recruit==1){
    recruit_year <- c(2018,2019,2020,2021)
    recruit_number <- c(10000,20000,20000,20000)
}
#--- 特定の年の漁獲量を外部から与える (0: 設定しない, 1: 設定する)
select_specific_catch <- 0
#---- 漁獲量を外部から与える場合の設定
if(select_specific_catch==1){
    catch_year <- c(2018,2019)
    catch_weight <- c(1000,1000)
}

#-- 6) 漁獲シナリオの設定
#--- HCRを実施するときのSSB参照年 (0: ABC算定年とSSBの参照年が同じ。0より小さい負の整数：時間遅れの年数（２年遅れの場合は-2）。デフォルトは0)
HCR_year_lag <- 0
#--- HCRが有効な場合の管理基準値 (0: MSY_resで指定されたBtarget0, Blimit0, Bban0をそのまま使う。
#---                         1: MSY_resの設定を上書きする)
overwrite_RP <- 0
if(overwrite_RP==1){ # MSY_resの設定を上書きする場合
    #--- 目標管理基準値の選択 (0: MSY,
    #---                   1以上の数字: MSY_res$summaryの行数,
    #---                   負の数字: インタラクティブに決定)
    select_Btarget <- -1
    #--- 限界管理基準値の選択 (0: 60%MSY,
    #---                   1以上の数字: MSY_res$summaryの行数,
    #---                   負の数字: インタラクティブに決定)
    select_Blimit  <- -1
    #--- 禁漁水準の選択      (0: 10%MSY,
    #---                   1以上の数字: MSY_res$summaryの行数,
    #---                   負の数字: インタラクティブに決定)
    select_Bban  <- -1
}
#---- HCRの将来予測におけるデフォルトのベータ(通常は0.8)
beta_default <- 0.8
#---- ベータをいろいろ変える将来予測におけるベータの範囲
beta_table <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

#--将来予測におけるF at ageの設定
#--  (HCRをもとにした将来予測では、ABC計算年以前にはselect_FAA_preABCで設定された年齢別漁獲係数で漁獲し、
#--   ABC計算年以降はselect_FAA_afterABCで設定された年齢別漁獲係数×β×γで漁獲します)
#--  これとは別に、select_FAA_preABCで設定された年齢別漁獲係数で漁獲しつづける将来予測(「現在の漁獲圧」シナリオ)も実施します。
#---
#--- ABC計算年以前のF-at-age(FAA)の設定(デフォルトは3または4で、ABC算定年以前のF at ageを最もよく代表していると思われるFを用いる)
#---   1: Fmsy at age (=MSY_resで"Btarget0"の管理基準値に対応するF at age)
#---   2: 手動でFcurrentを設定する
#---   3: vpaのF at ageに対して年を指定し、その平均を使う,
#---   4: 選択率を参照する年と漁獲圧を参照する年を別にする（漁獲圧はSPR換算して、指定された選択率において同じ漁獲圧になるようなFcurrentを用いる。SPR換算するときの生物パラメータは、漁獲圧として指定された年の生物パラメータの平均とする））
#---   5: 漁獲圧はFmsyを使うが、別の選択率を用いる（漁獲圧はSPR換算して、Fmsyと漁獲圧になるようなFcurrentを計算する。SPR換算するときの生物パラメータは、MSY推定に用いた生物パラメータの平均とする）
select_FAA_preABC <- 3
#---- 上で2を選んだ場合:FcurrentとしたいFをベクトルで入力
if(select_FAA_preABC==2){
    FAA_preABC <- c(0.02302929,0.04498867,0.07500238,
                      0.10139408,0.14474343,0.15539237,
                      0.16375070,0.12544913,0.12544913)
}
#---- 上で3を選んだ場合:Fを平均したい年数
if(select_FAA_preABC==3){
    # 実際の年を指定する場合
    FAA_preABC_year <- 2015:2017
    # マイナス値を入れると最新年から数えて何年分遡るかで指定できる
    #FAA_preABC_year <- -1:-3
}
#---- 上で4を選んだ場合:選択率参照年とFcurrent参照年を分ける
if(select_FAA_preABC==4){
    # 漁獲圧の強さとして近年の漁獲圧を代表とする年(実際の年を指定するか、マイナス値で相対値として指定する）
    FAA_preABC_year <- 2015:2017
    # FAA_preABC_year <- -1:-3
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    Fsel_preABC_year <- 2010:2017
    # Fsel_year <- -1:-10
}
#---- 上で5を選んだ場合:Fmsyをもとにするが別の選択率を用いる
if(select_FAA_preABC==5){
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    Fsel_preABC_year <- 2010:2017
    # Fsel_year <- -1:-10
}

#--- ABC計算年以降のF-at-age(FAA)の設定(デフォルトは1)
#---   1: Fmsy at age (=MSY_resで"Btarget0"の管理基準値に対応するF at age)
#---   2: ABC.year以前のF at ageと共通
#---   3: 手動でFcurrentを設定する
#---   4: vpaのF at ageに対して年を指定し、その平均を使う,
#---   5: 選択率を参照する年と漁獲圧を参照する年を別にする（漁獲圧はSPR換算して、指定された選択率において同じ漁獲圧になるようなFcurrentを用いる。SPR換算するときの生物パラメータは、漁獲圧として指定された年の生物パラメータの平均とする））
#---   6: 漁獲圧はFmsyを使うが、別の選択率を用いる（漁獲圧はSPR換算して、Fmsyと漁獲圧になるようなFcurrentを計算する。SPR換算するときの生物パラメータは、MSY推定に用いた生物パラメータの平均とする）

select_FAA_afterABC <- 1
#---- 上で3を選んだ場合はこちらも設定する
if(select_FAA_afterABC==3){ 
    FAA_afterABC <- c(0.2,0.3,0.3,0.44) # ベクトルとして直接指定する場合
}
#---- 上で4を選んだ場合:Fを平均したい年数
if(select_FAA_afterABC==4){
    # 実際の年を指定する場合
    #FAA_afterABC_year <- 2015:2017
    # マイナス値を入れると最新年から数えて何年分遡るかで指定できる
    FAA_afterABC_year <- -1:-3
}
#---- 上で5を選んだ場合:選択率参照年とFcurrent参照年を分ける
if(select_FAA_afterABC==5){
    # 漁獲圧の強さとして近年の漁獲圧を代表とする年(実際の年を指定するか、マイナス値で相対値として指定する）
    FAA_afterABC_year <- 2015:2017
    # FAA_afterABC_year <- -1:-3
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    Fsel_afterABC_year <- 2010:2017
    # Fsel_afterABC_year <- -1:-10
}
#---- 上で6を選んだ場合:Fmsyをもとにするが別の選択率を用いる
if(select_FAA_afterABC==6){
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    Fsel_afterABC_year <- 2010:2017
    # Fsel_year <- -1:-10
}

#-- 7) 出力の調整：下記の各項目について表として出力したい年数を入れるか、表が必要ない場合はマイナス値を入れる
#--- 将来の平均漁獲量
year_catch_average <- c(2019:2030,2040,2050)
#--- 将来の平均親魚量
year_ssb_average <- c(2019:2030,2040,2050)
#--- 目標管理基準値を上回る確率
year_catch_average <- c(2019:2030,2040,2050)
#--- 目標管理基準値を上回る確率
year_ssbtarget_prob <- c(2019:2030,2040,2050)
#--- 限界管理基準値を上回る確率
year_ssblimit_prob  <- c(2019:2030,2040,2050)
#--- 禁漁水準を上回る確率
year_ssbban_prob    <- c(2019:2030,2040,2050)
#--- 過去の最低親魚量を上回る確率
year_ssbmin_prob    <- c(2019:2030,2040,2050)
#--- 過去の最高親魚量を上回る確率
year_ssbmax_prob    <- c(2019:2030,2040,2050)
#--- 漁獲量のAAV
year_catch_aav      <- c(2019:2030,2040,2050)
#--- Fの削減率の平均
year_Fsakugen_mean  <- c(2019:2030,2040,2050)


####################################################
### 以下は基本的には編集しないこと
####################################################
old.warning <- options()$warn
options(warn=warning_option)
options(tibble.width=100,tibble.print_max=Inf)

# read MSY res
res_MSY <- load(MSY_file_path) %>% get()

input_MSY <- res_MSY$input.list[[1]]

# read VPA res
res_vpa_MSY <- res_MSY$input$msy$res0     
{if(use_new_vpa_res==0){
     res_vpa_update <- res_MSY$input$msy$res0
 }
 else{
     if(vpa_file_type==1){
         res_vpa_update <- load(vpa_file_path_update) %>% get()
     }
     if(vpa_file_type==2){
         res_vpa_update <- read.vpa(vpa_file_path_update)
     }
 }}

# 管理基準値の設定
if(overwrite_RP==1){
    # define RP.definition for Btarget
    if(select_Btarget!=0){
        if(select_Btarget<0){
            print(select(res_MSY$summary,-AR,-Catch.CV))
            select_Btarget <- readline("Enter row number to be Btarget: ")
            select_Btarget <- as.integer(select_Btarget)
        }
        res_MSY$summary$RP.definition[1] <- NA    
        res_MSY$summary$RP.definition[select_Btarget] <- "Btarget0"
    }
    # define RP.definition for Blimit
    if(select_Blimit!=0){
        if(select_Blimit<0){
            print(select(res_MSY$summary,-AR,-Catch.CV))
            select_Blimit <- readline("Enter row number to be Blimit: ")
            select_Blimit <- as.integer(select_Blimit)
        }
        res_MSY$summary$RP.definition[which(res_MSY$summary$RP.definition=="Blimit0")] <- NA
        res_MSY$summary$RP.definition[select_Blimit] <- "Blimit0"
    }
    # define RP.definition for Bban
    if(select_Bban!=0){
        if(select_Bban<0){
            print(select(res_MSY$summary,-AR,-Catch.CV,-RP.definition))
            select_Bban <- readline("Enter row number to be Bban: ")
            select_Bban <- as.integer(select_Bban)
        }
        res_MSY$summary$RP.definition[which(res_MSY$summary$RP.definition=="Bban0")] <- NA    
        res_MSY$summary$RP.definition[select_Bban] <- "Bban0"
    }
}

Btarget_update <- derive_RP_value(res_MSY$summary,"Btarget0")$SSB
Blimit_update  <- derive_RP_value(res_MSY$summary,"Blimit0")$SSB
Bban_update    <- derive_RP_value(res_MSY$summary,"Bban0")$SSB
SPR_MSY_update <- derive_RP_value(res_MSY$summary,"Btarget0")$perSPR
Fmsy_update <- res_MSY$Fvector %>%
    slice(which(res_MSY$summary$RP.definition=="Btarget0")) %>%
    as.numeric()

cat("## --------------------------------------------------------\n")
cat("Btarget:",Btarget_update,"\n")
cat("Blimit :",Blimit_update,"\n")
cat("Bban   :",Bban_update,"\n")
cat("Fmsy:  ",Fmsy_update,"\n")
cat("## --------------------------------------------------------\n")

{if(use_MSY_SR==0){
     # read SR fit results
     res_SR_future <- load(SR_file_path) %>% get()     
     if(SR_error_future==1){# lognormal
         if(res_SR_future$input$SR=="HS") SRfun_future <- HS.recAR
         if(res_SR_future$input$SR=="BH") SRfun_future <- BH.recAR
         if(res_SR_future$input$SR=="RI") SRfun_future <- RI.recAR
         
         rho_future <- switch(as.character(select_AR_future),
                              "-1"=res_SR_future$pars$rho,
                              "0"=0,
                              select_AR_future)
        opt_SR_future <- list(a=res_SR_future$pars$a,
                           b=res_SR_future$pars$b,
                           rho=rho_future,
                           sd=ifelse(SR_sigma_future<0,res_SR_future$pars$sd,SR_sigma_future),
                           bias.correction=ifelse(bias_correction_future==1,TRUE,FALSE),
                           resample=FALSE,resid=res_SR_future$pars$resid,
                           resid.year=NULL)
        if(rho_future>0){
            opt_SR_future$resid.year <- AR_average_year_future 
        }
    }

    if(SR_error_future==2){# lognormal
        SRfun_future <- resample.rec
        if(select_resample_year==0){
            resid_selected <- res_SR_future$resid
        }
        else{
            resid_selected <- res_SR_future$resid[res_SR_future$input$SRdata$year%in%
                                      select_resample_year]            
        }
        opt_SR_future <- list(a=res_SR_future$pars$a,
                              b=res_SR_future$pars$b,
                              resample=TRUE,
                              SR=res_SR_future$input$SR,
                              resid=resid_selected)    
    }
    opt_SR_future$bias.correction <- ifelse(bias_correction_future==1,TRUE,FALSE)
 }
 else{
     opt_SR_future <- input_MSY$rec.arg
     SRfun_future <- input_MSY$recfunc
 }}

{if(do_MSE==1){
     if(SR_assumption_MSE==1){
         res_SR_MSE <- load(SR_file_MSE_path) %>% get()

         if(SR_error_MSE==1){# lognormal
             if(res_SR_MSE$input$SR=="HS") SRfun_MSE <- HS.recAR
             if(res_SR_MSE$input$SR=="BH") SRfun_MSE <- BH.recAR
             if(res_SR_MSE$input$SR=="RI") SRfun_MSE <- RI.recAR

             rho_MSE <- switch(as.character(select_AR_MSE),
                                  "-1"=res_SR_MSE$pars$rho,
                                  "0"=0,
                                  select_AR_MSE)
             opt_SR_MSE <- list(a=res_SR_MSE$pars$a,
                                   b=res_SR_MSE$pars$b,
                                   rho=rho_MSE,
                                   sd=ifelse(SR_sigma_MSE<0,res_SR_MSE$pars$sd,SR_sigma_MSE),
                                   bias.correction=ifelse(bias_correction_MSE==1,TRUE,FALSE),
                                   resample=FALSE,resid=res_SR_MSE$pars$resid,
                                   resid.year=NULL)
             if(rho_MSE>0){
                 opt_SR_MSE$resid.year <- AR_average_year_MSE 
             }
         }

         if(SR_error_MSE==2){# lognormal
             SRfun_MSE <- resample.rec
             select_resample_year_MSE <- ifelse(select_resample_year_MSE==0,
                                         TRUE,
                                         select_resample_year_MSE)
             resid_selected <- res_SR_MSE$resid[res_SR_MSE$input$SRdata$year%in%
                                                   select_resample_year_MSE]
             opt_SR_MSE <- list(resample=TRUE,
                                   SR=res_SR_MSE$input$SR,
                                   resid=resid_selected)    
         }
         opt_SR_MSE$bias.correction <- ifelse(bias_correction_MSE==1,TRUE,FALSE)
     }
     if(SR_assumption_MSE==0){
         opt_SR_MSE <- opt_SR_future
         SRfun_MSE <- SRfun_future
     }
     opt_MSE <- list(recfunc=SRfun_MSE,rec.arg=opt_SR_MSE,
                     N=MSE_nsim,max.ER=max_ER)     
 }
 else{
     opt_MSE <- NULL
     }
}

# setting future F pre ABC year
if(select_FAA_preABC%in%c(1,3,4,5)){
    if(select_FAA_preABC==1){
        FAA_preABC <- Fmsy_update
    }
    if(select_FAA_preABC==3){
        FAA_preABC <- apply_year_colum(res_vpa_update$faa,target_year=FAA_preABC_year)
    }
    if(select_FAA_preABC==4){
        FAA_preABC <- convert_faa_perSPR(res_vpa_update,sel_year=Fsel_preABC_year,faa_year=FAA_preABC_year)
    }
    if(select_FAA_preABC==5){
        FAA_preABC <- convert_faa_perSPR(res_vpa_update,sel_year=Fsel_preABC_year,faa_year=NULL,
                                         Fcurrent_MSY=Fmsy_update)
    }    
    cat("Set future F vector before ABC.year as: ")
    FAA_preABC  %>% as.numeric() %>% round(2) %>% print()    
}
if(select_FAA_preABC>5) stop("select_FAA_preABC should be 1-5")

# setting future F after ABC year
if(select_FAA_afterABC%in%c(1,2,4,5,6)){
    if(select_FAA_afterABC==1){
        FAA_afterABC <- Fmsy_update
    }
    if(select_FAA_afterABC==2){
        FAA_afterABC <- FAA_preABC
    }
    if(select_FAA_afterABC==4){
        FAA_afterABC <- apply_year_colum(res_vpa_update$faa,target_year=FAA_afterABC_year)
    }    
    if(select_FAA_afterABC==5){
        FAA_afterABC <- convert_faa_perSPR(res_vpa_update,sel_year=Fsel_afterABC_year,faa_year=FAA_afterABC_year)        
    }
    if(select_FAA_afterABC==6){
        FAA_afterABC <- convert_faa_perSPR(res_vpa_update,sel_year=Fsel_afterABC_year,faa_year=NULL,
                                         Fcurrent_MSY=Fmsy_update)
    }        
    cat("Set future F vector after ABC.year as: ")
    FAA_afterABC  %>% round(2) %>% print()
}


HCR.future <- list(Blim    = Blimit_update,
                   Bban    = Bban_update,
                   beta    = beta_default,
                   year.lag= HCR_year_lag)

if(select_waa_in_future==4) waa.fun.set <- TRUE
if(select_waa_in_future%in%c(1,2)) waa.fun.set <- FALSE
if(select_waa_in_future==3) waa.fun.set <- input_MSY$waa.fun


input_future_0.8HCR <- list(
    res0     =res_vpa_update,
    currentF =FAA_preABC,
    multi    =1,
    N        =future_nsim,    
    futureF  =FAA_afterABC,
    nyear    =future_nyear,
    Pope     =switch(is_pope,
                     res_vpa_update$input$Pope,
                     TRUE,
                     FALSE,
                     stop("Set appropriate number (1-3) in is_pope")),
    outtype  ="FULL",
    multi.year=1,
    start.year=future_start_year,
    ABC.year  =future_ABC_year,
    waa.year      =switch(select_waa_in_future,
                          waa_year_in_future,
                          NULL,
                          input_MSY$waa.year,
                          stop("Set appropriate number (1-3) in select_waa_in_future")),
    waa.catch.year=switch(select_waa.catch_in_future,
                          waa.catch_year_in_future,
                          NULL,
                          input_MSY$waa.catch.year,
                          stop("Set appropriate number (1-3) in select_waa.catch_in_future")),
    maa.year      =switch(select_maa_in_future,
                          maa_year_in_future,
                          NULL,
                          input_MSY$maa.year,                          
                          stop("Set appropriate number (1-3) in select_maa_in_future")),
    M.year        =switch(select_M_in_future,
                          M_year_in_future, # case 1
                          NULL,             # case 2
                          input_MSY$M.year, # case 3
                          stop("Set appropriate number (1-3) in select_M_in_future")),            
    waa           =switch(select_waa_in_future,
                          NULL,              # case 1
                          waa_in_future,     # case 2
                          input_MSY$waa,     # case 3                     
                          stop("Set appropriate number (1-3) in select_waa_in_future")),
    waa.catch     =switch(select_waa.catch_in_future,
                          NULL,                          
                          waa.catch_in_future,
                          input_MSY$waa.catch,                          
                          stop("Set appropriate number (1-3) in select_waa.catch_in_future")),            
    maa           =switch(select_maa_in_future,
                          NULL,
                          maa_in_future,
                          input_MSY$maa,
                          stop("Set appropriate number (1-3) in select_maa_in_future")),    
    M             =switch(select_M_in_future,
                          NULL,
                          M_in_future,
                          input_MSY$M,                          
                          stop("Set appropriate number (1-3) in select_M_in_future")),
    seed       = future_seed,
    strategy   = "F", 
    HCR        = HCR.future,
    use.MSE    = ifelse(do_MSE==0,FALSE,TRUE),
    MSE.options= (if(do_MSE==0) NULL else opt_MSE),
    beta       = NULL,
    delta      = NULL,
    Blim       = 0,
    Bban       = 0,
    plus.group = res_vpa_update$input$plus.group,
    silent     = TRUE,
    is.plot    = future_est_plot, 
    random.select=NULL, 
    recfunc    = SRfun_future, 
    rec.arg    = opt_SR_future,
    rec.new    = (if(select_specific_recruit==0) NULL else list(year=recruit_year,rec=recruit_number)),
    pre.catch  = (if(select_specific_catch==0) NULL else list(year=catch_year,wcatch=catch_weight)),
    waa.fun    = waa.fun.set,
    det.run    = as.logical(det_run),    
    naa0=NULL,eaa0=NULL,ssb0=NULL,faa0=NULL,
    add.year=0)

# Default HCR Run
if(do_MSE==1){
cat("## --------------------------------------------------------\n")
cat(str_c("## Condut simple MSE calc (N=",MSE_nsim,").... Please wait...  \n"))
cat("## --------------------------------------------------------\n")
}

res_future_0.8HCR <- do.call(future.vpa,input_future_0.8HCR)

# current F run
input_future_current <- input_future_0.8HCR
input_future_current$futureF <- NULL
input_future_current$HCR     <- NULL
res_future_current <- do.call(future.vpa,input_future_current)

plot_futures(res_vpa_update,list(res_future_0.8HCR,res_future_current))

# kobe II table
kobeII.data <- beta.simulation(input_future_0.8HCR,
                               beta_vector=beta_table,
                               year.lag=HCR_year_lag)
kobeII.table <- make_kobeII_table(kobeII.data,
                                  res_vpa        = res_vpa_update,
                                  year.catch     = year_catch_average,
                                  year.ssb       = year_ssb_average,
                                  year.Fsakugen  = year_Fsakugen_mean,
                                  year.ssbtarget = year_ssbtarget_prob,
                                  year.ssblimit  = year_ssblimit_prob,
                                  year.ssbban    = year_ssbban_prob,
                                  year.ssbmin    = year_ssbmin_prob,
                                  year.ssbmax    = year_ssbmax_prob,                                                        year.aav       = year_catch_aav,
                                  Btarget=Btarget_update,
                                  Blimit =Blimit_update,
                                  Bban   =Bban_update
                                  )

cat("## --------------------------------------------------------\n")
cat("## print setting for future simulations (with current F) \n")
cat("## --------------------------------------------------------\n")
print(tibble(age                =dimnames(res_future_current$naa)$age,
             currentF           =res_future_current$currentF,
             futureF            =res_future_current$futureF,
             maturity_init_year =res_future_current$maa[,1,1],
             bweight_init_year  =res_future_current$waa[,1,1],
             cweight_init_year  =res_future_current$waa.catch[,1,1],
             matural_mortality  =res_future_current$M[,1,1]))
cat("## --------------------------------------------------------\n")

cat("## --------------------------------------------------------\n")
cat("## print setting for future simulations (with HCR) \n")
cat("## --------------------------------------------------------\n")
print(tibble(age                =dimnames(res_future_0.8HCR$naa)$age,
             currentF           =res_future_0.8HCR$currentF,
             futureF            =res_future_0.8HCR$futureF,
             maturity_init_year =res_future_0.8HCR$maa[,1,1],
             bweight_init_year  =res_future_0.8HCR$waa[,1,1],
             cweight_init_year  =res_future_0.8HCR$waa.catch[,1,1],
             matural_mortality  =res_future_0.8HCR$M[,1,1]))
cat("## --------------------------------------------------------\n")

cat("## print results of future probabilities under given HCR ------------\n")
cat("## --------------------------------------------------------\n")
print(kobeII.table)
cat("## --------------------------------------------------------\n")

# save results
cat("\n***** Summary results *****\n")
save(res_future_0.8HCR,res_future_current,kobeII.table,file=future_file_path)
cat(paste("create output file of",future_file_path,": Future simualtion results\n"))

#### plot results
# kobe plot
# SPR.msyを目標としたとき、それぞれのF at age by yearを何倍すればSPR.msyを達成できるか計算
SPR.history <- get.SPR(res_vpa_update,
                       target.SPR=SPR_MSY_update*100,
                       max.age=Inf,Fmax=1)$ysdata
Fratio <- SPR.history$"F/Ftarget"
Bratio <- colSums(res_vpa_update$ssb)/derive_RP_value(res_MSY$summary,"Btarget0")$SSB

cat("## --------------------------------------------------------\n")
cat("## Historical F/Fmsy & B/Bmsy values ------------\n")
cat("## --------------------------------------------------------\n")
kobe.ratio <- tibble(Bratio=Bratio,Fratio=Fratio) %>%
    mutate(Bratio=round(Bratio,2),Fratio=round(Fratio,2)) %>%
    print()
cat("## --------------------------------------------------------\n")

# SPRの時系列と目標SPR
#plot(SPR.history$perSPR,ylim=c(0,max(c(SPR.history$perSPR,SPR.msy))),type="b")
#abline(h=SPR.msy,lty=2)

theme_SH <- function(){
    theme_bw(base_size=12) +
    theme(panel.grid = element_blank(),
          axis.text.x=element_text(size=11,color="black"),
          axis.text.y=element_text(size=11,color="black"),
          axis.line.x=element_line(size= 0.3528),
          axis.line.y=element_line(size= 0.3528),
          legend.position="none")
}

g3_kobe4 <- plot_kobe_gg(res_vpa_update,
                           refs_base=res_MSY$summary,
                           roll_mean=1,category=4,
                           Btarget="Btarget0",
                           Blow="Btarget0",                           
                           beta=0.8, # 推奨されるβに変える
                           refs.color=c(1,1,1),
                           yscale=1.2, # y軸を最大値の何倍まで表示するか。ラベルの重なり具合を見ながら調整してください
                           HCR.label.position=c(1,1),# HCRの説明を書くラベルの位置。相対値なので位置を見ながら調整してください。
                            ylab.type="F",Fratio=Fratio)+theme_SH()
#ggsave_SH("g3_kobe4.png",g3_kobe4)


# plot future projection
(g4_future <- plot_futures(res_vpa_update, #vpaの結果
                   list(res_future_0.8HCR,res_future_current), # 将来予測結果
                   future.name=c(str_c(beta_default,"HCR"),"Fcurrent"),
                   CI_range=c(0.05,0.95),
                   maxyear=2045,
                   ncol=2, # 図の出力の列数。3行x1列ならncol=1
                   what.plot=c("biomass","SSB","catch"),
                   Btarget=derive_RP_value(res_MSY$summary,"Btarget0")$SSB,
                   Blimit=derive_RP_value(res_MSY$summary,"Blimit0")$SSB,
                   Bban=derive_RP_value(res_MSY$summary,"Bban0")$SSB,
                   RP_name=c("目標管理基準値","限界管理基準値","禁漁水準"),
                   biomass.unit=1000,  # バイオマスの単位(100, 1000, or 10000トン)
                   n_example=5,seed=2,
                   example_width=0.3)+ # どのシミュレーションをピックアップするかはseedの値を変えて調整してください
    theme_SH()+
    theme(legend.position="right")+
    scale_color_hue(labels=c(VPA="過去の推定値",s1="現状の漁獲圧",s2="漁獲管理規則\n(β=0.8)"))+
    scale_fill_hue(labels=c(VPA="過去の推定値",s1="現状の漁獲圧",s2="漁獲管理規則\n(β=0.8)"))+
    scale_linetype_discrete(guide=FALSE)
)
#ggsave_SH_large("g4_future.png",g4_future)

graph_all2 <- gridExtra::grid.arrange(g3_kobe4,g4_future)
ggsave(graph_file_future,graph_all2)

out.vpa(res=res_vpa_update,
        msyres=res_MSY,
        fres_current=res_future_current,
        fres_HCR=res_future_0.8HCR,
        kobeII=kobeII.table,
        csvname=csv_file_future,pdfname=pdf_file_future)

options(warn=old.warning)
options(tibble.width=NULL,tibble.print_max=20)
