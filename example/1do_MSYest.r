#-- MSY管理基準値を計算する

#-- 1) 読み込みファイルの設定
#--- VPA結果が保存されているファイルの名前
vpa_file_path_MSY <- "res_vpa.rda"
#--- VPAの結果のファイルの種類（1: Rオブジェクト, 2: csv形式)
vpa_file_type_MSY <- 1
#--- MSY推定で仮定する再生産関係の推定結果を保存するファイルの名前
SR_file_path_MSY <- "res_SR_HSL2.rda"
#--- MSY推定結果を保存するファイルの名前
MSY_file_path <- "res_MSY_HSL2.rda"
#--- グラフのファイル名
graph_file_MSY <- "MSY_graph.png"
#--- 結果をcsvファイルで出力するときのファイル名
csv_file_MSY <- "future0.csv"
#--- 結果の簡単なグラフをpdfファイルで出力するときのファイル名(まだちゃんとした結果は出ないです)
pdf_file_MSY <- "future0.pdf"
#--- 実行したときの警告を表示するかどうか (-1: 表示しない, 0: 表示する)
warning_option <- -1

#-- 2) 再生産関係のフィット
#--- 再生産関係をあてはめる年の範囲 0: 全部の年のデータを使う, マイナスの数字: 指定された年数分、最近年データを除く, 正の数字: 指定された数字に一致する年のデータを用いる
year_for_srfit <- -1
#--- 再生産関係の式 (1: HS, 2: BH, 3: RI)
function_srfit <- 1
#--- あてはめの方法 (1: 最小絶対値, 2: 最小二乗)
L1L2_srfit <- 2
#--- 自己相関の考慮 (0: なし, 1: あり)
AR_srfit <- 0
#--- 自己相関を計算するときに、自己相関を外側で計算する(1)か、尤度に組み込んで内側で計算する(2)か。推奨は外側(1)。
AR_estimation <- 1
#--- 再生産関係のフィットについて、細かいモデル診断をするかどうか(1:する、0: しない)
do_diagnostics <- 1
#---- 細かいモデル診断をする場合の設定（ファイル名など）
#------- 細かい説明はhttps://ichimomo.github.io/future-rvpa/SRR-guidline.html
if(do_diagnostics==1){
    # モデル診断結果を保存するグラフ名
    diagnostic_file <- "diagnostic.pdf"
    # ブートストラップ推定する場合のブートストラップ回数
    n_boot_SR <- 10
    
}

#-- 3) MSY推定の設定（F一定の条件下での将来予測をもとにする）
#-- MSY推定をするかどうか（1: する, 0: しない）
do_MSY_estimation <- 1
#--- MSY推定で用いる選択率（近年のF at age＝Fcurrentにおける選択率がそのまま将来も受け継がれるという仮定）
#---   (1: vpaの結果の中のFc.at.ageをそのまま使う; ややこしいので廃止予定)
#---   2: 手動でFcurrentを設定する
#---   3: vpaのF at ageに対して年を指定し、その平均を使う,
#---   4: 選択率を参照する年と漁獲圧を参照する年を別にする（漁獲圧はSPR換算して、指定された選択率において同じ漁獲圧になるようなFcurrentを用いる。SPR換算するときの生物パラメータは、漁獲圧として指定された年の生物パラメータの平均とする））
select_Fcurrent_MSY <- 4
#---- 上で2を選んだ場合:FcurrentとしたいFをベクトルで入力
if(select_Fcurrent_MSY==2){
    Fcurrent_MSY <- c(0.02302929,0.04498867,0.07500238,
                      0.10139408,0.14474343,0.15539237,
                      0.16375070,0.12544913,0.12544913)
}
#---- 上で3を選んだ場合:Fを平均したい年数
if(select_Fcurrent_MSY==3){
    # 実際の年を指定する場合
    Fcurrent_year_MSY <- 2015:2017
    # マイナス値を入れると最新年から数えて何年分遡るかで指定できる
    #Fc_at_age_year <- -1:-3
}
#---- 上で4を選んだ場合:選択率参照年とFcurrent参照年を分ける
if(select_Fcurrent_MSY==4){
    # 漁獲圧の強さとして近年の漁獲圧を代表とする年(実際の年を指定するか、マイナス値で相対値として指定する）
    # Fcurrent_year_MSY <- 2015:2017
    Fcurrent_year_MSY <- -1:-3
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    #Fsel_year_MSY <- 2010:2017
    Fsel_year_MSY <- -1:-10
}

#---- Rのスクリプトを含むメモを追加する場合は、以下のようにif(0){}で囲った範囲でメモする。
#---- 以下、スケトウ日本海の場合はvpa$Fc.at.ageは2013:2017の単純平均=>選択率計算に利用
#---- MSY計算用として与えるFc_at_ageは2015:2017年の平均F at ageの平均値と上記のF at ageの平均値の比
if(0){
    round(rowMeans(res_vpa_MSY$faa[as.character(2013:2017)]),4)==round(res_vpa_MSY$Fc.at.age,4)
    # 2015-2017のF at ageの平均/
    Fratio <- mean(rowMeans(res_vpa_MSY$faa[as.character(2015:2017)]))/mean(res_vpa_MSY$Fc.at.age)
    Fc_at_age <- res_vpa_MSY$Fc.at.age * Fratio
}

#-- 各種計算方法
#--- 漁獲量の計算方法（1:VPAと同じ設定を使う, 2:Popeの近似式を使う, 3:Bavanovの式を使う）
is_pope <- 1
#--- 乱数のシード
MSY_seed <- 1
#--- MSY計算時のシミュレーション回数(1000回以上推奨)
MSY_nsim <- 10
#--- 計算した結果の簡単な図を示す（1:示す,1以外:しない）
MSY_est_plot <- 1

#-- 生物パラメータ
#--- MSY計算時の年齢別体重(資源量計算用)の設定(1:年で指定する、2:直接指定する、3:年齢別資源尾数と体重の回帰式から毎年の体重を予測する。このオプションを使う場合は、waa.catchも同じ体重を使うことになるので注意)
select_waa_in_MSY <- 3
if(select_waa_in_MSY==1){ # 1の場合にはこちらを設定
    waa_year_in_MSY <- 2013:2017
}
if(select_waa_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    waa_in_MSY <- c(100,200,300,400)
}

#--- MSY計算時の年齢別体重(漁獲量計算用)の設定(0:資源量計算用と同じ、1:年数で指定、2:直接指定)
#---  注) select_waa_in_MSYを３に指定した場合には、以下の設定を1-2にしても、３で上書きされる
select_waa.catch_in_MSY <- 1
if(select_waa.catch_in_MSY==1){ # 1の場合にはこちらを設定
    waa.catch_year_in_MSY <- 2013:2017
}
if(select_waa.catch_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    waa.catch_in_MSY <- c(100,200,300,400)
}

#--- MSY計算時の年齢別成熟率の設定(1:年数で指定、2:直接指定)
select_maa_in_MSY <- 1
if(select_maa_in_MSY==1){ # 1の場合にはこちらを設定
    maa_year_in_MSY <- 2015:2017
}
if(select_maa_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    maa_in_MSY <- c(0,0,0.5,1)
}

#--- MSY計算時の自然死亡係数の設定(1:年数で指定、2:直接指定)
select_M_in_MSY <- 1
if(select_M_in_MSY==1){ # 1の場合にはこちらを設定
    M_year_in_MSY <- 2015:2017
}
if(select_M_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    M_in_MSY <- c(0,0,0.5,1)
}

#-- 再生産関係
#---  バイアス補正（シミュレーションの平均と決定論的予測が一致するようにする）(1:する(デフォルト)、1以外: しない)
bias_correction_MSY <- 1
#--- 加入変動の誤差分布(1: 対数正規誤差, 2: 残差リサンプリング)
SR_error_MSY <- 1
#---- SR_error_MSY=1（対数正規分布の誤差）の場合の設定
if(SR_error_MSY==1){ # 対数正規分布の場合は自己相関のオプションも選ぶ
    #--- 自己相関の仮定 (-1: 推定結果どおりに設定する, 0: 推定結果にかかわらず自己相関を「なし」にする,
    #---               0以上の数字：推定結果にかかわらず自己相関係数をここで設定した値にする)
    select_AR_MSY <- -1
}
#---- SR_error_MSY=2（リサンプリング誤差）の場合の設定
if(SR_error_MSY==2){
    #--- リサンプリングの年の範囲(0: 全年, それ以外の数字: 指定した加入年の残差のみリサンプリング）
    select_resample_year <- 0 # or 1990:2000
}

#-- MSY推定時の設定
#--- 漁獲量曲線を細かく書くか（0: 書かない（計算時間短縮), 1: 書く）
calc_yieldcurve <- 1
#--- 管理基準値を計算するPGYレベル（-1: 計算しない, 0から1までの数字のベクトル: MSYx割合に対応する親魚レベル）
select_PGY <- c(0.95, 0.9, 0.6,0.1)
#--- PGYの下側のみの管理基準値を計算する（1: 下側のみ（計算時間短縮）, 2:両側）
only_lowerPGY <- 1
#--- 管理基準値を計算するB0レベル（-1: 計算しない, 0から1までの数字のベクトル: B0x割合に対応する親魚レベル）
select_B0 <- c(0.2)
#--- 特定の親魚量レベルを直接指定する（-1: 計算しない, 親魚量のベクトル: その親魚量に維持するときの管理基準値）
# 過去最低の親魚資源量をvpaの結果から持ってくる(#########オプション検討##########)
select_Babs <- 12200 # 
#--- 平衡状態にあると仮定する年の決め方（1: 世代時間から計算する, 2: 具体的な年数を与える）
select_nyear_MSY <- 1
#---- 世代時間から計算する場合
if(select_nyear_MSY==1){
    #--- 世代時間の推定方法（0: 自動的に計算, 1以上の数：ここで指定された値を世代時間（年）とする）
    select_GT <- 0
    #--- 世代時間の何倍を平衡状態に置くか（デフォルトは20)
    GT_multi <- 20
}
#---- 具体的な年数を与える場合
if(select_nyear_MSY==2){
    nyear_MSY <- 400
}
#--- 複数シミュレーションの結果をあわせて漁獲量を最大化するときの統計量(算定ルールでは1（平均）を使うことになっている) (1: 平均（デフォルト）, 2: 中央値)
stat_maximize <- 1
#--- 自己相関を考慮した管理基準値を計算するか (0:しない, 1:する)
calc_RP_with_AR <- 0
#---- 自己相関を考慮した管理基準値を計算する場合(ARありの場合だけ有効)
if(calc_RP_with_AR==1){
    #--- 何年分の残差の平均から平衡状態からスタートさせるか
    AR_average_year_MSY <- 5
    #--- 平衡状態から何年進めたときの値を管理基準値とするか（デフォルトは5年）
    forward_year_ARRP <- 5
    #--- 残差を手動で設定する場合
    current.resid <- 0
}
#--- 目標管理基準値の選択 (0: MSY,
#---                   1以上の数字: MSY_res$summaryの行数,
#---                   負の数字: インタラクティブに決定)
select_Btarget <- 0
#--- 限界管理基準値の選択 (0: 60%MSY,
#---                   1以上の数字: MSY_res$summaryの行数,
#---                   負の数字: インタラクティブに決定)
select_Blimit  <- 0
#--- 禁漁水準の選択      (0: 10%MSY,
#---                   1以上の数字: MSY_res$summaryの行数,
#---                   負の数字: インタラクティブに決定)
select_Bban  <- 0


####################################################
### 以下は基本的には編集しないこと
####################################################
old.warning <- options()$warn
options(warn=warning_option)
options(tibble.width=100)

# 1) VPA結果の読み込み
if(vpa_file_type_MSY==1){
    res_vpa_name <- load(vpa_file_path_MSY)
    res_vpa_MSY <- get(res_vpa_name)
}
if(vpa_file_type_MSY==2){
    res_vpa_MSY <- read.vpa(vpa_file_path_MSY)
}

# 2) SR関係のフィット
if(year_for_srfit<0) year_for_srfit <- rev(as.numeric(colnames(res_vpa_MSY$naa)))[-1:year_for_srfit]
if(year_for_srfit==0) year_for_srfit <- as.numeric(colnames(res_vpa_MSY$naa))
data_SR <- get.SRdata(res_vpa_MSY,years=year_for_srfit)
res_SR_MSY <- fit.SR(data_SR,
                SR      = switch(function_srfit,
                                "HS",
                                "BH",
                                "RI",
                                ("Set appropriate number (1-3) in function_srfit")),
                method  = switch(L1L2_srfit,
                                "L1",
                                "L2",
                                ("Set appropriate number (1-2) in function_L1L2_srfit")),
                AR      = AR_srfit,
                out.AR  = switch(AR_estimation,
                                 TRUE,
                                 FALSE,
                                 "Set appropriate number (1-2) in AR_estimation"),
                hessian = FALSE)

## print results of SR fit
cat("## --------------------------------------------------------\n")
cat("## print estimated SR parameters\n")
cat("## --------------------------------------------------------\n")
as_tibble(res_SR_MSY$pars) %>% mutate(AICc   = res_SR_MSY$AICc,
                                      method = res_SR_MSY$input$method,
                                      type   = res_SR_MSY$input$SR) %>%
    print()
cat("## --------------------------------------------------------\n")

# 2-1) 再生産関係のdiagnosticをする場合
if(do_diagnostics==1){
    pdf(diagnostic_file)
    par(mfrow=c(2,2),mar=c(4,4,3,2))    
    # 正規性のチェック
    check1 <- shapiro.test(res_SR_MSY$resid)
    check2 <- ks.test(res_SR_MSY$resid,y="pnorm")

    hist(res_SR_MSY$resid,xlab="Residuals",main="Normality test",freq=FALSE)
    X <- seq(min(res_SR_MSY$resid)*1.3,max(res_SR_MSY$resid)*1.3,length=200)
    points(X,dnorm(X,0,res_SR_MSY$pars$sd),col=2,lwd=3,type="l")
    mtext(text=" P value",adj=1,line=-1,lwd=2,font=2)
    mtext(text=sprintf(" SW: %1.3f",check1$p.value),adj=1,line=-2)
    mtext(text=sprintf(" KS: %1.3f",check2$p.value),adj=1,line=-3)
    
    qqnorm(res_SR_MSY$resid2,cex=2)
    qqline(res_SR_MSY$resid2,lwd=3)

    # 残差のトレンドと自己相関
    plot(data_SR$year, res_SR_MSY$resid2,pch=16,main="",xlab="Year",ylab="Residual")
    title("Time series of residuals")
    abline(0,0,lty=2)
    par(new=T)
    scatter.smooth(data_SR$year, res_SR_MSY$resid2, lpars=list(col="red", lwd=2),ann=F,axes=FALSE)
    ac.res <- acf(res_SR_MSY$resid2,plot=FALSE)
    plot(ac.res,main="",lwd=3)
    title("Auto correlation function (rho vs. lag)")    

    # ブートストラップ
    boot.res <- boot.SR(res_SR_MSY, n=n_boot_SR)
    hist(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$a),xlab="",ylab="",main="a in bootstrap")
    abline(v=res_SR_MSY$pars$a,col=2,lwd=3)
    abline(v=median(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$a)),col=3,lwd=3,lty=2)
    arrows(quantile(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$a),0.1),0,
           quantile(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$a),0.9),0,
           col=4,lwd=3,code=3)
    legend("topright",
           legend=c("Estimate","Median","CI(0.8)"),lty=1:2,col=2:4,lwd=2,ncol=1,cex=1)

    hist(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$b),xlab="",ylab="",main="b in bootstrap")
    abline(v=res_SR_MSY$pars$b,col=2,lwd=3)
    abline(v=median(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$b)),col=3,lwd=3,lty=2)
    arrows(quantile(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$b),0.1),0,
           quantile(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$b),0.9),0,
           col=4,lwd=3,code=3)

    hist(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$sd),xlab="",ylab="",main="sd in bootstrap")
    abline(v=res_SR_MSY$pars$sd,col=2,lwd=3)
    abline(v=median(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$sd)),col=3,lwd=3,lty=2)
    arrows(quantile(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$sd),0.1),0,
           quantile(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$sd),0.9),0,
           col=4,lwd=3,code=3)

    if (res_SR_MSY$input$AR==1) {
        hist(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$rho),xlab="",ylab="",main="rho")
        abline(v=res_SR_MSY$pars$rho,col=2,lwd=3)
        abline(v=median(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$rho)),col=3,lwd=3,lty=2)
        arrows(quantile(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$rho),0.1),0,
               quantile(sapply(1:length(boot.res), function(i) boot.res[[i]]$pars$rho),0.9),0,
               col=4,lwd=3,code=3)
    }

    # SR function
    plot(data_SR$R ~ data_SR$SSB, cex=2, type = "b",xlab="SSB",ylab="R",
         main="Bootstrapped SR functions",ylim=c(0,max(data_SR$R)*1.3))
    points(rev(data_SR$SSB)[1],rev(data_SR$R)[1],col=1,type="p",lwd=3,pch=16,cex=2)
    for (i in 1:length(boot.res)) {
        points(boot.res[[i]]$pred$SSB,boot.res[[i]]$pred$R,type="l",lwd=2,col=rgb(0,0,1,alpha=0.1))
    }
    points(res_SR_MSY$pred$SSB,res_SR_MSY$pred$R,col=2,type="l",lwd=3)

    # ジャックナイフ
    jack.res <- lapply(1:length(data_SR$year), function(i){
        jack <- res_SR_MSY
        jack$input$w[i] <- 0
        do.call(fit.SR,jack$input)
    })

    par(mfrow=c(2,2),mar=c(4,4,2,2))
    plot(data_SR$year,sapply(1:length(data_SR$year), function(i) jack.res[[i]]$pars$a),type="b",
         xlab="Removed year",ylab="",main="a in jackknife",pch=19)
    abline(res_SR_MSY$pars$a,0,lwd=3,col=2)

    plot(data_SR$year,sapply(1:length(data_SR$year), function(i) jack.res[[i]]$pars$b),type="b",
         xlab="Removed year",ylab="",main="b in jackknife",pch=19)
    abline(res_SR_MSY$pars$b,0,lwd=3,col=2)

    plot(data_SR$year,sapply(1:length(data_SR$year), function(i) jack.res[[i]]$pars$sd),type="b",
         xlab="Removed year",ylab="",main="sd in jackknife",pch=19)
    abline(res_SR_MSY$pars$sd,0,lwd=3,col=2)

    if (res_SR_MSY$input$AR==1){
        plot(data_SR$year,sapply(1:length(data_SR$year), function(i) jack.res[[i]]$pars$rho),type="b",
             xlab="Removed year",ylab="",main="rho in jackknife",pch=19)
        abline(res_SR_MSY$pars$rho,0,lwd=3,col=2)
    }
    dev.off()
}

# 3) MSY推定のための将来予測の設定

vpa_years <- colnames(res_vpa_MSY$naa)
future_MSY_year <- vpa_years[apply(res_vpa_MSY$input$dat$caa,2,sum)>0] %>%
    as.numeric() %>% max()

if(SR_error_MSY==1){# lognormal
    if(res_SR_MSY$input$SR=="HS") SRfun_MSY <- HS.recAR
    if(res_SR_MSY$input$SR=="BH") SRfun_MSY <- BH.recAR
    if(res_SR_MSY$input$SR=="RI") SRfun_MSY <- RI.recAR

    rho_MSY <- switch(as.character(select_AR_MSY),
                      "-1"=res_SR_MSY$pars$rho,
                      "0"=0,
                      select_AR_MSY)
    opt_SR_MSY <- list(a=res_SR_MSY$pars$a,
                       b=res_SR_MSY$pars$b,
                       rho=rho_MSY,
                       sd=res_SR_MSY$pars$sd,
                       bias.correction=ifelse(bias_correction_MSY==1,TRUE,FALSE),
                       resample=FALSE,resid=res_SR_MSY$resid,
                       resid.year=NULL)
    if(rho_MSY>0 && calc_RP_with_AR==1){
        opt_SR_MSY$resid.year <- AR_average_year_MSY
    }
    if(rho_MSY>0 && calc_RP_with_AR==0){
        opt_SR_MSY$resid.year <- 1
    }        
}

if(SR_error_MSY==2){# lognormal
    SRfun_MSY <- resample.rec
    select_resample_year <- ifelse(select_resample_year==0,
                                    TRUE,
                                    select_resample_year)
    resid_selected <- res_SR_MSY$resid[res_SR_MSY$input$SRdata$year%in%
                                       select_resample_year]
    opt_SR_MSY <- list(resample=TRUE,
                       SR=res_SR_MSY$input$SR,
                       resid=resid_selected)
}

opt_SR_MSY$bias.correction <- ifelse(bias_correction_MSY==1,TRUE,FALSE)

if(select_Fcurrent_MSY==1) Fcurrent_MSY <- res_vpa_MSY$Fc.at.age
if(select_Fcurrent_MSY==3){
    Fcurrent_MSY <- apply_year_colum(res_vpa_MSY$faa,target_year=Fcurrent_year_MSY)    
}
if(select_Fcurrent_MSY==4){
    Fcurrent_MSY <- convert_faa_perSPR(res_vpa_MSY,sel_year=Fsel_year_MSY,faa_year=Fcurrent_year_MSY)
}

input_future_MSY <- list(
    res0     =res_vpa_MSY,
    currentF =Fcurrent_MSY,
    multi    =1,
    N        =10,
    futureF  =NULL,
    nyear    =10,
    Pope     =switch(is_pope,
                     res_vpa_MSY$input$Pope,
                     TRUE,
                     FALSE,
                     stop("Set appropriate number (1-3) in is_pope")),
    outtype  ="FULL",
    multi.year=1,
    start.year=future_MSY_year+1,
    ABC.year  =future_MSY_year+1,
    waa.year      =switch(select_waa_in_MSY,
                          waa_year_in_MSY,
                          NULL,
                          NULL,
                          stop("Set appropriate number (1-3) in select_waa_in_MSY")),
    waa.catch.year=switch(select_waa.catch_in_MSY,
                          waa.catch_year_in_MSY,
                          NULL,
                          stop("Set appropriate number (1-3) in select_waa.catch_in_MSY")),
    maa.year      =switch(select_maa_in_MSY,
                          maa_year_in_MSY,
                          NULL,
                          stop("Set appropriate number (1-2) in select_maa_in_MSY")),
    M.year        =switch(select_M_in_MSY,
                          M_year_in_MSY,
                          NULL,
                          stop("Set appropriate number (1-2) in select_M_in_MSY")),
    waa           =switch(select_waa_in_MSY,
                          NULL,
                          waa_in_MSY,
                          NULL,
                          stop("Set appropriate number (1-2) in select_waa_in_MSY")),
    waa.catch     =switch(select_waa.catch_in_MSY,
                          NULL,
                          waa.catch_in_MSY,
                          NULL,
                          stop("Set appropriate number (1-2) in select_waa.catch_in_MSY")),
    maa           =switch(select_maa_in_MSY,
                          NULL,
                          maa_in_MSY,
                          stop("Set appropriate number (1-2) in select_maa_in_MSY")),
    M             =switch(select_M_in_MSY,
                          NULL,
                          M_in_MSY,
                          stop("Set appropriate number (1-2) in select_M_in_MSY")),
    seed       =MSY_seed,
    strategy   ="F",
    HCR        =NULL,
    use.MSE    =FALSE,
    MSE.options=NULL,
    beta       =NULL,
    delta      =NULL,
    Blim       =0,
    Bban       =0,
    plus.group =res_vpa_MSY$input$plus.group,
    silent     =TRUE,
    is.plot    =FALSE,
    random.select=NULL,
    recfunc    =SRfun_MSY,
    rec.arg    =opt_SR_MSY,
    rec.new    =NULL,
    pre.catch  =NULL,
    waa.fun    =ifelse(select_waa_in_MSY==3,TRUE,FALSE),
    naa0=NULL,eaa0=NULL,ssb0=NULL,faa0=NULL,
    add.year=0, det.run=TRUE)

# test run
res_future_preMSY <- do.call(future.vpa,input_future_MSY)

options(tibble.width=100)

cat("## print setting for MSY estimation as example (1st year, 1st run parameters)------\n")
cat("## --------------------------------------------------------\n")
print(tibble(age                =dimnames(res_future_preMSY$naa)$age,
             currentF           =res_future_preMSY$currentF,
             futureF            =res_future_preMSY$futureF,
             maturity_init_year =res_future_preMSY$maa[,1,1],
             bweight_init_year  =res_future_preMSY$waa[,1,1],
             cweight_init_year  =res_future_preMSY$waa.catch[,1,1],
             matural_mortality  =res_future_preMSY$M[,1,1]))
cat("## --------------------------------------------------------\n")


is.estAR.RP <- as.logical(calc_RP_with_AR) && as.logical(rho_MSY>0)

input_est_MSY <-
    list(vpares         =res_vpa_MSY,
         farg           =input_future_MSY,
         N              =MSY_nsim,
         is.plot        =ifelse(MSY_est_plot==1,TRUE,FALSE),
         calc.yieldcurve=ifelse(calc_yieldcurve==1,TRUE,FALSE),
         onlylower.pgy  =ifelse(only_lowerPGY==1,TRUE,FALSE),
         PGY            =(if(select_PGY[1] <0) NULL else select_PGY),
         B0percent      =(if(select_B0[1]  <0) NULL else select_B0),
         Bempirical     =(if(select_Babs[1]<0) NULL else select_Babs),
         seed           =MSY_seed,
         eyear          =0, # 将来予測の最後のeyear+1年分を平衡状態とする
         nyear          =switch(select_nyear_MSY,
                                NULL,
                                nyear_MSY,
                                stop("Set appropriate number (1-2) in select_nyear_MSY")),
         long.term      =(if(select_nyear_MSY==2) NULL else GT_multi),
         GT             =(if(select_nyear_MSY==2||select_GT==0) NULL else select_GT),
         FUN            =switch(stat_maximize,
                                mean,
                                median,
                                stop("Set appropriate number (1-2) in stat_maximize")),
         optim.method   ="optimize",
         max.target     ="catch.mean", # この設定は意味ない
         trace.multi    =c(seq(from=0,to=0.9,by=0.1),1,seq(from=1.1,to=2,by=0.1),3:5,7,20,100),
         estAR.RP       =is.estAR.RP,
         resid.year     =ifelse(isTRUE(is.estAR.RP),AR_average_year_MSY,0),
         mY             =ifelse(isTRUE(is.estAR.RP),forward_year_ARRP,5),
         current.resid  =NULL
         )

# do estimation
if(do_MSY_estimation==1){
   res_MSY <- do.call(est.MSY,input_est_MSY)

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

Btarget0 <- derive_RP_value(res_MSY$summary,"Btarget0")$SSB
Blimit0  <- derive_RP_value(res_MSY$summary,"Blimit0")$SSB
Bban0    <- derive_RP_value(res_MSY$summary,"Bban0")$SSB
SPR_MSY0 <- derive_RP_value(res_MSY$summary,"Btarget0")$perSPR
Fmsy0 <- res_MSY$Fvector %>%
        slice(which(res_MSY$summary$RP.definition=="Btarget0")) %>%
        as.numeric()

cat("## print estimated RP parameters ------------------------------\n")
cat("## --------------------------------------------------------\n")
print(select(res_MSY$summary,-AR,-Catch.CV))
cat("## --------------------------------------------------------\n")
   options(tibble.width=NULL)
   save(res_MSY,file=MSY_file_path)   
}

# save results
save(res_SR_MSY,file=SR_file_path_MSY)
cat("## results of SR parameters and reference points are saved to",
    SR_file_path_MSY,"and", MSY_file_path,"\n")

theme_SH <- function(){
    theme_bw(base_size=12) +
    theme(panel.grid = element_blank(),
          axis.text.x=element_text(size=11,color="black"),
          axis.text.y=element_text(size=11,color="black"),
          axis.line.x=element_line(size= 0.3528),
          axis.line.y=element_line(size= 0.3528),
          legend.position="none")
}

ggsave_SH <- function(...){
    ggsave(width=150,height=85,dpi=600,units="mm",...)
}

ggsave_SH_large <- function(...){
    ggsave(width=150,height=120,dpi=600,units="mm",...)
}

# plot graph
{if(do_MSY_estimation==1){
    refs <- list(Bmsy  = derive_RP_value(res_MSY$summary,"Btarget0")$SSB,
                 Blimit= derive_RP_value(res_MSY$summary,"Blimit0" )$SSB,
                 Bban  = derive_RP_value(res_MSY$summary,"Bban0"   )$SSB)
}
else{
    refs <- list(Bmsy  = 0,
                 Blimit= 0,
                 Bban  = 0)    
    }}

(g1_SRplot <- SRplot_gg(res_SR_MSY,
                        xscale=1000,xlabel="千トン",
                        yscale=1000,   ylabel="千尾",
                        labeling.year=c(1990,2000,2010,2017),
                        refs=refs,
                        add.info=TRUE) + theme_SH())
#ggsave_SH(MSY_graph_SR_file,g1_SRplot)

## yield curve & kobe chart
if(do_MSY_estimation==1){
refs.plot <- dplyr::filter(res_MSY$summary,RP.definition%in%c("Btarget0","Blimit0","Bban0"))
(g2_yield_curve <- plot_yield(res_MSY$trace,
                              refs.plot,
                              refs.label=c("目標管理基準値","限界管理基準値","禁漁水準"),
                              future=NULL,
                              past=NULL,labeling=FALSE,
                              refs.color=c(1,1,1), # 印刷が出ないので縦線の色は黒にすることに
                              biomass.unit=1000,#資源量の単位
                              AR_select=FALSE,
                              xlim.scale=1,ylim.scale=1.1 # x, y軸の最大値の何倍までプロットするか。ラベルやyield curveの形を見ながら適宜調整してください
                              ) + theme_SH())

# kobe plot
# SPR.msyを目標としたとき、それぞれのF at age by yearを何倍すればSPR.msyを達成できるか計算
SPR.history <- get.SPR(res_vpa_MSY,
                       target.SPR=SPR_MSY0*100,
                       max.age=Inf,Fmax=1)$ysdata
Fratio <- SPR.history$"F/Ftarget"
Bratio <- colSums(res_vpa_MSY$ssb)/derive_RP_value(res_MSY$summary,"Btarget0")$SSB

cat("## --------------------------------------------------------\n")
cat("## Historical F/Fmsy & B/Bmsy values ------------\n")
cat("## --------------------------------------------------------\n")
kobe.ratio <- tibble(Bratio=Bratio,Fratio=Fratio) %>%
    mutate(Bratio=round(Bratio,2),Fratio=round(Fratio,2)) %>%
    print()
cat("## --------------------------------------------------------\n")

g3_kobe4 <- plot_kobe_gg(res_vpa_MSY,
                           refs_base=res_MSY$summary,
                           roll_mean=1,category=4,
                           Btarget="Btarget0",
                           Blow="Btarget0",                           
                           beta=0.8, # 推奨されるβに変える
                           refs.color=c(1,1,1),
                           yscale=1.2, # y軸を最大値の何倍まで表示するか。ラベルの重なり具合を見ながら調整してください
                           HCR.label.position=c(1,1),# HCRの説明を書くラベルの位置。相対値なので位置を見ながら調整してください。
                         ylab.type="F",Fratio=Fratio)+theme_SH()
}

if(do_MSY_estimation==1){
    graph_all <- gridExtra::grid.arrange(g1_SRplot,g2_yield_curve,g3_kobe4)
    ggsave(graph_file_MSY,graph_all)
}
if(do_MSY_estimation==0){
    graph_all <- gridExtra::grid.arrange(g1_SRplot)
    ggsave(graph_file_MSY,graph_all)
}

out.vpa(res=res_vpa_MSY,
        srres=res_SR_MSY,
        msyres=(if(do_MSY_estimation==1) res_MSY else NULL),
        fres_current=NULL,
        fres_HCR=NULL,
        kobeII=NULL,filename=NULL,
        csvname=csv_file_MSY,pdfname=pdf_file_MSY)



options(warn=old.warning)
options(tibble.width=NULL)

