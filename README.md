WIKIに，バグ相談，エラー報告のやり方を解説しました．問題あったかたはまずこちらをご一読し，その後，issueかチームスかどちらかに質問を投げてください． https://github.com/ichimomo/frasyr_tool/wiki/%E3%83%90%E3%82%B0%E5%A0%B1%E5%91%8A%E3%83%BB%E3%82%A8%E3%83%A9%E3%83%BC%E7%9B%B8%E8%AB%87%E3%81%AE%E3%82%84%E3%82%8A%E3%81%8B%E3%81%9F

## ユーティリティファイルの使い方（テスト）

### 0. ファイルの表示設定

ファイルを見るときに、拡張子や隠しファイルも表示される設定に変更してください。Windowsであれば、エクスプローラーの「表示」タブの「ファイル名拡張子」「隠しファイル」にチェックを入れます。
    
### 1. ファイルをフォルダごとダウンロード

このサイトにあるファイルをすべてダウンロードします。右上の「Clone or download」を選択し、「Download ZIP」を選んでください。フォルダ全体がzipファイルでダウンロードできます。ダウンロードしたファイルは展開して適当な場所においてください。

### 2. ファイルの確認

展開したフォルダには以下のファイルが入っています

- .RData : このファイルをクリックするとRが立ち上がります。
- 0do_all.r : ライブラリをインストールしたり、走らせるRプログラムを選択したりする上位のプログラムです  
- 1do_MSYest.r : VPA結果に再生産関係をあてはめ、MSY推定をするところまでおこなうプログラムです。主にこの各種設定を変更します。
- 2do_future_projection.r : MSY管理基準値をもとに将来予測を実施したり、kobe matrixを計算したりするプログラムです。主にこの各種設定を変更します
- res_vpa.rda : VPA結果のサンプルデータです

### 3.   Rの立ち上げと必要ライブラリのインストール

- .RDataをダブルクリックするか、0do_all.rをRstduioで開くかしてRを立ち上げます
- 必要なライブラリ(devtools, tidyverse, frasyr)をインストールします。 インストール方法は0do_all.rにコメントアウトされた部分にかかれています。毎回インストールする必要はありませんが、frasyrの更新頻度は高いので、うまく行かない場合はfrasyrを再インストールし、Rを再起動してからまた試してください。

### 4. 計算条件の設定
    
- 1do_MSYest.rと2do_future_projection.rを編集し、計算条件の設定をします。
- example中には例としてres_vpa.rda(VPA結果のRオブジェクト)とres_vpa.csv(VPA結果のcsvファイル)も入っています。csvファイルから読み込む場合はres_vpa.csvの書式を参考にしてください。

### 4. コードの実行
    
設定が終わったら、0do_all.rの中身を一行づつ走らせてください。0do_all.rのサンプルは4行だけのコードですが、自分の好きなように組み合わせて改良してください。

```
# 0do_all.rの中身(コメントアウト部分を除く）
library(tidyverse)
library(frasyr)
source("1do_MSYest.r") # MSY管理基準値の計算
source("2do_future_projection.r") # 将来予測の実施
```

### 4. コードの拡張

研究機関会議では、1do_MSYest.rと2do_future_projection.rの２つのファイルを使って作業することになります。研究機関会議後、データを１年追加した新しいVPA結果で将来予測を実施する場合には2do_future_projection.rを3do_future_projection.rとかいう名前でコピーして、3つめのファイルをそのときの設定にあわせて編集し、実行し、将来予測結果を得ます。資源評価結果が更新されるごとに、do_future_projectionファイルが増えていくようなしくみになります。将来予測の設定の違いなどは、フォルダの違いとして保存しておくと管理が楽だと思います。

	
- 例）資源評価結果が１年更新された場合の0do_all.r
```
library(tidyverse)
library(frasyr)
source("1do_MSYest.r") # MSY管理基準値の計算
source("2do_future_projection.r") # 研究機関会議での将来予測
source("3do_future_projection.r") # 新しいVPA結果をもとにした将来予測
```

- 例) HSとBHで比較したい場合	
```
library(tidyverse)
library(frasyr)
source("1do_MSYest-HS.r") # MSY管理基準値の計算(SRの設定をHSにする)
source("2do_future_projection-HS.r") # 上で作成したファイルをもとに将来予測するように設定しておく

source("1do_MSYest-BH.r") # MSY管理基準値の計算(SRの設定をBHにする)
source("2do_future_projection-BH.r") # 上で作成したファイルをもとに将来予測するように設定しておく

```

## 実際に自分の魚種の資源評価をおこなう場合
- VPA結果をcsv出力したものか，Rオブジェクトにしたものを用意し，それをexampleフォルダのres_vpa.csvやres_vpa.rdaなどと置き換えてください．または設定ファイルの読み込みファイル名を魚種にあわせて指定してください．
- VPAの実行のしかたはfrasyrのマニュアル　https://ichimomo.github.io/frasyr/doc/vpa.html　にあります
- vpa結果をcsvファイルにする場合
```
out.vpa(vpaのオブジェクト, file="ファイル名”)
```

- Rのオブジェクトにする場合
```
save(vpaのオブジェクト, file="ファイル名")
```


