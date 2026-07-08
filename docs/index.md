---
title: 教材用デモアプリ集
---

# 教材用デモアプリ集

心理統計の講義で使う，推定・検定の考え方を体感するためのインタラクティブなデモアプリ集です。
R / Shiny で書かれており，学科の計算機サーバ（Shiny Server）で公開しています。

作者: 小杉考司（専修大学）

## アプリ一覧

### 母平均の区間推定

母数（母平均 μ・母標準偏差 σ）を設定して母集団を描き，そこから標本をとって母平均を
区間推定する様子を可視化します。点推定と区間推定の違い，不偏分散，標準誤差，信頼区間の
解釈，そして母分散が未知のときに使う t 分布までを，サンプルサイズを変えながら体感できます。
既知（正規分布）・未知（t 分布）・両方の重ね比較の3モードに対応しています。

- ライブデモ: <https://sv1.psy.senshu-u.ac.jp/kosugi-demo/mean-ci/>
- コード: [`mean-ci/`](https://github.com/kosugitti/stat-demo-apps/tree/main/mean-ci)

### 2群の平均値差の推定と t 検定

効果量（Cohen's d）と母標準偏差から2群の母集団を描き，各群から標本をとって
平均値差を区間推定し，t 検定（Welch / Student を選択）の結果を表示します。
差の95%信頼区間が0を含むかどうかで判断する様子を可視化します（ch12§4〜ch13）。

- ライブデモ: <https://sv1.psy.senshu-u.ac.jp/kosugi-demo/two-group/>
- コード: [`two-group/`](https://github.com/kosugitti/stat-demo-apps/tree/main/two-group)

### 相関係数の検定

母相関 ρ の世界から標本をとり，標本相関係数 r を t 統計量に変換して無相関の
帰無仮説を検定します。散布図と，自由度 n−2 の t 分布・棄却域・実現値を並べて
示します（ch13 §相関係数の検定）。

- ライブデモ: <https://sv1.psy.senshu-u.ac.jp/kosugi-demo/cor-test/>
- コード: [`cor-test/`](https://github.com/kosugitti/stat-demo-apps/tree/main/cor-test)

### 今後追加予定

- ベイズ推定（ベイズの回で追加予定）

## 手元で動かす

R と `shiny` / `ggplot2` / `pacman` があれば，各アプリのディレクトリを指定して起動できます。

```r
shiny::runApp("mean-ci")
```

## ライセンス

MIT License。授業で自由に使い，改変・再配布して構いません。

リポジトリ: <https://github.com/kosugitti/stat-demo-apps>
