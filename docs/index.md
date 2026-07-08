---
title: 教材用デモアプリ集
---

# 教材用デモアプリ集

心理統計の講義で使う，推定・検定の考え方を体感するためのインタラクティブなデモアプリ集です。
R / Shiny で書かれており，学科の計算機サーバ（Shiny Server）で公開しています。

作者: 小杉考司（専修大学）

各アプリは，下のコードを R にコピペするだけで GitHub からソースを取ってきて手元で動きます
（初回のみ `install.packages(c("shiny", "ggplot2", "pacman"))` が必要です）。

## アプリ一覧

### 母平均の区間推定

母数（母平均 μ・母標準偏差 σ）を設定して母集団を描き，そこから標本をとって母平均を
区間推定する様子を可視化します。点推定と区間推定の違い，不偏分散，標準誤差，信頼区間の
解釈，そして母分散が未知のときに使う t 分布までを，サンプルサイズを変えながら体感できます。
既知（正規分布）・未知（t 分布）・両方の重ね比較の3モードに対応しています。

- ライブデモ: <https://sv1.psy.senshu-u.ac.jp/kosugi-demo/mean-ci/>
- コード: [`mean-ci/`](https://github.com/kosugitti/stat-demo-apps/tree/main/mean-ci)

```r
shiny::runGitHub("stat-demo-apps", "kosugitti", subdir = "mean-ci")
```

### 2群の平均値差の推定と t 検定

効果量（Cohen's d）と母標準偏差から2群の母集団を描き，各群から標本をとって
各群の平均を区間推定し，平均値差について t 検定（Welch / Student を選択）を行います。
下段は「差の分布（区間推定）」と「帰無分布（t 検定：棄却域・臨界値・実現値）」を
切り替えて示します（ch12§4〜ch13）。

- ライブデモ: <https://sv1.psy.senshu-u.ac.jp/kosugi-demo/two-group/>
- コード: [`two-group/`](https://github.com/kosugitti/stat-demo-apps/tree/main/two-group)

```r
shiny::runGitHub("stat-demo-apps", "kosugitti", subdir = "two-group")
```

### 相関係数の検定

母集団を点群（散布の雲）として描き，そこから標本をとって標本相関係数 r を得ます。
母集団は「帰無分布（ρ=0）」も選べ，無相関の世界からとった標本でも r が0にならず
ばらつくことを確かめられます。r を t 統計量に変換して無相関の帰無仮説を検定し，
散布図と，自由度 n−2 の t 分布・棄却域・実現値を並べて示します（ch13 §相関係数の検定）。

- ライブデモ: <https://sv1.psy.senshu-u.ac.jp/kosugi-demo/cor-test/>
- コード: [`cor-test/`](https://github.com/kosugitti/stat-demo-apps/tree/main/cor-test)

```r
shiny::runGitHub("stat-demo-apps", "kosugitti", subdir = "cor-test")
```

### 今後追加予定

- ベイズ推定（ベイズの回で追加予定）

## ライセンス

MIT License。授業で自由に使い，改変・再配布して構いません。

リポジトリ: <https://github.com/kosugitti/stat-demo-apps>
