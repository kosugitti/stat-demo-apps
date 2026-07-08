# 教材用デモアプリ集（stat-demo-apps）

心理統計の講義（心理学データ解析基礎ほか）で使う，推定・検定の考え方を体感するための
インタラクティブなデモアプリ集です。R / Shiny で書かれています。

作者: 小杉考司（専修大学）

## 収録アプリ

| ディレクトリ | 内容 | 状態 |
|---|---|---|
| `mean-ci/` | 母平均の区間推定（点推定・区間推定・不偏分散・標準誤差・信頼区間の解釈・t分布） | 公開中 |
| `two-group/` | 2群の平均値差の推定と t 検定（効果量とσから2群母集団→標本→差の区間推定→t検定） | 公開中 |
| `cor-test/` | 相関係数の検定（母相関ρから標本→散布図→t検定，t分布の棄却域と実現値を図示） | 公開中 |
| `bayes/` | ベイズ推定 | 予定 |

## ライブデモ

学科の計算機サーバ（Shiny Server）で公開しています。

- 母平均の区間推定: https://sv1.psy.senshu-u.ac.jp/kosugi-demo/mean-ci/
- 2群の平均値差の推定と t 検定: https://sv1.psy.senshu-u.ac.jp/kosugi-demo/two-group/
- 相関係数の検定: https://sv1.psy.senshu-u.ac.jp/kosugi-demo/cor-test/

## 手元で動かす

R に次のコードをコピペするだけで，GitHub からソースを取ってきて手元で起動できます
（初回のみ `install.packages(c("shiny", "ggplot2", "pacman"))` が必要です）。

```r
shiny::runGitHub("stat-demo-apps", "kosugitti", subdir = "mean-ci")    # 母平均の区間推定
shiny::runGitHub("stat-demo-apps", "kosugitti", subdir = "two-group")  # 2群の平均値差と t 検定
shiny::runGitHub("stat-demo-apps", "kosugitti", subdir = "cor-test")   # 相関係数の検定
```

リポジトリを clone した場合は `shiny::runApp("mean-ci")` のようにディレクトリ指定でも起動できます。

## ライセンス

MIT License（`LICENSE` を参照）。授業で自由に使い，改変・再配布して構いません。
