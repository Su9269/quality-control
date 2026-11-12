# X-bar 管制圖 Power Curve 分析

品質管制課程作業。比較 X̄ 管制圖在常態分配、平均偏移 δσ 時的偵測能力。

## 內容

- **Part 1：只用規則 1**（點超出 ±3σ）
  Power = 1 − [Φ(3 − δ√n) − Φ(−3 − δ√n)]
- **Part 2：規則 1 + zone rule**（連續 3 點中至少 2 點落在同側 A 區）
  以 Markov chain（7 個狀態）求 ARL，等效 α = 1/ARL₀、Power = 1/ARL₁

參數：n = 5, 10, 25；δ = 0.5, 1, 1.5, 2, 2.5, 3

## 主要結果

| | ARL₀ | α |
|---|---|---|
| 只用規則 1 | 370.4 | 0.0027 |
| 規則 1 + zone rule | 225.4 | 0.0044 |

zone rule 對小幅偏移幫助最大（n = 5、δ = 0.5 時 power 約為 2.2 倍），代價是誤警報率增加約 64%。

## 執行

```r
source("xbar_power_curve.R")
```

## 參考

Champ, C. W., & Woodall, W. H. (1987). Exact results for Shewhart control charts with supplementary runs rules. *Technometrics*, 29(4), 393–399.
