# =============================================================
# X-bar 管制圖 Power Curve 分析
#   Part 1：只用規則 1（點超出 ±3σ 管制界限）
#   Part 2：規則 1 + zone rule（連續 3 點中至少 2 點落在同側 A 區）
# 假設：常態分配，平均偏移 delta 個標準差
# =============================================================

L          <- 3                          # 3σ 管制界限
n_list     <- c(5, 10, 25)               # 樣本大小
delta_list <- c(0.5, 1, 1.5, 2, 2.5, 3)  # 偏移量（σ 的倍數）
cols       <- c("steelblue", "darkorange", "forestgreen")
d_grid     <- seq(0, 3, by = 0.01)       # 畫平滑曲線用


# -------------------------------------------------------------
# Part 1：只用規則 1
# Power = 1 - [Φ(L - δ√n) - Φ(-L - δ√n)]
# -------------------------------------------------------------
xbar_power <- function(delta, n, L = 3) {
  shift <- delta * sqrt(n)                       # 標準化後的中心 δ√n
  beta  <- pnorm(L - shift) - pnorm(-L - shift)  # 落在界限內 = 沒抓到
  1 - beta
}

rule1_table <- outer(delta_list, n_list, xbar_power, L = L)
dimnames(rule1_table) <- list(delta = delta_list, n = n_list)
cat("\n=== 只用規則 1：Power ===\n")
print(round(rule1_table, 4))

cat("\n只用規則 1：alpha =", round(xbar_power(0, 5), 4),
    "，ARL0 =", round(1 / xbar_power(0, 5), 2), "\n")


# -------------------------------------------------------------
# Part 2：規則 1 + zone rule（Markov chain 求 ARL）
# 狀態 = (前一點, 這一點) 的區域，U / L / C
# 去掉 UU、LL（已發訊號），共 7 個狀態
# ARL 滿足 (I - Q) ARL = 1
# -------------------------------------------------------------
arl_zone <- function(delta, n) {
  s  <- delta * sqrt(n)
  pU <- pnorm(3 - s)  - pnorm(2 - s)    # 上 A 區
  pL <- pnorm(-2 - s) - pnorm(-3 - s)   # 下 A 區
  pC <- pnorm(2 - s)  - pnorm(-2 - s)   # 中間 ±2σ

  st <- c("UL", "UC", "LU", "LC", "CU", "CL", "CC")
  Q  <- matrix(0, 7, 7, dimnames = list(st, st))
  Q["UL", "LC"] <- pC
  Q["UC", "CL"] <- pL;  Q["UC", "CC"] <- pC
  Q["LU", "UC"] <- pC
  Q["LC", "CU"] <- pU;  Q["LC", "CC"] <- pC
  Q["CU", "UL"] <- pL;  Q["CU", "UC"] <- pC
  Q["CL", "LU"] <- pU;  Q["CL", "LC"] <- pC
  Q["CC", "CU"] <- pU;  Q["CC", "CL"] <- pL;  Q["CC", "CC"] <- pC

  ARL <- solve(diag(7) - Q, rep(1, 7))
  unname(ARL["CC"])                     # 從 CC 開始
}

zone_power <- function(delta, n) 1 / arl_zone(delta, n)

ARL0  <- arl_zone(0, 5)
alpha <- 1 / ARL0
cat("\n規則 1 + zone rule：alpha =", round(alpha, 5),
    "，ARL0 =", round(ARL0, 2), "\n")

zone_table <- sapply(n_list, function(n) sapply(delta_list, zone_power, n = n))
dimnames(zone_table) <- list(delta = delta_list, n = n_list)
cat("\n=== 規則 1 + zone rule：等效 Power = 1/ARL1 ===\n")
print(round(zone_table, 4))


# -------------------------------------------------------------
# 模擬驗證 ARL0（約需數十秒）
# -------------------------------------------------------------
run_length <- function() {
  z1 <- 0; z2 <- 0; t <- 0
  repeat {
    t <- t + 1
    z <- rnorm(1)
    if (abs(z) > 3) return(t)
    last3 <- c(z2, z1, z)
    if (sum(last3 > 2) >= 2 || sum(last3 < -2) >= 2) return(t)
    z2 <- z1; z1 <- z
  }
}
# set.seed(123)
# rl <- replicate(10000, run_length())
# cat("模擬 ARL0 =", mean(rl), "\n")


# -------------------------------------------------------------
# 圖 1：只用規則 1
# -------------------------------------------------------------
plot(NA, xlim = c(0, 3), ylim = c(0, 1),
     xlab = expression(delta ~ "(shift in units of" ~ sigma * ")"),
     ylab = "Power",
     main = expression("Power curve of " * bar(X) * " chart (Rule 1, L = 3)"))
for (i in seq_along(n_list)) {
  lines(d_grid, xbar_power(d_grid, n_list[i], L), col = cols[i], lwd = 2)
  points(delta_list, xbar_power(delta_list, n_list[i], L), col = cols[i], pch = 19)
}
legend("bottomright", legend = paste("n =", n_list), col = cols, lwd = 2, pch = 19)
grid()


# -------------------------------------------------------------
# 圖 2：規則 1 vs 規則 1 + zone rule
# -------------------------------------------------------------
plot(NA, xlim = c(0, 3), ylim = c(0, 1),
     xlab = expression(delta ~ "(shift in units of" ~ sigma * ")"),
     ylab = "Power = 1 / ARL",
     main = expression("Power curve of " * bar(X) * " chart: Rule 1 vs Rule 1 + Zone rule"))
for (i in seq_along(n_list)) {
  n <- n_list[i]
  lines(d_grid, sapply(d_grid, zone_power, n = n), col = cols[i], lwd = 2)
  points(delta_list, sapply(delta_list, zone_power, n = n), col = cols[i], pch = 19)
  lines(d_grid, xbar_power(d_grid, n), col = cols[i], lwd = 2, lty = 2)
}
legend("bottomright",
       legend = c(paste("n =", n_list), "Rule 1 + Zone rule", "Rule 1 only"),
       col    = c(cols, "black", "black"),
       lty    = c(1, 1, 1, 1, 2),
       lwd    = 2)
grid()
