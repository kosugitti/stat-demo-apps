# 教材用デモアプリ: 2群の平均値差の推定と t 検定
# 心理学データ解析基礎 ch12(§4)〜ch13「検定」準拠
# 効果量と母標準偏差から2群の母集団を描き，各群から標本をとって
# 各群の平均を区間推定し，平均値差の推定（差の分布）または帰無分布で t 検定を示す

pacman::p_load(shiny, ggplot2)

# UI --------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("2群の平均値差の推定と t 検定"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("母集団の設定"),
      numericInput("mu1", "統制群の母平均 μ1", value = 50, step = 1),
      numericInput("sigma", "母標準偏差 σ（両群共通）", value = 10, min = 0.1, step = 1),
      sliderInput("d", "効果量 Cohen's d（実験群のズレ = d×σ）",
                  min = 0, max = 4, value = 0.5, step = 0.1),
      tags$hr(),
      h4("標本の設定"),
      sliderInput("n", "各群のサンプルサイズ n", min = 2, max = 200, value = 20, step = 1),
      radioButtons("veq", "分散の扱い",
        choices = c("Welch（等分散を仮定しない・R既定）" = "welch",
                    "Student（等分散を仮定）" = "student"),
        selected = "welch"),
      radioButtons("lower", "下段の表示",
        choices = c("差の分布（平均値差の区間推定）" = "diff",
                    "帰無分布（t 検定：棄却域と実現値）" = "null"),
        selected = "diff"),
      actionButton("draw", "標本をとる", class = "btn-primary"),
      actionButton("test", "検定する", class = "btn-success"),
      tags$hr(),
      helpText("実験群の母平均 μ2 = μ1 + d×σ。d=0 なら2群の母平均は同じです。",
               "帰無仮説 H0: μ1 = μ2，対立仮説 H1: μ1 ≠ μ2 を有意水準5%で検定します。")
    ),
    mainPanel(
      width = 9,
      plotOutput("pop_plot", height = "300px"),
      plotOutput("lower_plot", height = "230px"),
      tags$hr(),
      fluidRow(
        column(6, h4("とれた標本"), verbatimTextOutput("sample_txt")),
        column(6, h4("検定の結果"), uiOutput("result_txt"))
      )
    )
  )
)

# server ----------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(x1 = NULL, x2 = NULL, show = FALSE)

  mu2 <- reactive(input$mu1 + input$d * input$sigma)

  draw_sample <- function() {
    rv$x1 <- rnorm(input$n, mean = input$mu1, sd = input$sigma)
    rv$x2 <- rnorm(input$n, mean = mu2(),     sd = input$sigma)
    rv$show <- FALSE
  }
  observeEvent(input$draw, draw_sample())
  observe({ if (is.null(rv$x1)) draw_sample() })

  observeEvent(list(input$mu1, input$sigma, input$d, input$n, input$veq), {
    rv$show <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(input$test, { rv$show <- TRUE })

  # 推定量と t 検定 -----------------------------------------------------------
  res <- reactive({
    req(rv$x1, rv$x2)
    n1 <- length(rv$x1); n2 <- length(rv$x2)
    m1 <- mean(rv$x1);   m2 <- mean(rv$x2)
    # 各群の母平均の95%信頼区間（母分散未知＝t 分布）
    se1 <- sd(rv$x1) / sqrt(n1); se2 <- sd(rv$x2) / sqrt(n2)
    ci1 <- m1 + c(-1, 1) * qt(0.975, n1 - 1) * se1
    ci2 <- m2 + c(-1, 1) * qt(0.975, n2 - 1) * se2
    # 2群の t 検定
    tt <- t.test(rv$x2, rv$x1, var.equal = (input$veq == "student"))
    df <- unname(tt$parameter)
    tcrit <- qt(0.975, df)
    se_diff <- (tt$conf.int[2] - tt$conf.int[1]) / (2 * tcrit)  # 検定と整合する差の標準誤差
    list(n1 = n1, n2 = n2, m1 = m1, m2 = m2, ci1 = ci1, ci2 = ci2,
         diff = m2 - m1, t = unname(tt$statistic), df = df, tcrit = tcrit,
         p = tt$p.value, ci = tt$conf.int, se_diff = se_diff,
         sig = tt$p.value < 0.05)
  })

  # 上段: 母集団2つ + 各群の標本と信頼区間 ------------------------------------
  output$pop_plot <- renderPlot({
    mu1 <- input$mu1; s <- input$sigma; m2 <- mu2()
    xlim <- c(min(mu1, m2) - 4 * s, max(mu1, m2) + 4 * s)
    grid <- seq(xlim[1], xlim[2], length.out = 512)
    peak <- dnorm(mu1, mu1, s)
    pop <- rbind(
      data.frame(x = grid, y = dnorm(grid, mu1, s), 群 = "統制群"),
      data.frame(x = grid, y = dnorm(grid, m2,  s), 群 = "実験群")
    )
    cols <- c("統制群" = "steelblue", "実験群" = "tomato")
    p <- ggplot(pop, aes(x, y, color = 群, fill = 群)) +
      geom_area(alpha = 0.18, position = "identity") +
      geom_line(linewidth = 0.7) +
      scale_color_manual(values = cols) + scale_fill_manual(values = cols)

    if (!is.null(rv$x1)) {
      rug <- rbind(data.frame(x = rv$x1, 群 = "統制群"),
                   data.frame(x = rv$x2, 群 = "実験群"))
      p <- p + geom_rug(data = rug, aes(x = x, color = 群), inherit.aes = FALSE,
                        sides = "b", alpha = 0.6, length = unit(0.06, "npc"))
    }
    if (rv$show) {
      r <- res()
      # 各群の平均のCIバー（曲線上）＋平均点
      y1 <- peak * 0.55; y2 <- peak * 0.78
      cap <- peak * 0.03
      p <- p +
        # 統制群CI
        annotate("segment", x = r$ci1[1], xend = r$ci1[2], y = y1, yend = y1,
                 color = "steelblue", linewidth = 1.1) +
        annotate("segment", x = c(r$ci1[1], r$ci1[2]), xend = c(r$ci1[1], r$ci1[2]),
                 y = y1 - cap, yend = y1 + cap, color = "steelblue", linewidth = 1.1) +
        annotate("point", x = r$m1, y = y1, color = "steelblue", size = 2.6) +
        # 実験群CI
        annotate("segment", x = r$ci2[1], xend = r$ci2[2], y = y2, yend = y2,
                 color = "tomato", linewidth = 1.1) +
        annotate("segment", x = c(r$ci2[1], r$ci2[2]), xend = c(r$ci2[1], r$ci2[2]),
                 y = y2 - cap, yend = y2 + cap, color = "tomato", linewidth = 1.1) +
        annotate("point", x = r$m2, y = y2, color = "tomato", size = 2.6) +
        annotate("text", x = r$m1, y = y1, label = sprintf("x̄1=%.2f", r$m1),
                 color = "steelblue", vjust = -1, size = 3.8) +
        annotate("text", x = r$m2, y = y2, label = sprintf("x̄2=%.2f", r$m2),
                 color = "tomato", vjust = -1, size = 3.8)
    }
    p + coord_cartesian(xlim = xlim) +
      labs(x = "値", y = "密度", title = "母集団（2群）・標本・各群の平均の95%信頼区間") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank(), legend.position = "top")
  })

  # 下段: 差の分布 or 帰無分布 ------------------------------------------------
  output$lower_plot <- renderPlot({
    if (!rv$show) {
      msg <- if (input$lower == "diff")
        "「検定する」を押すと，平均値差の分布と95%信頼区間を表示します" else
        "「検定する」を押すと，帰無分布・棄却域・実現値 t を表示します"
      return(ggplot() + annotate("text", x = 0, y = 0, label = msg) +
             theme_void(base_size = 13))
    }
    r <- res()
    col <- if (r$sig) "forestgreen" else "purple"

    if (input$lower == "diff") {
      # 平均値差の標本分布（d̂ 中心）＋95%CI＋0の位置
      lim <- max(abs(r$diff) + 4 * r$se_diff, 4 * r$se_diff)
      ctr <- r$diff
      grid <- seq(ctr - lim, ctr + lim, length.out = 512)
      dens <- data.frame(x = grid, y = dnorm(grid, ctr, r$se_diff))
      pk <- max(dens$y)
      ggplot(dens, aes(x, y)) +
        geom_area(fill = col, alpha = 0.18) + geom_line(color = col, linewidth = 0.8) +
        geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
        annotate("text", x = 0, y = pk * 1.02, label = "0", color = "grey40", size = 4) +
        geom_segment(aes(x = ctr, xend = ctr, y = 0, yend = pk),
                     color = col, linewidth = 0.7) +
        # 95%CI バー
        annotate("segment", x = r$ci[1], xend = r$ci[2], y = pk * 0.5, yend = pk * 0.5,
                 color = col, linewidth = 1.2) +
        annotate("segment", x = c(r$ci[1], r$ci[2]), xend = c(r$ci[1], r$ci[2]),
                 y = pk * 0.44, yend = pk * 0.56, color = col, linewidth = 1.2) +
        annotate("text", x = ctr, y = pk * 0.5,
                 label = sprintf("差=%.2f，95%%CI[%.2f, %.2f]", r$diff, r$ci[1], r$ci[2]),
                 color = col, vjust = -0.9, size = 4) +
        labs(x = "母平均の差 (μ2 − μ1)", y = "密度", title = "差の分布と95%信頼区間") +
        theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
    } else {
      # 帰無分布（H0 のもとでの t 分布）＋棄却域＋臨界値＋実現値
      lim <- max(4, abs(r$t) * 1.15)
      grid <- seq(-lim, lim, length.out = 512)
      dens <- data.frame(x = grid, y = dt(grid, df = r$df))
      rej_r <- subset(dens, x >=  r$tcrit)
      rej_l <- subset(dens, x <= -r$tcrit)
      pk <- max(dens$y)
      ggplot(dens, aes(x, y)) +
        geom_area(data = rej_r, fill = "grey70", alpha = 0.6) +
        geom_area(data = rej_l, fill = "grey70", alpha = 0.6) +
        geom_line(linewidth = 0.7) +
        geom_vline(xintercept = c(-r$tcrit, r$tcrit), color = "grey40", linetype = "dotted") +
        geom_vline(xintercept = 0, color = "grey60") +
        geom_segment(aes(x = r$t, xend = r$t, y = pk * 0.9, yend = 0),
                     color = col, linewidth = 1,
                     arrow = arrow(length = unit(0.18, "cm"))) +
        annotate("text", x = r$t, y = pk * 0.9, label = sprintf("t = %.2f", r$t),
                 color = col, vjust = -0.4, size = 4.5) +
        annotate("text", x = r$tcrit, y = pk * 0.5, label = sprintf("臨界値 %.2f", r$tcrit),
                 color = "grey30", hjust = -0.05, size = 3.6) +
        annotate("text", x = 0, y = -pk * 0.06, label = "0", color = "grey40", size = 4) +
        labs(x = "t 値", y = "密度",
             title = sprintf("帰無分布：自由度 %.1f の t 分布（灰色＝棄却域）", r$df)) +
        theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
    }
  })

  output$sample_txt <- renderText({
    req(rv$x1)
    paste0("統制群 (n=", length(rv$x1), "): ",
           paste(sprintf("%.1f", rv$x1), collapse = ", "),
           "\n\n実験群 (n=", length(rv$x2), "): ",
           paste(sprintf("%.1f", rv$x2), collapse = ", "))
  })

  output$result_txt <- renderUI({
    if (!rv$show) return(helpText("「検定する」ボタンを押すと結果が出ます。"))
    r <- res()
    method <- if (input$veq == "student") "Student の t 検定（等分散仮定）" else "Welch の t 検定"
    judge <- if (r$sig) {
      span(style = "color:forestgreen; font-weight:bold;",
           "p < 0.05：帰無仮説を棄却。母平均に差があると判断（差の信頼区間は0を含まない）")
    } else {
      span(style = "color:purple; font-weight:bold;",
           "p ≥ 0.05：帰無仮説を棄却できない。差があるとは言えない（差の信頼区間が0を含む）")
    }
    tagList(
      tags$p(method),
      tags$p(sprintf("統制群 x̄1 = %.2f，95%%CI[%.2f, %.2f]", r$m1, r$ci1[1], r$ci1[2])),
      tags$p(sprintf("実験群 x̄2 = %.2f，95%%CI[%.2f, %.2f]", r$m2, r$ci2[1], r$ci2[2])),
      tags$p(sprintf("差 = %.2f，差の95%%CI[%.2f, %.2f]", r$diff, r$ci[1], r$ci[2])),
      tags$p(HTML(sprintf("検定結果：<i>t</i>(%.1f) = %.3f，<i>p</i> = %.4f",
                          r$df, r$t, r$p))),
      tags$p(judge)
    )
  })
}

shinyApp(ui, server)
