# 心理学データ解析基礎1A ch12「推定」
# 信頼区間の可視化 Shiny アプリ
# 母数(μ, σ)を決めて母集団を描き，標本をとって母平均を区間推定する様子を可視化する

# パッケージ読み込み ----------------------------------------------------------
pacman::p_load(shiny, ggplot2)

# UI --------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("母平均の区間推定：標本から母集団を推し量る"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("母集団の設定"),
      numericInput("mu", "母平均 μ", value = 50, step = 1),
      numericInput("sigma", "母標準偏差 σ", value = 10, min = 0.1, step = 1),
      tags$hr(),
      h4("標本の設定"),
      sliderInput("n", "サンプルサイズ n", min = 2, max = 200, value = 9, step = 1),
      radioButtons("mode", "母分散(σ)は",
        choices = c("既知（正規分布で推定）" = "known",
                    "未知（不偏分散で代用し t 分布で推定）" = "unknown",
                    "両方を重ねて比較する" = "both"),
        selected = "unknown"),
      actionButton("draw", "標本をとる", class = "btn-primary"),
      actionButton("estimate", "推定する", class = "btn-success"),
      tags$hr(),
      helpText("初期値はテキストの数値例（μ=50, σ=10）です。",
               "n を大きくして『標本平均の分布』がどう細くなるか観察しましょう。")
    ),
    mainPanel(
      width = 9,
      plotOutput("plot", height = "420px"),
      tags$hr(),
      fluidRow(
        column(6,
          h4("とれた標本"),
          verbatimTextOutput("sample_txt")
        ),
        column(6,
          h4("推定の結果"),
          uiOutput("result_txt")
        )
      )
    )
  )
)

# server ----------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(sample = NULL, show = FALSE)

  # 標本をとる（起動時にも一度実行）
  draw_sample <- function() {
    rv$sample <- rnorm(input$n, mean = input$mu, sd = input$sigma)
    rv$show <- FALSE           # 新しい標本をとったら推定結果は一旦消す
  }
  observeEvent(input$draw, draw_sample())
  observe({                    # 起動時の初期標本
    if (is.null(rv$sample)) draw_sample()
  })

  # 母数・サンプルサイズ・モードを変えたら推定結果はリセット
  observeEvent(list(input$mu, input$sigma, input$n, input$mode), {
    rv$show <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(input$estimate, { rv$show <- TRUE })

  # 既知・未知それぞれの推定量を計算 ------------------------------------------
  est <- reactive({
    x <- rv$sample
    req(x)
    n <- length(x)
    xbar <- mean(x)

    known <- local({
      se <- input$sigma / sqrt(n)          # 母標準偏差を使う
      mult <- qnorm(0.975)                 # 1.96
      list(se = se, mult = mult,
           lower = xbar - mult * se, upper = xbar + mult * se,
           lab = "標準正規分布", color = "royalblue")
    })
    unknown <- local({
      se <- sd(x) / sqrt(n)                # 不偏分散の平方根で代用
      mult <- qt(0.975, df = n - 1)        # 自由度 n-1 の t
      list(se = se, mult = mult,
           lower = xbar - mult * se, upper = xbar + mult * se,
           lab = sprintf("t 分布（自由度 %d）", n - 1), color = "tomato")
    })
    known$hit   <- (input$mu >= known$lower   & input$mu <= known$upper)
    unknown$hit <- (input$mu >= unknown$lower & input$mu <= unknown$upper)

    list(n = n, xbar = xbar, known = known, unknown = unknown)
  })

  # 表示するモード（"known"/"unknown" のどちらを描くか）のリスト
  active_modes <- reactive({
    switch(input$mode,
      known   = "known",
      unknown = "unknown",
      both    = c("known", "unknown"))
  })

  # プロット ------------------------------------------------------------------
  output$plot <- renderPlot({
    mu <- input$mu; sigma <- input$sigma
    xlim <- c(mu - 4 * sigma, mu + 4 * sigma)
    grid <- seq(xlim[1], xlim[2], length.out = 512)

    # 母集団の分布（広い山）
    pop <- data.frame(x = grid, y = dnorm(grid, mu, sigma))
    p <- ggplot() +
      geom_area(data = pop, aes(x, y), fill = "grey70", alpha = 0.35) +
      geom_line(data = pop, aes(x, y), color = "grey40", linewidth = 0.6) +
      geom_vline(xintercept = mu, color = "grey40",
                 linetype = "dashed", linewidth = 0.5) +
      annotate("text", x = mu, y = dnorm(mu, mu, sigma),
               label = "母集団", vjust = -0.6, color = "grey30", size = 4.5)

    # 標本を下端にラグで表示
    if (!is.null(rv$sample)) {
      smp <- data.frame(x = rv$sample)
      p <- p + geom_rug(data = smp, aes(x = x), sides = "b",
                        color = "steelblue", alpha = 0.7, length = unit(0.05, "npc"))
    }

    # 推定結果（標本平均の分布 + 信頼区間）
    if (rv$show && !is.null(rv$sample)) {
      e <- est()
      modes <- active_modes()

      # 描画高さの基準（表示するモードの標本分布ピークの最大）
      peak <- max(sapply(modes, function(m) dnorm(0, 0, e[[m]]$se)))

      # 標本平均の分布（モードごと）
      for (m in modes) {
        d <- e[[m]]
        samp <- data.frame(x = grid, y = dnorm(grid, e$xbar, d$se))
        p <- p +
          geom_area(data = samp, aes(x, y), fill = d$color, alpha = 0.22) +
          geom_line(data = samp, aes(x, y), color = d$color, linewidth = 0.8)
      }

      # 標本平均の位置（矢印）
      p <- p +
        geom_segment(aes(x = e$xbar, xend = e$xbar, y = 0, yend = peak),
                     color = "red", linewidth = 0.8,
                     arrow = arrow(length = unit(0.18, "cm"), ends = "first")) +
        annotate("text", x = e$xbar, y = peak,
                 label = sprintf("x̄ = %.2f", e$xbar),
                 vjust = -0.5, color = "red", size = 4.5)

      # 信頼区間（モードごとに高さを変えて並べる）
      ys <- if (length(modes) == 2) c(0.60, 0.44) * peak else 0.5 * peak
      for (i in seq_along(modes)) {
        d <- e[[modes[i]]]
        col <- if (d$hit) d$color else "purple"
        yb <- ys[i]
        p <- p +
          annotate("segment", x = d$lower, xend = d$upper, y = yb, yend = yb,
                   color = col, linewidth = 1.1) +
          annotate("segment", x = c(d$lower, d$upper), xend = c(d$lower, d$upper),
                   y = yb - 0.02 * peak, yend = yb + 0.02 * peak,
                   color = col, linewidth = 1.1) +
          annotate("text", x = e$xbar, y = yb,
                   label = sprintf("%s: CI[%.2f, %.2f]", d$lab, d$lower, d$upper),
                   vjust = -0.9, size = 3.9, color = col)
      }
    }

    p + coord_cartesian(xlim = xlim) +
      labs(x = "値", y = "密度") +
      theme_minimal(base_size = 15) +
      theme(panel.grid.minor = element_blank())
  })

  # 標本の値を並べて表示
  output$sample_txt <- renderText({
    x <- rv$sample
    req(x)
    paste0(paste(sprintf("%.2f", x), collapse = ", "),
           "\n\n（n = ", length(x), " 個）")
  })

  # 推定結果のテキスト
  output$result_txt <- renderUI({
    if (!rv$show || is.null(rv$sample)) {
      return(helpText("「推定する」ボタンを押すと結果が出ます。"))
    }
    e <- est()
    modes <- active_modes()
    blocks <- lapply(modes, function(m) {
      d <- e[[m]]
      hit_msg <- if (d$hit) {
        span(style = "color:forestgreen; font-weight:bold;",
             sprintf("μ = %g を含む（当たり）", input$mu))
      } else {
        span(style = "color:purple; font-weight:bold;",
             sprintf("μ = %g を含まない（外れ）", input$mu))
      }
      tagList(
        tags$p(tags$b(d$lab)),
        tags$p(sprintf("標準誤差 = %.3f ／ 係数 = %.3f ／ CI[%.3f, %.3f]",
                       d$se, d$mult, d$lower, d$upper)),
        tags$p(hit_msg)
      )
    })
    tagList(
      tags$p(sprintf("標本平均 x̄ = %.3f", e$xbar)),
      tags$hr(style = "margin:4px 0;"),
      blocks
    )
  })
}

shinyApp(ui, server)
