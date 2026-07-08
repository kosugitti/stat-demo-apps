# 教材用デモアプリ: 相関係数の検定
# 心理学データ解析基礎 ch13「検定」§相関係数の検定 準拠
# 母相関 ρ の世界から標本をとり，標本相関係数 r を t 統計量に変換して
# H0: ρ=0 を検定する（t = r√(n-2)/√(1-r²)，自由度 n-2，両側）

pacman::p_load(shiny, ggplot2)

# UI --------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("相関係数の検定"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("母集団の設定"),
      sliderInput("rho", "母相関 ρ", min = -0.95, max = 0.95, value = 0.5, step = 0.05),
      sliderInput("n", "サンプルサイズ n", min = 4, max = 300, value = 20, step = 1),
      actionButton("draw", "標本をとる", class = "btn-primary"),
      actionButton("test", "検定する", class = "btn-success"),
      tags$hr(),
      helpText("帰無仮説 H0: ρ = 0（無相関），対立仮説 H1: ρ ≠ 0 を有意水準5%で検定します。",
               "検定統計量は t = r√(n−2) / √(1−r²)，自由度は n−2 です。",
               "ρ を 0 にして『標本をとる』を繰り返すと，無相関でも r がばらつく様子が見えます。")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(6, plotOutput("scatter", height = "340px")),
        column(6, plotOutput("tdist", height = "340px"))
      ),
      tags$hr(),
      uiOutput("result_txt")
    )
  )
)

# server ----------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(x = NULL, y = NULL, show = FALSE)

  draw_sample <- function() {
    rho <- input$rho; n <- input$n
    x <- rnorm(n)
    z <- rnorm(n)
    y <- rho * x + sqrt(1 - rho^2) * z    # 相関 ρ の二変量正規から生成
    rv$x <- x; rv$y <- y
    rv$show <- FALSE
  }
  observeEvent(input$draw, draw_sample())
  observe({ if (is.null(rv$x)) draw_sample() })

  observeEvent(list(input$rho, input$n), { rv$show <- FALSE }, ignoreInit = TRUE)
  observeEvent(input$test, { rv$show <- TRUE })

  stat <- reactive({
    req(rv$x, rv$y)
    n <- length(rv$x)
    r <- cor(rv$x, rv$y)
    tval <- r * sqrt(n - 2) / sqrt(1 - r^2)
    df <- n - 2
    p <- 2 * pt(-abs(tval), df = df)     # 両側
    crit <- qt(0.975, df = df)
    list(n = n, r = r, t = tval, df = df, p = p, crit = crit, sig = p < 0.05)
  })

  # 散布図 --------------------------------------------------------------------
  output$scatter <- renderPlot({
    req(rv$x, rv$y)
    d <- data.frame(x = rv$x, y = rv$y)
    r <- cor(rv$x, rv$y)
    ggplot(d, aes(x, y)) +
      geom_point(color = "steelblue", size = 2, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "tomato", linewidth = 0.7,
                  formula = y ~ x) +
      labs(title = sprintf("標本の散布図（n=%d，r=%.3f）", length(rv$x), r),
           x = "変数 X", y = "変数 Y") +
      theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
  })

  # t 分布・棄却域・実現値 ----------------------------------------------------
  output$tdist <- renderPlot({
    if (!rv$show) {
      return(ggplot() + annotate("text", x = 0, y = 0,
             label = "「検定する」を押すと\nt分布・棄却域・実現値を表示します") +
             theme_void(base_size = 13))
    }
    s <- stat()
    lim <- max(4, abs(s$t) * 1.15)
    grid <- seq(-lim, lim, length.out = 512)
    dens <- data.frame(x = grid, y = dt(grid, df = s$df))
    rej_r <- subset(dens, x >=  s$crit)
    rej_l <- subset(dens, x <= -s$crit)
    col <- if (s$sig) "forestgreen" else "purple"
    ggplot(dens, aes(x, y)) +
      geom_area(data = rej_r, fill = "grey70", alpha = 0.6) +
      geom_area(data = rej_l, fill = "grey70", alpha = 0.6) +
      geom_line(linewidth = 0.7) +
      geom_vline(xintercept = c(-s$crit, s$crit), color = "grey40", linetype = "dotted") +
      geom_vline(xintercept = s$t, color = col, linewidth = 1) +
      annotate("text", x = s$t, y = max(dens$y) * 0.9,
               label = sprintf("t = %.2f", s$t), color = col, hjust = -0.1, size = 4.5) +
      annotate("text", x = s$crit, y = max(dens$y) * 0.5,
               label = sprintf("臨界値 %.2f", s$crit), color = "grey30",
               hjust = -0.05, size = 3.6) +
      labs(title = sprintf("自由度 %d の t 分布（灰色＝棄却域）", s$df),
           x = "t 値", y = "密度") +
      theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
  })

  output$result_txt <- renderUI({
    if (!rv$show) return(helpText("「検定する」ボタンを押すと結果が出ます。"))
    s <- stat()
    judge <- if (s$sig) {
      span(style = "color:forestgreen; font-weight:bold;",
           "p < 0.05：帰無仮説を棄却。母相関は0でない（有意な相関がある）と判断")
    } else {
      span(style = "color:purple; font-weight:bold;",
           "p ≥ 0.05：帰無仮説を棄却できない。相関があるとは言えない")
    }
    tagList(
      h4("検定の結果"),
      tags$p(sprintf("標本相関係数 r = %.3f（n = %d）", s$r, s$n)),
      tags$p(HTML(sprintf("検定統計量：<i>t</i>(%d) = %.3f，<i>p</i> = %.4f（両側）",
                          s$df, s$t, s$p))),
      tags$p(judge),
      tags$p(style = "color:grey; font-size:90%;",
             "注意：有意かどうかと相関の強さは別の話です。n が大きいと小さな r でも有意になります（ch13）。")
    )
  })
}

shinyApp(ui, server)
