# 教材用デモアプリ: 相関係数の検定
# 心理学データ解析基礎 ch13「検定」§相関係数の検定 準拠
# 母集団を点群(散布の雲)として描き，そこから標本をとって標本相関係数 r を得る。
# 母集団は「帰無分布(ρ=0)」も選べる。r を t 統計量に変換して H0: ρ=0 を検定する。
# 検定統計量 t = r√(n-2)/√(1-r²)，自由度 n-2，両側。

pacman::p_load(shiny, ggplot2)

NPOP <- 2000  # 母集団の点数（画面に描く雲）

# UI --------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("相関係数の検定"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("母集団の設定"),
      radioButtons("popmode", "母集団分布",
        choices = c("設定した ρ の母集団" = "rho",
                    "帰無分布（ρ = 0）"   = "null"),
        selected = "rho"),
      sliderInput("rho", "母相関 ρ（上で『設定した ρ』のとき有効）",
                  min = -0.95, max = 0.95, value = 0.5, step = 0.05),
      sliderInput("n", "サンプルサイズ n", min = 4, max = 300, value = 20, step = 1),
      actionButton("draw", "標本をとる", class = "btn-primary"),
      actionButton("test", "検定する", class = "btn-success"),
      tags$hr(),
      helpText("帰無仮説 H0: ρ = 0（無相関），対立仮説 H1: ρ ≠ 0 を有意水準5%で検定します。",
               "検定統計量は t = r√(n−2) / √(1−r²)，自由度は n−2 です。",
               "母集団を『帰無分布（ρ=0）』にして『標本をとる』を繰り返すと，",
               "無相関の世界からとった標本でも標本相関係数 r が0にならず，ばらつく様子が見えます。")
    ),
    mainPanel(
      width = 9,
      plotOutput("pop_scatter", height = "290px"),
      fluidRow(
        column(6, plotOutput("sample_scatter", height = "290px")),
        column(6, plotOutput("tdist", height = "290px"))
      ),
      tags$hr(),
      uiOutput("result_txt")
    )
  )
)

# server ----------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(pop = NULL, idx = NULL, show = FALSE)

  rho_eff <- reactive(if (input$popmode == "null") 0 else input$rho)

  # 母集団(点群)を生成 → そこから標本を抜く
  gen_pop <- function() {
    rho <- rho_eff()
    x <- rnorm(NPOP); z <- rnorm(NPOP)
    y <- rho * x + sqrt(1 - rho^2) * z
    rv$pop <- data.frame(x = x, y = y)
    resample_idx()
  }
  # 同じ母集団から標本を抜き直す
  resample_idx <- function() {
    rv$idx <- sample(nrow(rv$pop), input$n)
    rv$show <- FALSE
  }

  observe({ if (is.null(rv$pop)) gen_pop() })                 # 初期化
  observeEvent(list(input$rho, input$popmode), gen_pop(),     # 母集団が変わったら作り直し
               ignoreInit = TRUE)
  observeEvent(input$n, {                                     # n だけ変えたら抜き直し
    if (!is.null(rv$pop)) resample_idx()
  }, ignoreInit = TRUE)
  observeEvent(input$draw, resample_idx())                    # 標本をとる
  observeEvent(input$test, { rv$show <- TRUE })

  smp <- reactive({ req(rv$pop, rv$idx); rv$pop[rv$idx, ] })

  stat <- reactive({
    s <- smp(); n <- nrow(s)
    r <- cor(s$x, s$y)
    tval <- r * sqrt(n - 2) / sqrt(1 - r^2)
    df <- n - 2
    p <- 2 * pt(-abs(tval), df = df)
    list(n = n, r = r, t = tval, df = df, p = p,
         crit = qt(0.975, df), sig = p < 0.05)
  })

  # 共通の軸範囲（母集団と標本で揃える）
  lims <- reactive({ req(rv$pop); r <- range(c(rv$pop$x, rv$pop$y)); r })

  # 母集団分布（点群） --------------------------------------------------------
  output$pop_scatter <- renderPlot({
    req(rv$pop)
    s <- smp()
    ttl <- if (input$popmode == "null")
      "母集団分布（帰無分布 ρ = 0）" else
      sprintf("母集団分布（ρ = %.2f）", input$rho)
    ggplot(rv$pop, aes(x, y)) +
      geom_point(color = "grey65", size = 0.7, alpha = 0.35) +
      geom_point(data = s, color = "tomato", size = 2, alpha = 0.9) +
      coord_equal(xlim = lims(), ylim = lims()) +
      labs(title = paste0(ttl, "  ／ 赤 = とった標本 n=", nrow(s)),
           x = "変数 X", y = "変数 Y") +
      theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank())
  })

  # 標本の散布図（回帰直線なし） ----------------------------------------------
  output$sample_scatter <- renderPlot({
    s <- smp(); r <- cor(s$x, s$y)
    ggplot(s, aes(x, y)) +
      geom_point(color = "steelblue", size = 2.2, alpha = 0.85) +
      coord_equal(xlim = lims(), ylim = lims()) +
      labs(title = sprintf("標本の散布図（n=%d，r=%.3f）", nrow(s), r),
           x = "変数 X", y = "変数 Y") +
      theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank())
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
               hjust = -0.05, size = 3.4) +
      annotate("text", x = 0, y = -max(dens$y) * 0.06, label = "0", color = "grey40", size = 4) +
      labs(title = sprintf("自由度 %d の t 分布（灰色＝棄却域）", s$df),
           x = "t 値", y = "密度") +
      theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank())
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
