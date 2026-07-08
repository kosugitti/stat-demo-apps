# 教材用デモアプリ: 2群の平均値差の推定と t 検定
# 心理学データ解析基礎 ch12(§4)〜ch13「検定」準拠
# 効果量と母標準偏差から2群の母集団を描き，各群から標本をとって
# 平均値差を区間推定し，t 検定の結果を表示する

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
                  min = 0, max = 2, value = 0.5, step = 0.1),
      tags$hr(),
      h4("標本の設定"),
      sliderInput("n", "各群のサンプルサイズ n", min = 2, max = 200, value = 20, step = 1),
      radioButtons("veq", "分散の扱い",
        choices = c("Welch（等分散を仮定しない・R既定）" = "welch",
                    "Student（等分散を仮定）" = "student"),
        selected = "welch"),
      actionButton("draw", "標本をとる", class = "btn-primary"),
      actionButton("test", "検定する", class = "btn-success"),
      tags$hr(),
      helpText("実験群の母平均 μ2 = μ1 + d×σ。d=0 なら2群の母平均は同じです。",
               "帰無仮説 H0: μ1 = μ2，対立仮説 H1: μ1 ≠ μ2 を有意水準5%で検定します。")
    ),
    mainPanel(
      width = 9,
      plotOutput("pop_plot", height = "300px"),
      plotOutput("diff_plot", height = "150px"),
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

  # t 検定 --------------------------------------------------------------------
  res <- reactive({
    req(rv$x1, rv$x2)
    tt <- t.test(rv$x2, rv$x1, var.equal = (input$veq == "student"))
    list(
      m1 = mean(rv$x1), m2 = mean(rv$x2),
      diff = mean(rv$x2) - mean(rv$x1),
      t = unname(tt$statistic), df = unname(tt$parameter),
      p = tt$p.value, ci = tt$conf.int,
      sig = tt$p.value < 0.05
    )
  })

  # 母集団と標本のプロット ----------------------------------------------------
  output$pop_plot <- renderPlot({
    mu1 <- input$mu1; s <- input$sigma; m2 <- mu2()
    xlim <- c(min(mu1, m2) - 4 * s, max(mu1, m2) + 4 * s)
    grid <- seq(xlim[1], xlim[2], length.out = 512)
    pop <- rbind(
      data.frame(x = grid, y = dnorm(grid, mu1, s), 群 = "統制群"),
      data.frame(x = grid, y = dnorm(grid, m2,  s), 群 = "実験群")
    )
    cols <- c("統制群" = "steelblue", "実験群" = "tomato")
    p <- ggplot(pop, aes(x, y, color = 群, fill = 群)) +
      geom_area(alpha = 0.20, position = "identity") +
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
      p <- p +
        geom_vline(xintercept = r$m1, color = "steelblue", linetype = "dashed") +
        geom_vline(xintercept = r$m2, color = "tomato",    linetype = "dashed") +
        annotate("text", x = r$m1, y = max(pop$y), label = sprintf("x̄1=%.2f", r$m1),
                 color = "steelblue", hjust = 1.05, vjust = 1, size = 4) +
        annotate("text", x = r$m2, y = max(pop$y), label = sprintf("x̄2=%.2f", r$m2),
                 color = "tomato", hjust = -0.05, vjust = 1, size = 4)
    }
    p + coord_cartesian(xlim = xlim) +
      labs(x = "値", y = "密度", title = "母集団（2群）と標本") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank(), legend.position = "top")
  })

  # 平均値差の95%信頼区間プロット（0を含むか） --------------------------------
  output$diff_plot <- renderPlot({
    if (!rv$show) {
      return(ggplot() + annotate("text", x = 0, y = 0,
             label = "「検定する」を押すと，平均値差の95%信頼区間を表示します") +
             theme_void(base_size = 13))
    }
    r <- res()
    col <- if (r$sig) "forestgreen" else "purple"
    rng <- range(c(0, r$ci)); pad <- diff(rng) * 0.25 + 0.5
    ggplot() +
      geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
      annotate("segment", x = r$ci[1], xend = r$ci[2], y = 0, yend = 0,
               color = col, linewidth = 1.4) +
      annotate("segment", x = c(r$ci[1], r$ci[2]), xend = c(r$ci[1], r$ci[2]),
               y = -0.3, yend = 0.3, color = col, linewidth = 1.4) +
      annotate("point", x = r$diff, y = 0, color = col, size = 3) +
      annotate("text", x = r$diff, y = 0.5,
               label = sprintf("差 = %.2f，95%%CI[%.2f, %.2f]", r$diff, r$ci[1], r$ci[2]),
               color = col, size = 4.3) +
      annotate("text", x = 0, y = -0.6, label = "0", color = "grey40", size = 4) +
      coord_cartesian(xlim = c(rng[1] - pad, rng[2] + pad), ylim = c(-0.9, 0.9)) +
      labs(x = "母平均の差 (μ2 − μ1)", y = NULL, title = "平均値差の95%信頼区間") +
      theme_minimal(base_size = 13) +
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank())
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
           "p < 0.05：帰無仮説を棄却。母平均に差があると判断（信頼区間は0を含まない）")
    } else {
      span(style = "color:purple; font-weight:bold;",
           "p ≥ 0.05：帰無仮説を棄却できない。差があるとは言えない（信頼区間が0を含む）")
    }
    tagList(
      tags$p(method),
      tags$p(sprintf("x̄1 = %.2f，x̄2 = %.2f，差 = %.2f", r$m1, r$m2, r$diff)),
      tags$p(sprintf("差の95%%信頼区間：[%.2f, %.2f]", r$ci[1], r$ci[2])),
      tags$p(HTML(sprintf("検定結果：<i>t</i>(%.1f) = %.3f，<i>p</i> = %.4f",
                          r$df, r$t, r$p))),
      tags$p(judge)
    )
  })
}

shinyApp(ui, server)
