library(shiny)
library(DT)
library(ggplot2)
library(readxl)
library(gridExtra)
library(grid)

life_table_citation_text <- paste(
  "Almaraz-Valle, V. M., Rodríguez-Maciel, J. C., Ramírez-Valverde, G. & Aguilar-Castillo, C.E. (2026).",
  "Life Table Builder: Interactive Shiny application for automated stage-structured life table calculations (Version 1.0) [Software]."
)

calc_life_table_nqx <- function(df, radix = 100000) {
  df <- df[order(df$x), ]
  n_int <- nrow(df)
  
  lx  <- numeric(n_int)
  ndx <- numeric(n_int)
  Lx  <- numeric(n_int)
  Tx  <- numeric(n_int)
  ex  <- numeric(n_int)
  
  lx[1] <- radix
  
  for (i in seq_len(n_int)) {
    ndx[i] <- lx[i] * df$nQx[i]
    if (i < n_int) lx[i + 1] <- lx[i] - ndx[i]
  }
  
  if (n_int > 1) {
    for (i in 1:(n_int - 1)) {
      Lx[i] <- df$n[i] * (lx[i] + lx[i + 1]) / 2
    }
  }
  Lx[n_int] <- df$n[n_int] * lx[n_int] / 2
  
  Tx[n_int] <- Lx[n_int]
  if (n_int > 1) {
    for (i in (n_int - 1):1) Tx[i] <- Tx[i + 1] + Lx[i]
  }
  
  for (i in seq_len(n_int)) ex[i] <- ifelse(lx[i] > 0, Tx[i] / lx[i], NA)
  
  df$lx  <- lx
  df$ndx <- ndx
  df$Lx  <- Lx
  df$Tx  <- Tx
  df$ex  <- ex
  
  list(
    table = df,
    summary = list(radix = radix, e0 = ex[1])
  )
}

example_stages <- data.frame(
  Stage     = c("Egg", "L1", "L2", "L3", "L4", "Pupa"),
  N_initial = c(100, 80, 77, 65, 61, 60),
  N_pass    = c(80, 77, 65, 61, 60, 60),
  Duration  = c(3.7, 2.3, 2.7, 3.6, 3.8, 7.5)
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML('
      summary.btn-summary{display:inline-block;padding:6px 12px;margin:4px 0;font-weight:600;background:#e0e0e0;border-radius:4px;cursor:pointer;border:1px solid #b0b0b0}
      details[open]>summary.btn-summary{background:#c8c8c8}
      .citation{font-size:13px;color:#444;font-style:italic;white-space:normal}
    '))
  ),
  
  titlePanel("Stage-Structured Life Table"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data input"),
      fileInput("file", "Upload CSV or Excel", accept = c(".csv", ".CSV", ".xls", ".xlsx")),
      checkboxInput("header", "CSV has header", TRUE),
      radioButtons("sep", "CSV separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      numericInput("radix", "Radix (lx0) / initial cohort size", value = 100, min = 1, step = 1),
      
      tags$hr(),
      checkboxInput("use_example", "Use example data", TRUE),
      tags$small("If checked, the uploaded file is ignored."),
      
      tags$hr(),
      tags$details(
        tags$summary(class = "btn-summary", "Column mapping (optional)"),
        uiOutput("col_select")
      ),
      
      tags$hr(),
      h4("Suggested citation"),
      div(class = "citation", textOutput("citation_sidebar"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input data", br(), DTOutput("table_input")),
        
        tabPanel(
          "Life table",
          br(),
          h4("Calculated life table"),
          DTOutput("table_life"),
          br(),
          downloadButton("download_life_table", "Download life table (.pdf)"),
          br(), br(),
          h4("Summary"),
          verbatimTextOutput("summary_out"),
          br(),
          h5("Citation"),
          div(class = "citation", textOutput("citation_text"))
        ),
        
        tabPanel(
          "e(x) plot",
          br(),
          plotOutput("plot_ex"),
          br(),
          downloadButton("download_plot_ex", "Download e(x) plot (.png)"),
          br(), br(),
          h5("Citation"),
          div(class = "citation", textOutput("citation_text_plot"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$citation_sidebar <- renderText(life_table_citation_text)
  output$citation_text <- renderText(life_table_citation_text)
  output$citation_text_plot <- renderText(life_table_citation_text)
  
  data_input <- reactive({
    if (input$use_example || is.null(input$file)) return(example_stages)
    
    ext <- tools::file_ext(input$file$name)
    if (tolower(ext) %in% c("xls", "xlsx")) {
      readxl::read_excel(input$file$datapath)
    } else {
      read.table(
        input$file$datapath,
        header = input$header,
        sep = input$sep,
        dec = ".",
        stringsAsFactors = FALSE
      )
    }
  })
  
  output$table_input <- renderDT({
    df <- data_input()
    validate(need(!is.null(df), "No data available."))
    datatable(df, options = list(pageLength = 10))
  })
  
  output$col_select <- renderUI({
    df <- data_input()
    if (is.null(df)) return(NULL)
    
    cols <- names(df)
    guess <- function(pattern, default_index) {
      m <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
      if (length(m) > 0) m[1] else cols[min(default_index, length(cols))]
    }
    
    tagList(
      selectInput("col_stage", "Column: Stage", choices = cols, selected = guess("stage|estad", 1)),
      selectInput("col_Nini",  "Column: N_initial", choices = cols, selected = guess("n[_ ]?ini|initial|ini", 2)),
      selectInput("col_Nnext", "Column: N_pass", choices = cols, selected = guess("pass|pasan|next", 3)),
      selectInput("col_dur",   "Column: Duration (days)", choices = cols, selected = guess("dur|duration", 4))
    )
  })
  
  life_results <- reactive({
    df <- data_input()
    validate(need(!is.null(df), "No data available."))
    
    cols <- names(df)
    guess <- function(pattern, default_index) {
      m <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
      if (length(m) > 0) m[1] else cols[min(default_index, length(cols))]
    }
    
    col_stage <- if (!is.null(input$col_stage)) input$col_stage else guess("stage|estad", 1)
    col_Nini  <- if (!is.null(input$col_Nini))  input$col_Nini  else guess("n[_ ]?ini|initial|ini", 2)
    col_Nnext <- if (!is.null(input$col_Nnext)) input$col_Nnext else guess("pass|pasan|next", 3)
    col_dur   <- if (!is.null(input$col_dur))   input$col_dur   else guess("dur|duration", 4)
    
    df_st <- data.frame(
      Stage     = as.character(df[[col_stage]]),
      N_initial = suppressWarnings(as.numeric(df[[col_Nini]])),
      N_pass    = suppressWarnings(as.numeric(df[[col_Nnext]])),
      Duration  = suppressWarnings(as.numeric(df[[col_dur]]))
    )
    df_st <- df_st[!is.na(df_st$N_initial), ]
    
    validate(need(nrow(df_st) > 0, "No valid rows found after reading data."))
    validate(need(all(!is.na(df_st$N_pass)), "N_pass has missing values after numeric conversion."))
    validate(need(all(!is.na(df_st$Duration)), "Duration has missing values after numeric conversion."))
    validate(need(all(df_st$Duration >= 0), "Duration must be >= 0."))
    validate(need(all(df_st$N_initial >= 0), "N_initial must be >= 0."))
    validate(need(all(df_st$N_pass >= 0), "N_pass must be >= 0."))
    validate(need(all(df_st$N_pass <= df_st$N_initial), "Some rows have N_pass > N_initial."))
    
    df_st$nQx <- with(df_st, ifelse(N_initial > 0, (N_initial - N_pass) / N_initial, 0))
    df_st$n   <- df_st$Duration
    df_st$x   <- c(0, head(cumsum(df_st$n), -1))
    
    res <- calc_life_table_nqx(df_st[, c("x", "n", "nQx")], radix = input$radix)
    
    res$table$Stage     <- df_st$Stage
    res$table$N_initial <- df_st$N_initial
    res$table$N_pass    <- df_st$N_pass
    
    res$table <- res$table[, c("Stage", "x", "n", "nQx", "N_initial", "N_pass", "lx", "ndx", "Lx", "Tx", "ex")]
    
    num_cols <- c("x", "n", "nQx", "lx", "ndx", "Lx", "Tx", "ex")
    for (cn in num_cols) res$table[[cn]] <- round(res$table[[cn]], 4)
    
    res
  })
  
  output$table_life <- renderDT({
    datatable(life_results()$table, options = list(pageLength = 20))
  })
  
  output$summary_out <- renderPrint({
    s <- life_results()$summary
    cat("Radix (lx0):", s$radix, "\n")
    cat("Initial life expectancy e(0):", sprintf("%.4f days\n", s$e0))
  })
  
  output$download_life_table <- downloadHandler(
    filename = function() paste0("life_table_", Sys.Date(), ".pdf"),
    content = function(file) {
      df_tab <- life_results()$table
      
      title_grob <- grid::textGrob(
        "Life table generated with Life Table Builder",
        gp = grid::gpar(fontsize = 16, fontface = "bold")
      )
      citation_grob <- grid::textGrob(
        life_table_citation_text,
        x = 0, y = 1, just = c("left", "top"),
        gp = grid::gpar(fontsize = 10)
      )
      table_grob <- gridExtra::tableGrob(df_tab, rows = NULL)
      
      pdf(file, width = 11, height = 8.5)
      gridExtra::grid.arrange(
        title_grob, citation_grob, table_grob,
        ncol = 1, heights = c(0.1, 0.15, 0.75)
      )
      dev.off()
    }
  )
  
  plot_ex_fun <- function(df) {
    dur_str <- format(round(df$n, 1), nsmall = 1)
    df$label_plot <- paste0(df$Stage, " (", dur_str, " d)")
    
    ggplot(df, aes(x = x, y = ex, label = label_plot)) +
      geom_point(size = 3) +
      geom_line() +
      geom_text(vjust = -0.7, size = 4.5) +
      theme_minimal() +
      labs(
        x = "Age (days since egg)",
        y = "e(x) (life expectancy, days)",
        title = "Life expectancy at age x by stage"
      ) +
      theme(
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 16),
        axis.text  = element_text(size = 16)
      )
  }
  
  output$plot_ex <- renderPlot(plot_ex_fun(life_results()$table))
  
  output$download_plot_ex <- downloadHandler(
    filename = function() paste0("ex_plot_", Sys.Date(), ".png"),
    content = function(file) {
      df <- life_results()$table
      png(file, width = 1600, height = 900, res = 150)
      print(plot_ex_fun(df))
      dev.off()
    }
  )
}

shinyApp(ui, server)
