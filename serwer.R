# ================= SERVER =================
server <- function(input, output, session) {
  
  dane <- reactiveVal(NULL)
  
  # ---- Pobieranie danych ----
  observeEvent(input$pobierz,{
    req(input$okres)
    showModal(modalDialog("📡 Pobieram dane...", easyClose=FALSE))
    
    tryCatch({
      dni <- seq(as.Date(input$okres[1])-1, as.Date(input$okres[2])+1, by="day")
      data_list <- lapply(as.character(dni), function(d){ tryCatch(get_pse_data(d), error=function(e) NULL) })
      valid <- data_list[!sapply(data_list,is.null)]
      if(length(valid)>0){
        dt <- rbindlist(valid, fill=TRUE)
        setorder(dt,godzina)
        dane(dt)
        showNotification("✅ Dane pobrane", type="message")
      } else showNotification("⚠️ Brak danych", type="warning")
    }, error=function(e){
      showNotification(e$message, type="error")
    })
    
    removeModal()
  })
  
  # ---- Filtrowanie danych ----
  filtered_data <- reactive({
    req(dane(), input$szeregi)
    dt <- copy(dane())
    
    setnames(dt, intersect(c("godzina","timestamp","time"), names(dt))[1], "ts")
    dt[, data := as.Date(substr(ts,1,10))]
    if("ts" %in% names(dt)) dt[, godzina_full := substr(ts,12,16)]
    dt[, godzina_num := as.integer(substr(ts,12,13))]
    dt[, minuta := as.integer(substr(ts,15,16))]
    dt[, ts := NULL]
    
    dt <- dt[data >= input$okres[1] & data <= input$okres[2]]
    
    col_map <- list(
      demand="zapotrzebowanie_na_moc",
      pv="sumaryczna_generacja_zrodel_fotowoltaicznych",
      wi="sumaryczna_generacja_zrodel_wiatrowych",
      jgm="sumaryczna_moc_ladowania_jgm"
    )
    selected_cols <- unlist(col_map[input$szeregi])
    selected_cols <- selected_cols[selected_cols %in% names(dt)]
    
    if(input$agregacja=="1d"){
      dt[, (selected_cols) := lapply(.SD,sum,na.rm=TRUE), by=data, .SDcols=selected_cols]
      dt <- unique(dt, by="data")
      base_cols <- "data"
    } else {
      setorder(dt, data, godzina_num, minuta)
      base_cols <- c("data","godzina_full")
    }
    
    final_cols <- c(base_cols, selected_cols)
    dt <- dt[, ..final_cols]
    
    num_cols <- names(dt)[sapply(dt,is.numeric)]
    if(length(num_cols)>0) dt[, (num_cols) := lapply(.SD, round,2), .SDcols=num_cols]
    
    if("godzina_full" %in% names(dt)){
      dt[, ts_full := as.POSIXct(paste0(data," ",godzina_full,":00"), format="%Y-%m-%d %H:%M:%S")]
      dt[, hour := as.integer(substr(godzina_full,1,2))]
    } else {
      dt[, ts_full := as.POSIXct(data)]
      dt[, hour := 0]  # dla danych dziennych
    }
    dt[, wday := as.POSIXlt(ts_full)$wday]
    dt[, wday := ifelse(wday==0,7,wday)]
    dt[, month := as.POSIXlt(ts_full)$mon + 1]
    dt[, ts_full := NULL]
    
    # filtry
    if(!is.null(input$filter_month)) dt <- dt[month %in% input$filter_month]
    if(!is.null(input$filter_wday)) dt <- dt[wday %in% input$filter_wday]
    if(!is.null(input$filter_hour)) dt <- dt[hour %in% input$filter_hour]
    
    dt
  })
  
  # ---- Kolory i etykiety ----
  series_colors <- c(
    zapotrzebowanie_na_moc="#1f77b4",
    sumaryczna_generacja_zrodel_fotowoltaicznych="#ff7f0e",
    sumaryczna_generacja_zrodel_wiatrowych="#2ca02c",
    sumaryczna_moc_ladowania_jgm="#d62728"
  )
  label_map <- c(
    zapotrzebowanie_na_moc="⚡ Zapotrzebowanie",
    sumaryczna_generacja_zrodel_fotowoltaicznych="☀️ PV",
    sumaryczna_generacja_zrodel_wiatrowych="🌪️ Wiatr",
    sumaryczna_moc_ladowania_jgm="🔋 Magazyny"
  )
  
  # ---- Dynamiczne checkboxy ----
  observe({
    dt <- filtered_data()
    num_cols <- names(dt)[sapply(dt,is.numeric)]
    num_cols <- num_cols[num_cols %in% names(label_map)]
    if(!length(num_cols)) return()
    choices <- setNames(num_cols, label_map[num_cols])
    updateCheckboxGroupInput(session,"wykresy_series",choices=choices,selected=character(0))
    updateCheckboxGroupInput(session,"boxplot_series",choices=choices,selected=character(0))
  })
  
  # ---- Tabela ----
  output$tabela <- renderDT({
    dt <- copy(filtered_data())
    num_cols <- names(dt)[sapply(dt,is.numeric)]
    num_cols <- num_cols[num_cols %in% names(label_map)]
    setnames(dt, num_cols, label_map[num_cols])
    if("godzina_full" %in% names(dt)) setnames(dt,"godzina_full","Godzina")
    DT::datatable(dt, options=list(pageLength=25, scrollX=TRUE), rownames=FALSE)
  })
  
  # ---- Wykres liniowy ----
  output$timeseries <- renderPlotly({
    dt <- filtered_data()
    req(input$wykresy_series)
    if(length(input$wykresy_series)==0) return(plotly::plot_ly())
    
    if(input$wykres_layout=="single"){
      p <- plot_ly()
      for(col in input$wykresy_series){
        p <- add_lines(p, x=~dt$data, y=dt[[col]], name=label_map[col], line=list(color=series_colors[col]))
      }
      p <- layout(p, xaxis=list(title="Data"), yaxis=list(title="Wartość"), legend=list(orientation="h", x=0, y=-0.2))
    } else {
      # osobne układy: subplot()
      plots <- lapply(input$wykresy_series, function(col){
        plot_ly(x=dt$data, y=dt[[col]], type="scatter", mode="lines",
                name=label_map[col], line=list(color=series_colors[col])) %>%
          layout(yaxis=list(title=label_map[col]))
      })
      p <- subplot(plots, nrows=length(plots), shareX=TRUE, titleY=TRUE) %>%
        layout(xaxis=list(title="Data"))
    }
    p
  })
  
  # ---- Boxplot ----
  output$boxplot <- renderPlotly({
    dt <- filtered_data()
    req(input$boxplot_series)
    if(length(input$boxplot_series)==0) return(plotly::plot_ly())
    
    group_col <- switch(input$boxplot_type,
                        "month"=dt$month,
                        "wday"=dt$wday,
                        "hour"=dt$hour)
    
    p <- plot_ly()
    for(col in input$boxplot_series){
      p <- add_boxplot(p, y=dt[[col]], x=group_col, name=label_map[col])
    }
    p %>% layout(yaxis=list(title="Wartość"), xaxis=list(title="Grupa"), legend=list(orientation="h", x=0, y=-0.2))
  })
  
  # ---- Statystyki ----
  output$statystyki_ui <- renderUI({
    dt <- filtered_data()
    num_cols <- names(dt)[sapply(dt,is.numeric)]
    num_cols <- num_cols[num_cols %in% names(label_map)]
    if(!length(num_cols)) return(h4("Brak danych"))
    
    stats_ui <- lapply(num_cols,function(col){
      v <- dt[[col]]
      div(style="border:1px solid #ccc; padding:10px; margin-bottom:10px; border-radius:5px;",
          h5(label_map[col]),
          fluidRow(
            column(3, paste0("Min: ", min(v, na.rm=TRUE))),
            column(3, paste0("Max: ", max(v, na.rm=TRUE))),
            column(3, paste0("Średnia: ", round(mean(v, na.rm=TRUE),2))),
            column(3, paste0("Suma: ", round(sum(v, na.rm=TRUE),2)))
          )
      )
    })
    
    plot_ui <- plotlyOutput("stat_plot", height="400px")
    tagList(stats_ui, plot_ui)
  })
  
  output$stat_plot <- renderPlotly({
    dt <- filtered_data()
    num_cols <- names(dt)[sapply(dt,is.numeric)]
    num_cols <- num_cols[num_cols %in% names(label_map)]
    if(!length(num_cols)) return(plotly::plot_ly())
    
    p <- plot_ly()
    for(col in num_cols){
      p <- add_lines(p, x=~dt$data, y=dt[[col]], name=label_map[col], line=list(color=series_colors[col]))
    }
    p
  })
  
  # ---- CSV ----
  output$download_csv <- downloadHandler(
    filename = function() paste0("pse_",Sys.Date(),".csv"),
    content = function(file) fwrite(filtered_data(), file)
  )
  
  # ---- PPTX ----
  output$download_report <- downloadHandler(
    filename = function() paste0("PSE_raport_", Sys.Date(), ".pptx"),
    content = function(file) {
      
      dt <- filtered_data()
      req(is.data.table(dt), nrow(dt) > 0)
      
      numeric_cols <- names(dt)[sapply(dt, is.numeric)]
      numeric_cols <- numeric_cols[numeric_cols %in% names(label_map)]
      if(length(numeric_cols) == 0) return(NULL)
      
      ppt <- read_pptx()
      
      # ===== SLIDE 1: TABELA =====
      ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
      ppt <- ph_with(ppt, "Tabela danych", ph_location_type("title"))
      ppt <- ph_with(ppt, flextable(dt) %>% autofit(), ph_location_type("body"))
      
      # ===== SLIDE 2: WYKRES LINIOWY =====
      p_line <- ggplot(dt, aes(x = data))
      for(col in numeric_cols){
        p_line <- p_line + geom_line(aes_string(y = col, color = shQuote(label_map[col])))
      }
      p_line <- p_line + labs(x="Data", y="Wartość", color="Seria") + theme_minimal()
      
      img_line <- tempfile(fileext = ".png")
      ggsave(img_line, p_line, width = 12, height = 6)
      
      ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
      ppt <- ph_with(ppt, "Wykres liniowy", ph_location_type("title"))
      ppt <- ph_with(ppt, external_img(img_line, width = 12, height = 6), ph_location_type("body"))
      
      # ===== SLIDE 3: BOXPLOT =====
      box_data <- rbindlist(lapply(numeric_cols, function(col){
        data.table(Seria = label_map[col], Value = dt[[col]])
      }))
      
      p_box <- ggplot(box_data, aes(x = Seria, y = Value, fill = Seria)) +
        geom_boxplot() +
        labs(x="Seria", y="Wartość") +
        theme_minimal()
      
      img_box <- tempfile(fileext = ".png")
      ggsave(img_box, p_box, width = 12, height = 6)
      
      ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
      ppt <- ph_with(ppt, "Boxplot", ph_location_type("title"))
      ppt <- ph_with(ppt, external_img(img_box, width = 12, height = 6), ph_location_type("body"))
      
      # ===== SLIDE 4: STATYSTYKI =====
      stat_dt <- rbindlist(lapply(numeric_cols, function(col){
        data.table(
          Seria = label_map[col],
          Min = min(dt[[col]], na.rm=TRUE),
          Max = max(dt[[col]], na.rm=TRUE),
          Średnia = round(mean(dt[[col]], na.rm=TRUE),2),
          Suma = round(sum(dt[[col]], na.rm=TRUE),2)
        )
      }))
      
      ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
      ppt <- ph_with(ppt, "Statystyki", ph_location_type("title"))
      ppt <- ph_with(ppt, flextable(stat_dt) %>% autofit(), ph_location_type("body"))
      
      # ===== ZAPIS =====
      print(ppt, target = file)
    }
  )
  
}

shinyApp(ui, server)
