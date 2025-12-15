# Copyright 2025 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# source("server_objects.R", local = TRUE)

shinyServer(function(input, output, session) {
  
  # ------------------------------- reactives ------------------------------------
  rv_data <- reactiveValues(sort_by = "rate",
                            yearly = indicator_summary,
                            links = NULL,
                            title = bc_title,
                            colour = "#6C878F")
  
  observeEvent(input$sort_name, {rv_data$sort_by = "name"})
  observeEvent(input$sort_rate, {rv_data$sort_by = "rate"})
  observeEvent(input$sort_population, {rv_data$sort_by = "pop"})
  
  observe({
    if(!is.null(input$plot_rd_selected) && input$plot_rd_selected != stikine){
      rv_data$yearly = indicator[which(indicator$Regional_District %in% input$plot_rd_selected),]
      rv_data$links = link[which(link$Local_Govt_Name %in% input$plot_rd_selected),]
      rv_data$colour = district$pal[which(district$Regional_District %in% input$plot_rd_selected)]
    }
  })
  
  observe({
    if(is.null(input$plot_rd_selected)){
      rv_data$yearly = indicator_summary
      rv_data$links = NULL
      rv_data$title = bc_title
      rv_data$colour = "#6C878F"
    }
  })
  
  observe({
    req(input$plot_rd_selected)
    if(input$plot_rd_selected == stikine){
      rv_data$yearly = NULL
      rv_data$links = NULL
      rv_data$title = stikine_title
    }
  })
  
  observeEvent(input$show_bc, {
    session$sendCustomMessage(type = 'plot_rd_set', message = character(0))
  })
  
  district_data <- reactive({
    district$Regional_District <- sort_data(district, rv_data$sort_by)
    district
  })
  
  # ------------------------------- render UI ------------------------------------
  
  output$ui_resources <- renderUI({
    req(rv_data$links)
    HTML("<strong>Resources:</strong>")
  })
  
  output$ui_dl <- renderUI({
    req(rv_data$links)
    data <- rv_data$links %>% prepare_links()
    lapply(1:nrow(data), function(x){
      x <- data[x,]
      actionButton(inputId = row.names(x), label = x$label,
                   onclick = paste0("window.open('", x$web, "')"),
                   class = "msw-button", icon = icon(x$icon, lib = x$lib))
    })
  })
  
  output$ui_info <- renderUI({
    if (!is.null(input$plot_rd_selected) && input$plot_rd_selected == stikine) {
      return(h2(rv_data$title))
    }
    data <- rv_data$yearly 
    data <- data[order(data$Year, decreasing = TRUE),]
    data <- data[1,]
    rd <- h2(HTML(paste0("Disposal Rates in ", data$Regional_District, " (", min_year, "-", max_year, ") "))) 
    pop <- paste(max_year, "Population:<b>", format(data$Population, big.mark = ","), "</b>")
    rate <- paste(max_year, "Disposal Rate:<b>", round(data$Disposal_Rate_kg), "kg/person</b>")
    show_bc_button <- actionButton(inputId = "show_bc", "Show British Columbia", 
                                   class = 'msw-button')
    HTML(paste0(rd, pop, spaces(4), rate, spaces(6), 
                if (data$Regional_District != "British Columbia") 
                  show_bc_button))
  })
  
  # ------------------------------- render outputs ------------------------------------
  
  output$plot_rd <- renderGirafe({
    data <- district_data()
    hline <- indicator_summary$Disposal_Rate_kg[indicator_summary$Year == max_year]
    girafe(code = print(gg_map(district) - gg_bar_rd(data, hline) + plot_layout(ncol = 2,
                                                                            widths = c(9, 4))), 
           width_svg = translate_in(p1.w), 
           height_svg = translate_in(p1.h), 
           fonts = list(sans = "Roboto")) %>%
      girafe_options(opts_selection(type = "single", 
                                    css = paste0("fill: ", hex_select, ";")),
                     opts_hover(css = paste0("fill: ", hex_hover, ";")), 
                     opts_tooltip(css = tooltip_css, opacity = 1),
                     opts_toolbar(saveaspng = FALSE))
  })
  
  output$plot_year <- renderGirafe({
    data <- rv_data$yearly
    color = rv_data$colour
    req(data)
    girafe(code = print(gg_bar_year(data, color)),
           width_svg = translate_in(p2.w), 
           height_svg = translate_in(p2.h)) %>%
      girafe_options(opts_hover(css = paste0("fill: ", hex_hover, ";")),
                     opts_tooltip(css = tooltip_css, opacity = 1, 
                                  offx = 5, offy = -80),
                     opts_selection(type = "none"),
                     opts_toolbar(saveaspng = FALSE))
  })
})