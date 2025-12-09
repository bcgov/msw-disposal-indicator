
# ------------------------------ utils
translate_px <- function(x) paste0(x, "px")
translate_in <- function(x) x/72
div_css <- function(w, h, extra = ""){
  paste("width:", translate_px(w), "; height:", translate_px(h),"; text-align: center; overflow-x: visible; overflow-y: visible;", extra)
}

# ------------------------------ ggplots
gg_map <- function(data){
  yr <- max(as.numeric(as.character(data$Year)), na.rm = TRUE)
  ggplot() +
    geom_sf_interactive(data = data, 
                        aes(tooltip = Label, 
                            data_id = Regional_District,
                            fill = Disposal_Rate_kg),
                        size = 0.3, color = "black") +
    # coord_sf(crs = 3005) +
    theme_void() +
    scale_fill_gradientn(colours = tints, na.value = hex_na, 
                         name = paste(yr, "Disposal\n(kg / person)"), 
                         breaks = seq(300, 900, by = 150), 
                         limits = c(300, 900)) +
    theme(axis.text = element_blank(),
          legend.position = c(0.2, 0.16),
          legend.title = element_text(size = txt_size), 
          legend.text = element_text(size = txt_size - 1))
}

gg_bar_rd <- function(data, hline){
  yr <- max(as.numeric(as.character(data$Year)), na.rm = TRUE)
  ggplot(data = na.omit(data) %>% as.data.frame,
         aes(x = Regional_District, 
             y = Disposal_Rate_kg,
             fill = factor(Fill),
             tooltip = Label, 
             data_id = Regional_District)) + 
    geom_bar_interactive(stat = "identity",
                         col = "black",
                         linewidth = 0.3) +
    coord_flip(clip = 'off') +
    scale_fill_manual(values = c(hex_bar1_1, 
                                  hex_bar1_2), 
                      name = "Population") +
    theme_bw() +
    scale_y_continuous(position = "right", 
                       breaks = c(0, cumsum(rep(1000/6, 6))),
                       labels = c(0, "", "", "", "", "", "1,000"), 
                       limits = c(0, 1000)) +
    scale_x_discrete(expand = expansion(0.03)) +
    labs(x = NULL, y = paste(yr, "Disposal\n(kg / person)")) +
    theme(axis.text = element_text(size = txt_size),
          legend.position = "bottom",
          legend.title = element_text(size = txt_size), 
          legend.text = element_text(size = txt_size), 
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(color = hex_axis),
          axis.line.x = element_line(color = hex_axis, size = line_size),
          panel.border = element_blank(),
          axis.title = element_text(size = txt_size),
          panel.grid = element_blank(), 
          plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
          ) +
    geom_hline(yintercept = hline, size = 0.3, color = hex_axis) +
    annotate("text", -0.2, hline + 280, 
             label = paste0("B.C. (", format(hline), " kg / person)"),
             size = 4, colour = hex_axis) 
}

gg_bar_year <- function(data, color){
  ggplot(data = data, 
         aes(x = Year, 
             y = Disposal_Rate_kg,
             tooltip = Label,
             data_id = Year)) + 
    geom_bar_interactive(stat = "identity", fill = color,
                         col = "black",
                         linewidth = 0.5) +
    scale_x_discrete(drop = FALSE, 
                     breaks = c(seq(1990, 2018, by = 4), 2023),
                     labels = c(seq(1990, 2018, by = 4), 2023),
                     expand = expansion(0.04)) +
    scale_y_continuous(expand = c(0,0), 
                       breaks = c(0, 250, 500, 750, 1000),
                       labels = c("0", "250", "500", "750", "1,000"),
                       limits = c(0, 1000)) +
    # scale_fill_manual(drop = FALSE) +
    labs(x = NULL, y = "Disposal (kg / person)") +
    theme_bw() +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = hex_axis, size = line_size),
          axis.ticks = element_line(color = hex_axis),
          axis.ticks.length=unit(.25, "cm"),
          axis.text = element_text(size = txt_size),
          axis.title = element_text(size = txt_size),
          plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
}


sort_data <- function(data, x){
  if(x == "pop"){
    return(forcats::fct_reorder(data$Regional_District, data$Population, .desc = TRUE))
  }
  if(x == "name"){
    return(forcats::fct_rev(data$Regional_District))
  }
  if(x == "rate"){
    return(forcats::fct_reorder(data$Regional_District, data$Disposal_Rate_kg, .desc = TRUE))
  }
}

prepare_links <- function(data){
  data <- t(data[c("swmPlan", "wComposition")]) %>% 
    as.data.frame()
  colnames(data) <- "web"
  data$label <- c("Solid Waste Management Plan",
                  "Waste Composition Report")
  data$icon <- c("file",
                 "stats")
  data$lib <- c("font-awesome",
                "glyphicon")
  data <- data[which(!is.na(data$web)),]
  data
}
