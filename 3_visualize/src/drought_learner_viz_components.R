blank_plot <- function(streamflow_df, dv_tibble, growing_season = T){
  
  
  axis_rects_year <- tibble(
    ymax = rep(-70, 12),
    ymin = rep(-100, 12),
    xmin = seq.Date(as.Date(sprintf('01/01/%s', focal_year),'%d/%m/%Y'), 
                    as.Date(sprintf('01/12/%s', focal_year),'%d/%m/%Y'), 
                    by = '1 month'),
    xmax = xmin + lubridate::period("1 month")-lubridate::period("1 day")
  )
  
  axis_rects <- axis_rects_year %>%
    filter(xmin <= as.Date(sprintf('01/10/%s', focal_year),'%d/%m/%Y'),
           xmin >= as.Date(sprintf('01/05/%s', focal_year),'%d/%m/%Y')) %>%
    # cut October off on the 14th
    mutate(xmax = case_when(xmin == as.Date(sprintf('01/10/%s', focal_year),'%d/%m/%Y') ~ 
                              as.Date(sprintf('14/10/%s', focal_year),'%d/%m/%Y'),
                            TRUE ~ xmax))
  
  blank_plot <- ggplot(data = streamflow_df, aes(y = value, x = dt))+
    # Axis bars
    ylab("Streamflow\n(cfs)")+
    xlab(NULL)+
    scale_y_continuous(limits = c(-100, 800), 
                       labels = c(0, 800),
                       breaks = c(0, 800))+
    theme_tufte(base_family = "sans", base_size = 16)+
    theme(axis.ticks = element_blank(),
          axis.text = element_text(size = 6,
                                   color = dv_tibble$dv_basePlot_axis_text_color),
          axis.text.x = element_text(vjust = 10),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank())
  
  
  {if(growing_season == TRUE){
    blank_plot <- blank_plot + 
      scale_x_date(labels = date_format("%b"), 
                   date_breaks  ="1 month",
                   limits = c(as.Date(sprintf("01/05/%s", focal_year),'%d/%m/%Y'), 
                              as.Date(sprintf("15/10/%s", focal_year),'%d/%m/%Y')))+
      annotate("rect", 
               xmin = axis_rects$xmin,
               xmax = axis_rects$xmax,
               ymin = axis_rects$ymin, ymax = axis_rects$ymax,
               fill = dv_tibble$dv_basePlot_axis_fill_color, alpha = 1.0) 
  } else {
    blank_plot <- blank_plot + 
      scale_x_date(labels = date_format("%b"), 
                   date_breaks  ="1 month",
                   limits = c(as.Date(sprintf("01/01/%s", focal_year),'%d/%m/%Y'), 
                              as.Date(sprintf("31/12/%s", focal_year),'%d/%m/%Y')))+
      annotate("rect", 
               xmin = axis_rects_year$xmin,
               xmax = axis_rects_year$xmax,
               ymin = axis_rects_year$ymin, ymax = axis_rects_year$ymax,
               fill = dv_tibble$dv_basePlot_axis_fill_color, alpha = 1.0) 
  }}
  
  
  
  return(blank_plot)
}


inset_map <- function(state_fill,
                      border_size,
                      border_color,
                      highlight_site_color,
                      view,
                      site_info){
  
  

  
  if(view == "CONUS"){
    state <- ggplot2::map_data('state')
  } else if(view == "midwest"){
    state <- ggplot2::map_data('state') %>%
      filter(region %in% c("wisconsin", "michigan", "ohio", 
                           "indiana", "illinois", 
                           "kentucky"))
  }
  
  # Just needs a value so that it can be mapped
  site_info$group <- 2036
  
  inset <-  
    ggplot(data = state, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill = state_fill,
                 color = border_color,
                 size = border_size) +       
    geom_point(data = site_info, aes(x = LNG_GAGE, y = LAT_GAGE), 
               color = highlight_site_color, size = 0.5)+
    coord_map("conic", lat0 = 30)+
    theme_void()
  
  return(inset)
}




add_core_plot_elements <- function(text, ...) {
  
  
  # Background
  color_bknd = "transparent"
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 12,
    gp = grid::gpar(fill = color_bknd, alpha = 1, col = color_bknd)
  )
  
  # pull out passed elements into list
  elements <- list(...)
  
  # set positioning for each element
  element_positioning <- tibble(
    element = c('main', 'inset'),
    x = c(0, 0.8),
    y = c(0.1, 0.8),
    height = c(1, 0.2),
    width = c(1, 0.2)
  )
  
  # start plot with canvas
  plot <- ggdraw(ylim = c(0,1), 
                 xlim = c(0,1)) +
    # fill in the background
    draw_grob(canvas,
              x = 0.25, y = 1,
              height = 12, width = 16,
              hjust = 0, vjust = 1)
  
  # add each of the passed elements
  for (element in names(elements)) {
    element_position <- filter(element_positioning, element==!!element)
    plot <- plot + draw_plot(elements[[element]], 
                             x= element_position$x, 
                             y= element_position$y, 
                             height= element_position$height,
                             width = element_position$width)
  }
  
  # add text w/ frame name
  plot +
    draw_text(text,
              x = 0.05, 
              y = 0.95,
              size = 4)
  
  
}











frame_a <- function(blank_plot, streamflow_df, droughts_df,
                    canvas, inset, out_png, dv_tibble){
  
  
  
  main <- blank_plot +
    # Fixed threshold drought durations
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "fixed" & droughts_df$drought_id == 76],
             xmax = droughts_df$end[droughts_df$method == "fixed" & droughts_df$drought_id == 76],
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "fixed" & droughts_df$drought_id == 76],
             xmax = droughts_df$end[droughts_df$method == "fixed" & droughts_df$drought_id == 76],
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("text", label = "Period of\ndrought", 
             x = as.Date(sprintf("20/07/%s", focal_year),'%d/%m/%Y'), hjust = 0.5,
             y = 1200, color = dv_tibble$dv_drought_text_color, size = 2) + 
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow", 
             x = as.Date(sprintf("05/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 580, color = dv_tibble$dv_streamflow_text_color, size = 2)+
    theme(axis.text.x = element_text(size = 4))+
    scale_x_date(labels = date_format("%b"), 
                 date_breaks  ="1 month",
                 limits = c(as.Date(sprintf("01/05/%s", focal_year),'%d/%m/%Y'), 
                            as.Date(sprintf("15/10/%s", focal_year),'%d/%m/%Y')))
  
  inset_zoom <- inset +
    xlim(c(0,2516293)) +
    ylim(c(-1250000,731650))
  
  
  circle <- grid::circleGrob(gp = grid::gpar(color = dv_tibble$dv_circle_explainer))
  
  plot <- add_core_plot_elements(text = 'frame a')
  
  plot +
    draw_plot(inset,
              x = -0.1,
              y = 0.2,
              width = 0.6,
              height = 0.7)+
    draw_plot(main,
              x = 0.4,
              y = 0.1,
              width = 0.6,
              height = 0.5)+
    draw_grob(circle, scale = 0.15, y = -0.24, x = 0.29)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}




frame_b <- function(blank_plot, streamflow_df, droughts_df,
                    canvas, inset, out_png, dv_tibble){
  
  
  fill_drought_bottom <- streamflow_df %>%
    filter(dt >= as.Date(sprintf("15/08/%s", focal_year),'%d/%m/%Y'), 
           dt <= as.Date(sprintf("28/08/%s", focal_year),'%d/%m/%Y')) %>%
    select(dt, value)
  
  fill_drought_top <- fill_drought_bottom %>%
    mutate(value = 45) %>%
    arrange(-yday(dt))
  
  fill_drought <- bind_rows(fill_drought_bottom, fill_drought_top) %>%
    mutate(id = 1)
  
  mask_poly <- data.frame(
    id = rep(1, 8),
    subid = rep(1:2,each = 4),
    x = c(as.Date(sprintf("01/05/%s", focal_year),'%d/%m/%Y'), 
          as.Date(sprintf("01/05/%s", focal_year),'%d/%m/%Y'), 
          as.Date(sprintf("15/10/%s", focal_year),'%d/%m/%Y'), 
          as.Date(sprintf("15/10/%s", focal_year),'%d/%m/%Y'),
          as.Date(sprintf("15/08/%s", focal_year),'%d/%m/%Y'), 
          as.Date(sprintf("15/08/%s", focal_year),'%d/%m/%Y'), 
          as.Date(sprintf("28/08/%s", focal_year),'%d/%m/%Y'), 
          as.Date(sprintf("28/08/%s", focal_year),'%d/%m/%Y')),
    y = c(-Inf, Inf, Inf, -Inf,
          0, 100, 100, 0)
  )
  
  main <- blank_plot +
    # Fixed threshold drought durations
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "fixed" & droughts_df$drought_id == 76],
             xmax = droughts_df$end[droughts_df$method == "fixed" & droughts_df$drought_id == 76],
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "fixed" & droughts_df$drought_id == 76],
             xmax = droughts_df$end[droughts_df$method == "fixed" & droughts_df$drought_id == 76],
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill, 
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("text", label = "Period of\ndrought", 
             x = as.Date(sprintf("20/07/%s", focal_year),'%d/%m/%Y'), hjust = 0.5,
             y = 1200, color = dv_tibble$dv_drought_text_color, size = 2) + 
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow", 
             x = as.Date(sprintf("05/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 580, color = dv_tibble$dv_streamflow_text_color, size = 2)+
    theme(axis.text.x = element_text(size = 4))
  
  # Zoomed in plot
  zoom_plot <- ggplot(data = streamflow_df, aes(y = value, x = dt))+
    geom_polygon(data = fill_drought, aes(group = id, x = dt, y = value),
                 fill = dv_tibble$dv_drought_fill)+
    geom_line(color = dv_tibble$dv_streamflow_line_color, 
              size = dv_tibble$dv_streamflow_line_size)+
    geom_line(aes(y = thresh_10_site), 
              color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)+
    ylab(NULL)+
    xlab(NULL)+
    ylim(c(0,100))+
    scale_x_date(labels = date_format("%b"), 
                 date_breaks  ="1 month",
                 limits = c(as.Date(sprintf("10/08/%s", focal_year),'%d/%m/%Y'), 
                            as.Date(sprintf("02/09/%s", focal_year),'%d/%m/%Y')))+
    theme_tufte(base_family = "sans", base_size = 16)+
    theme(axis.line = element_line(color = "transparent"),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank())+
    annotate("text", label = "Daily\n  streamflow", 
             x = as.Date(sprintf("13/08/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 86, color = dv_tibble$dv_streamflow_text_color, size = 2)+
    annotate("text", label = "Drought threshold", 
             x = as.Date(sprintf("15/08/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 52, color = dv_tibble$dv_drought_text_color, size = 1.8)+
    annotate("text", label = "Drought", 
             x = as.Date(sprintf("23/08/%s", focal_year),'%d/%m/%Y')+0.5, hjust = 0.5,
             y = 25, color = dv_tibble$dv_streamflow_text_color, size = 1.8)
  
  circle <- grid::circleGrob(gp = grid::gpar(color = dv_tibble$dv_circle_explainer))
  
  plot <- add_core_plot_elements(text = 'frame b')
  
  plot +
    draw_plot(inset,
              x = -0.1,
              y = 0.2,
              width = 0.6,
              height = 0.7)+
    draw_plot(main,
              x = 0.4,
              y = 0.1,
              width = 0.6,
              height = 0.5)+
    draw_plot(zoom_plot,
              x = 0.61,
              y = 0.5, 
              height = 0.4,
              width = 0.35)+
    draw_grob(circle, scale = 0.15, y = -0.24, x = 0.29)+
    draw_grob(circle, scale = 0.42, y = 0.24, x = 0.29)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}


frame_c <- function(blank_plot, streamflow_df, droughts_df,
                    canvas, inset, out_png, dv_tibble){
  
  
  main <- blank_plot +
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow", 
             x = as.Date(sprintf("03/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 420, color = dv_tibble$dv_streamflow_text_color, size = 2)
  
  
  
  add_core_plot_elements(text = 'frame c', 
                         main = main, inset = inset)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}


frame_d <-  function(blank_plot, streamflow_df, droughts_df,
                     canvas, inset, out_png, dv_tibble){
  
  
  main <- blank_plot +
    # Severe drought threshold (10%)
    geom_line(aes(y = thresh_10_site), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)+
    annotate("text", label = "10% of annual average streamflow", 
             x = as.Date(sprintf("17/05/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 15, color = dv_tibble$dv_drought_text_color, size = 2)+
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow", 
             x = as.Date(sprintf("03/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 420, color = dv_tibble$dv_streamflow_text_color, size = 2)
  
  
  add_core_plot_elements(text = 'frame d', 
                         main = main, inset = inset)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}




frame_e <- function(blank_plot, streamflow_df, droughts_df,
                    canvas, inset, out_png, dv_tibble){
  
  main <- blank_plot +
    # Fixed threshold drought durations
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "fixed"],
             xmax = droughts_df$end[droughts_df$method == "fixed"],
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "fixed"],
             xmax = droughts_df$end[droughts_df$method == "fixed"],
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill, alpha = 0.5,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    # Severe drought threshold (10%)
    geom_line(aes(y = thresh_10_site), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)+
    annotate("text", label = "10% of annual average streamflow", 
             x = as.Date(sprintf("17/05/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 15, color = dv_tibble$dv_drought_text_color, size = 2)+
    
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow", 
             x = as.Date(sprintf("03/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 420, color = dv_tibble$dv_streamflow_text_color, size = 2)
  
  
  
  add_core_plot_elements(text = 'frame e', 
                         main = main, inset = inset)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}

frame_f <- function(blank_plot, streamflow_df, droughts_df,
                    canvas, inset, out_png, dv_tibble){
  
  
  main <- blank_plot +
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow",
             x = as.Date(sprintf("03/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 420, color = dv_tibble$dv_streamflow_text_color, size = 2)+
    # Average daily streamflow
    geom_line(aes(y = mean_flow), color = dv_tibble$dv_streamflow_line_color, 
              size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Average daily\nstreamflow\n(1951-2020)",
             x = as.Date(sprintf("17/07/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 800, color = dv_tibble$dv_streamflow_text_color, size = 2)+
    ylim(c(-100,1400))
  
  
  
  
  add_core_plot_elements(text = 'frame f', 
                         main = main, inset = inset)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}

frame_g <- function(blank_plot, streamflow_df, droughts_df,
                    canvas, inset, out_png, dv_tibble){
  main <- blank_plot +
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_fade_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow",
             x = as.Date(sprintf("03/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 420, color = dv_tibble$dv_streamflow_fade_color, size = 2)+
    # Average daily streamflow
    geom_line(aes(y = mean_flow), color = dv_tibble$dv_streamflow_fade_color,
              size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Average daily\nstreamflow\n(1951-2020)",
             x = as.Date(sprintf("17/07/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 800, color = dv_tibble$dv_streamflow_fade_color, size = 2)+
    # Circle explainer
    annotate("point", x = as.Date(sprintf("13/05/%s", focal_year),'%d/%m/%Y'), y = 110,
             size = 3, pch = 1, color = dv_tibble$dv_circle_explainer)+
    annotate("point", x = as.Date(sprintf("13/05/%s", focal_year),'%d/%m/%Y'), y = 1210,
             size = 3, pch = 1, color = dv_tibble$dv_circle_explainer)+
    annotate("segment", x = as.Date(sprintf("13/05/%s", focal_year),'%d/%m/%Y'),
             xend = as.Date(sprintf("13/05/%s", focal_year),'%d/%m/%Y'),
             y = 1100, yend = 200, color = dv_tibble$dv_circle_explainer,
             arrow = arrow(length = unit(0.1, "cm")))+
    annotate("text", label = "Typical daily streamflow",
             x = as.Date(sprintf("14/05/%s", focal_year),'%d/%m/%Y'), y = 1280, hjust = 0,
             color = dv_tibble$dv_circle_explainer, size = 2)+
    annotate("text", label = "Actual daily streamflow",
             x = as.Date(sprintf("14/05/%s", focal_year),'%d/%m/%Y'), y = 10, hjust = 0,
             color = dv_tibble$dv_circle_explainer, size = 2)+
    ylim(c(-100,1400))
  
  
  
  add_core_plot_elements(text = 'frame g', 
                         main = main, inset = inset)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}


frame_h <- function(blank_plot, streamflow_df, droughts_df,
                    canvas, inset, out_png, dv_tibble){
  
  
  main <- blank_plot +
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_fade_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow",
             x = as.Date(sprintf("03/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 420, color = dv_tibble$dv_streamflow_fade_color, size = 2)+
    # Average daily streamflow
    geom_line(aes(y = mean_flow), color = dv_tibble$dv_streamflow_fade_color,
              size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Average daily\nstreamflow\n(1951-2020)",
             x = as.Date(sprintf("17/07/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 800, color = dv_tibble$dv_streamflow_fade_color, size = 2)+
    # Variable Threshold
    geom_line(aes(y = thresh_10_jd_07d_wndw, x = dt), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)+
    annotate("text", label = "10% of average daily streamflow", 
             x = as.Date(sprintf("17/05/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 15, color = dv_tibble$dv_drought_threshold, size = 2)+
    ylim(c(-100,1400))
  
  
  
  add_core_plot_elements(text = 'frame h', 
                         main = main, inset = inset)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}

frame_i <- function(blank_plot, streamflow_df, droughts_df,
                    canvas, inset, out_png, dv_tibble){
  
  
  main <- blank_plot +
    # Variable threshold drought durations
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "variable"],
             xmax = droughts_df$end[droughts_df$method == "variable"],
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "variable"],
             xmax = droughts_df$end[droughts_df$method == "variable"],
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill, alpha = 0.5,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    # Variable Threshold
    geom_line(aes(y = thresh_10_jd_07d_wndw, x = dt), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)+
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)
  
  
  
  
  
  add_core_plot_elements(text = 'frame i', 
                         main = main, inset = inset)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}

frame_j <- function(blank_plot, streamflow_df, droughts_df,
                    canvas, inset, out_png, dv_tibble){
  
  
  variable <- blank_plot +
    # Variable threshold drought durations
    annotate("rect", # variable threshold
             xmin = droughts_df$start[droughts_df$method == "variable"],
             xmax = droughts_df$end[droughts_df$method == "variable"],
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # variable threshold
             xmin = droughts_df$start[droughts_df$method == "variable"],
             xmax = droughts_df$end[droughts_df$method == "variable"],
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill, alpha = 0.5,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    # Variable Threshold
    geom_line(aes(y = thresh_10_jd_07d_wndw, x = dt), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)+
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)
  
  
  
  fixed <- blank_plot +
    # Fixed threshold drought durations
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "fixed"],
             xmax = droughts_df$end[droughts_df$method == "fixed"],
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # fixed threshold
             xmin = droughts_df$start[droughts_df$method == "fixed"],
             xmax = droughts_df$end[droughts_df$method == "fixed"],
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill, alpha = 0.5,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    # Variable Threshold
    geom_line(aes(y = thresh_10_site, x = dt), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)+
    # This site's 1963 streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)
  
  

  plot <- ggdraw(ylim = c(0,1), 
                 xlim = c(0,1)) +
    draw_plot(variable,
              x = 0, 
              y = 0,
              height = 0.5,
              width = 1)+
    draw_plot(fixed, 
              x = 0,
              y = 0.5,
              height = 0.5,
              width = 1)
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}




frame_k <- function(blank_plot, 
                    streamflow_df, 
                    droughts_df,
                    droughts_70yr_site_df, 
                    droughts_70yr_j7_df, 
                    year_max,
                    canvas, 
                    out_png,
                    dv_tibble,
                    inset){
  
  
  variable <- blank_plot +
    # Variable threshold drought durations
    annotate("rect", # variable threshold
             xmin = droughts_70yr_j7_df$start_fakeYr[droughts_70yr_j7_df$end_year <= year_max],
             xmax = droughts_70yr_j7_df$end_fakeYr[droughts_70yr_j7_df$end_year <= year_max],
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # variable threshold
             xmin = droughts_70yr_j7_df$start_fakeYr[droughts_70yr_j7_df$end_year <= year_max],
             xmax = droughts_70yr_j7_df$end_fakeYr[droughts_70yr_j7_df$end_year <= year_max],
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill, alpha = 0.2,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    # Variable Threshold
    geom_line(aes(y = thresh_10_jd_07d_wndw, x = dt), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)
  
  
  
  fixed <- blank_plot +
    # Fixed threshold drought durations
    annotate("rect", # fixed threshold
             xmin = droughts_70yr_site_df$start_fakeYr[droughts_70yr_site_df$end_year <= year_max],
             xmax = droughts_70yr_site_df$end_fakeYr[droughts_70yr_site_df$end_year <= year_max],
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # fixed threshold
             xmin = droughts_70yr_site_df$start_fakeYr[droughts_70yr_site_df$end_year <= year_max],
             xmax = droughts_70yr_site_df$end_fakeYr[droughts_70yr_site_df$end_year <= year_max],
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill, alpha = 0.2,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    # Fixed Threshold
    geom_line(aes(y = thresh_10_site, x = dt), color = dv_tibble$dv_drought_threshold,
              size = dv_tibble$dv_threshold_line_size)
  
  
  
  plot <- ggdraw(ylim = c(0,1), 
                 xlim = c(0,1)) +
    draw_plot(variable,
              x = 0, 
              y = 0,
              height = 0.5,
              width = 1)+
    draw_plot(fixed, 
              x = 0,
              y = 0.5,
              height = 0.5,
              width = 1)+
    draw_text(sprintf("1951-%s", year_max),
              y = 0.95,
              x = 0.01,
              size = 12,
              hjust = 0)
  
  
  
  ggsave(out_png, width = dv_tibble$dv_png_width, 
         height = dv_tibble$dv_png_height, dpi = 300, units = "px")
}


