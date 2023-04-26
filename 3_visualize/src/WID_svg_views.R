create_map_views_svg <- function(state_fill,
                                 border_size,
                                 border_color,
                                 highlight_site_color,
                                 view,
                                 out_svg,
                                 site_info){
  
  
  
  
  if(view == "CONUS"){
    state <- ggplot2::map_data('state')
  } else if(view == "midwest"){
    state <- ggplot2::map_data('state') %>%
      filter(region %in% c("wisconsin", "michigan", "ohio", 
                           "indiana", "illinois", 
                           "kentucky"))
  }
  
  # Just needs a numeric value so that it can be mapped
  site_info$group <- 2036
  
  inset <-  
    ggplot(data = state, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill = state_fill,
                 color = border_color,
                 size = border_size) +       
    geom_point(data = site_info, aes(x = LNG_GAGE, y = LAT_GAGE), 
               color = highlight_site_color, size = 2)+
    coord_map("conic", lat0 = 30)+
    theme_void()
  
  ggsave(plot = inset, filename = out_svg, device = "svg", 
         width = 1000, height = 1000, units = "px")
}

create_axis_rectangles <- function(){
  # x-axis rectangles
  axis_rects <- tibble(
    ymax = rep(-70, 6),
    ymin = rep(-100, 6),
    xmin = seq.Date(as.Date(sprintf('01/05/%s', focal_year),'%d/%m/%Y'), 
                    as.Date(sprintf('01/10/%s', focal_year),'%d/%m/%Y'), 
                    by = '1 month'),
    xmax = xmin + lubridate::period("1 month") - lubridate::period("1 day")
  ) %>%
    # cut October off on the 14th
    mutate(xmax = case_when(xmin == as.Date(sprintf('01/10/%s', focal_year),'%d/%m/%Y') ~ 
                              as.Date(sprintf('14/10/%s', focal_year),'%d/%m/%Y'),
                            TRUE ~ xmax))
}

create_hydrograph_views_svg <- function(streamflow_df, droughts_df, axis_rects, dv_tibble, yaxis_max, out_svg){
  

  
  base_plot <- ggplot(data = streamflow_df, aes(y = value, x = dt))+
    scale_y_continuous(limits = c(-100, yaxis_max))+
    theme_void()+
    theme(axis.text.x = element_text(size = 6,
                                     color = dv_tibble$dv_basePlot_axis_text_color,
                                     vjust = 5))+
    scale_x_date(labels = date_format("%b"), 
                 date_breaks  ="1 month",
                 limits = c(as.Date(sprintf("01/05/%s", focal_year),'%d/%m/%Y'), 
                            as.Date(sprintf("15/10/%s", focal_year),'%d/%m/%Y'))) + 
    
    # Axis rectangles
    annotate("rect", 
             xmin = axis_rects$xmin,
             xmax = axis_rects$xmax,
             ymin = axis_rects$ymin, ymax = axis_rects$ymax,
             fill = dv_tibble$dv_basePlot_axis_fill_color, alpha = 1.0)+
    
    # This site's daily streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow", 
             x = as.Date(sprintf("05/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 580, color = dv_tibble$dv_streamflow_text_color, size = 2) +
    
    # Variable Threshold
    geom_line(aes(y = thresh_10_jd_07d_wndw, x = dt), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)
  
  # For the main hydrograph used in most views
  if(yaxis_max == 800){
    plot <- base_plot +
      
      # Only the very first highlighted fixed threshold drought
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
      
      # Severe drought threshold (10%)
      geom_line(aes(y = thresh_10_site), color = dv_tibble$dv_drought_threshold, 
                size = dv_tibble$dv_threshold_line_size)+
      annotate("text", label = "10% of annual average streamflow", 
               x = as.Date(sprintf("17/05/%s", focal_year),'%d/%m/%Y'), hjust = 0,
               y = 15, color = dv_tibble$dv_drought_text_color, size = 2)+
      
      # Fixed threshold droughts
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
               color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size)+
      
      # Variable threshold droughts
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
               color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) 
  }
  
  # For the zoomed out plot to explain variable threshold
  if(yaxis_max == 1400){
    plot <- base_plot +
      # Average daily streamflow
      geom_line(aes(y = mean_flow), color = dv_tibble$dv_streamflow_line_color,
                size = dv_tibble$dv_streamflow_line_size)+
      annotate("text", label = "Average daily\nstreamflow\n(1951-2020)",
               x = as.Date(sprintf("17/07/%s", focal_year),'%d/%m/%Y'), hjust = 0,
               y = 800, color = dv_tibble$dv_streamflow_text_color, size = 2)+
      
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
               color = dv_tibble$dv_circle_explainer, size = 2)
  }
  
  ggsave(plot = plot, filename = out_svg, device = "svg", 
         width = 1000, height = 1000, units = "px")
}






create_zoomed_explainer_svg <- function(streamflow_df, dv_tibble, out_svg){
  
  fill_drought_bottom <- streamflow_df %>%
    filter(dt >= as.Date(sprintf("15/08/%s", focal_year),'%d/%m/%Y'), 
           dt <= as.Date(sprintf("28/08/%s", focal_year),'%d/%m/%Y')) %>%
    select(dt, value)
  
  fill_drought_top <- fill_drought_bottom %>%
    mutate(value = 45) %>%
    arrange(-yday(dt))
  
  fill_drought <- bind_rows(fill_drought_bottom, fill_drought_top) %>%
    mutate(id = 1)
  
  zoom_plot <- ggplot(data = streamflow_df, aes(y = value, x = dt))+
    ylim(c(0,100))+
    scale_x_date(labels = date_format("%b"), 
                 date_breaks  ="1 month",
                 limits = c(as.Date(sprintf("10/08/%s", focal_year),'%d/%m/%Y'), 
                            as.Date(sprintf("02/09/%s", focal_year),'%d/%m/%Y')))+
    theme_void()+
    
    # Drought fill: Have to add 0.4 before and after dates to fill corners 
    annotate("ribbon", x = c(min(fill_drought_bottom$dt)-0.4, fill_drought_bottom$dt, max(fill_drought_bottom$dt)+0.4),
             ymin = c(45,fill_drought_bottom$value, 45),
             ymax = c(45,fill_drought_top$value, 45),
             fill = dv_tibble$dv_drought_fill)+
    
    # Hydrograph elements
    geom_line(color = dv_tibble$dv_streamflow_line_color, 
              size = dv_tibble$dv_streamflow_line_size)+
    geom_line(aes(y = thresh_10_site), 
              color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size)+
    
    # Labels
    annotate("text", label = "Daily\n  streamflow", 
             x = as.Date(sprintf("13/08/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 86, color = dv_tibble$dv_streamflow_text_color, size = 2)+
    annotate("text", label = "Drought threshold", 
             x = as.Date(sprintf("15/08/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 52, color = dv_tibble$dv_drought_text_color, size = 1.8)+
    annotate("text", label = "Drought", 
             x = as.Date(sprintf("23/08/%s", focal_year),'%d/%m/%Y')+0.5, hjust = 0.5,
             y = 25, color = dv_tibble$dv_streamflow_text_color, size = 1.8)
  
  # Use cowplot to add in bounding circle
  circle <- grid::circleGrob(gp = grid::gpar(color = dv_tibble$dv_circle_explainer))
  
  # Background
  color_bknd = "transparent"
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 12,
    gp = grid::gpar(fill = color_bknd, alpha = 1, col = color_bknd)
  )
  
  # start plot with canvas
  plot <- ggdraw(ylim = c(0,1), 
                 xlim = c(0,1)) +
    # fill in the background
    draw_grob(canvas,
              x = 0.25, y = 1,
              height = 12, width = 16,
              hjust = 0, vjust = 1) +
    draw_plot(zoom_plot,
              x = 0.1,
              y = 0.05, 
              height = 0.85,
              width = 0.85) +
    draw_grob(circle, scale = 0.9, 
              y = 0, x = 0)
  
  ggsave(plot = plot, filename = out_svg, device = "svg", 
         width = 1000, height = 1000, units = "px")
  
}





create_stacked_hydrograph_svg <- function(streamflow_df, 
                                          droughts_df,
                                          droughts_70yr_site_df, 
                                          droughts_70yr_j7_df, 
                                          axis_rects,
                                          dv_tibble, 
                                          out_svg){
  

  
  base_plot <- ggplot(data = streamflow_df, aes(y = value, x = dt))+
    scale_y_continuous(limits = c(-100, 800))+
    theme_void()+
    theme(axis.text.x = element_text(size = 6,
                                     color = dv_tibble$dv_basePlot_axis_text_color,
                                     vjust = 5))+
    scale_x_date(labels = date_format("%b"), 
                 date_breaks  ="1 month",
                 limits = c(as.Date(sprintf("01/05/%s", focal_year),'%d/%m/%Y'), 
                            as.Date(sprintf("15/10/%s", focal_year),'%d/%m/%Y'))) + 
    
    # Axis rectangles
    annotate("rect", 
             xmin = axis_rects$xmin,
             xmax = axis_rects$xmax,
             ymin = axis_rects$ymin, ymax = axis_rects$ymax,
             fill = dv_tibble$dv_basePlot_axis_fill_color, alpha = 1.0)+
    
    # This site's daily streamflow
    geom_line(color = dv_tibble$dv_streamflow_line_color, size = dv_tibble$dv_streamflow_line_size)+
    annotate("text", label = "Daily\nstreamflow", 
             x = as.Date(sprintf("05/06/%s", focal_year),'%d/%m/%Y'), hjust = 0,
             y = 580, color = dv_tibble$dv_streamflow_text_color, size = 2) 
  
  
  fixed_plot <- base_plot + 
    
    # Fixed Threshold
    geom_line(aes(y = thresh_10_site, x = dt), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size) +
    
    # Fixed threshold droughts for FOCAL YEAR
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
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size)+
    
    # Fixed threshold for All 70 years
    annotate("rect", # fixed threshold
             xmin = droughts_70yr_site_df$start_fakeYr,
             xmax = droughts_70yr_site_df$end_fakeYr,
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # fixed threshold
             xmin = droughts_70yr_site_df$start_fakeYr,
             xmax = droughts_70yr_site_df$end_fakeYr,
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill, alpha = 0.2,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) 
  
  variable_plot <- base_plot + 
    
    # Variable Threshold
    geom_line(aes(y = thresh_10_jd_07d_wndw, x = dt), color = dv_tibble$dv_drought_threshold, 
              size = dv_tibble$dv_threshold_line_size) +
    
    # Variable threshold droughts for focal year only
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
    
    # Variable threshold droughts for all 70 years
    annotate("rect", # variable threshold
             xmin = droughts_70yr_j7_df$start_fakeYr,
             xmax = droughts_70yr_j7_df$end_fakeYr,
             ymin = -100, ymax = -70,
             fill = dv_tibble$dv_drought_fill, alpha = 1.0,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size) +
    annotate("rect", # variable threshold
             xmin = droughts_70yr_j7_df$start_fakeYr,
             xmax = droughts_70yr_j7_df$end_fakeYr,
             ymin = -30, ymax = 700,
             fill = dv_tibble$dv_drought_fill, alpha = 0.2,
             color = dv_tibble$df_fill_outline_color, size = dv_tibble$dv_fill_outline_size)
  
  plot <- ggdraw(ylim = c(0,1), 
                 xlim = c(0,1)) +
    draw_plot(variable_plot,
              x = 0, 
              y = 0,
              height = 0.5,
              width = 1)+
    draw_plot(fixed_plot, 
              x = 0,
              y = 0.5,
              height = 0.5,
              width = 1)
  
  ggsave(plot = plot, filename = out_svg, device = "svg", 
         width = 1000, height = 1000, units = "px")
}





