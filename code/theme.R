
theme_xinavane <- 
  function(base_family = "Arial") {
    require(RColorBrewer)
    
    # Generate the colors for the chart procedurally with RColorBrewer
    # palette <- brewer.pal("Greys", n=9)
    palette <- colorRampPalette(c('white', 'white', 'black'))(9)
    color_background = palette[2]
    color_grid_major = palette[6]
    color_axis_text = palette[8]
    color_axis_title = palette[8]
    color = palette[8]
    color_title = palette[9]
    
    # Begin construction of chart
    theme_bw() +
      
      # Set the entire chart region to seasheel
      theme(panel.background=element_rect(fill=color_background, 
                                          color=color_background)) +
      theme(plot.background=element_rect(fill=color_background, 
                                         color=color_background)) +
      theme(panel.border=element_rect(color=color_background)) +
      
      # Format the grid
      theme(panel.grid.major=element_line(color=color_grid_major,
                                          size=.25)) +
      theme(panel.grid.minor=element_blank()) +
      theme(axis.ticks=element_blank()) +
      
      # Format the legend, but hide by default
      # theme(legend.position="none") +
      theme(legend.background = element_rect(fill=color_background)) +
      theme(legend.text = element_text(family = base_family,
                                       color=color_axis_title)) +
      
      # Set title and axis labels, and format these and tick marks
      theme(plot.title=element_text(family = base_family,
                                    color=color_title, 
                                    vjust=1.25)) +
      theme(axis.text.x=element_text(family = base_family,
                                     color=color_axis_text)) +
      theme(axis.text.y=element_text(family = base_family,
                                     color=color_axis_text)) +
      theme(axis.title.x=element_text(family = base_family,
                                      color=color_axis_title, 
                                      vjust=0)) +
      theme(axis.title.y=element_text(family = base_family,
                                      color=color_axis_title, 
                                      vjust=1.25)) +
      
      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) +
      theme(complete = TRUE)
  }