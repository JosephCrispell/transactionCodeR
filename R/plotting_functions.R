#' Plot values by month
#'
#' Given dataframe with months in one column and values in other column(s),
#' this function will plot the values against the months
#' @param data dataframe with month and value columns
#' @param month_column column with months in. Defaults to "month"
#' @param y_axis_label label for Y axis. Defaults to "£"
#' @param title title for plot. Defaults to "Values by month"
#' @param point_shape shape for points on line. Defaults to 19 (circle)
#' @param add_zero_line boolean value indicating whether to add horizontal line at zero on Y axis. Defaults to TRUE.
#' @param margins four value vector for margin sizes (bottom, left, top, right). Defaults to c(5.1, 4.1, 4.1, 8)
#' @param legend_cex scaling value for legend size. Defaults to 1.
plot_values_by_month_static <- function(
  data, month_column = "month", y_axis_label = "£",
  title = "Values by month", point_shape = 19, add_zero_line = TRUE,
  margins = c(5.1, 4.1, 4.1, 8), legend_cex = 1
){
  
  # Get and set plotting margins
  current_mar <- par()$mar
  par(mar = margins)
  
  # Get months and month column index
  column_names <- colnames(data)
  month_column_index <- which(column_names == month_column)
  months <- data[, month_column]
  
  # Calculate Y axis limits
  y_limits <- range(data[, -month_column_index], na.rm = TRUE)
  
  # Define colours for each column of values
  colours <- rainbow(ncol(data) - 1)
  
  # Create an empty plot
  plot(x=NA, y=NA,
       xlim=c(0.25, length(months) - 0.5), 
       ylim=y_limits,
       bty="n", las=1, xaxt = "n", xlab="", 
       ylab=y_axis_label, main = title)
  
  # Add X axis
  x_ticks <- seq(0.5, length(months) - 0.5)
  axis(side = 1, at = x_ticks, labels = months)
  
  # Add line for each set of values
  for(column_index in seq_len(ncol(data))[-month_column_index]){
    
    points(x=x_ticks, y=data[, column_index],
           type = "o", pch = point_shape,
           col = colours[column_index - 1])
  }
  
  # Add legend
  legend(x = x_ticks[length(x_ticks)] + 0.05, y = y_limits[2],
         legend = column_names[-month_column_index],
         col = colours,
         lty = 1, pch = point_shape,
         bty = "n", xpd = TRUE, cex = legend_cex)
  
  # Add zero line if requested
  if(add_zero_line){
    lines(x=x_ticks, y=rep(0, length(months)), lty = 2)
  }
  
  # Reset plotting margins
  par(mar = current_mar)
}

#' Plot values by month - interactive
#'
#' Given dataframe with months in one column and values in other column(s),
#' this function will plot the values against the months using plotly
#' @param data dataframe with month and value columns
#' @param month_column column with months in. Defaults to "month"
#' @param y_axis_label label for Y axis. Defaults to "£"
#' @param title title for plot. Defaults to "Values by month"
plot_values_by_month_interactive <- function(
  data, month_column = "month", y_axis_label = "£",
  title = "Values by month"
){
  
  # Get months and month column index
  column_names <- colnames(data)
  month_column_index <- which(column_names == month_column)
  months <- data[, month_column]
  
  # Define X axis ticks
  x_ticks <- seq(0.5, length(months) - 0.5)
  
  # Create the initial plot
  fig <- plot_ly(type = 'scatter', mode = 'lines')
  
  # Add X axis
  fig <- layout(
    fig,
    xaxis = list(
      ticktext = as.list(months), 
      tickvals = as.list(x_ticks),
      tickmode = "array"
    )
  )
  
  # Add title and Y axis label
  fig <- layout(
    fig, 
    title = title,
    yaxis = list(title = y_axis_label)
  )
  
  # Add line for each set of values
  for(column_index in seq_len(ncol(data))[-month_column_index]){
    
    fig <- add_trace(
      fig,
      x = x_ticks,
      y = data[, column_index], 
      name = column_names[column_index], 
      mode = 'lines+markers',
      hovertemplate = '<b>%{x}<b>: £%{y}'
    )
  }
  
  # Show the plot
  fig
}

#' Plot horizontal bar chart of values (prices)
#'
#' Creates horizontal bar chart of values
#' @param values values (length) for each bar
#' @param names names for each bar
#' @param ... additional parameters to pass to barplot()
plot_values_bar_chart_static <- function(values, names, ...){
  
  # Get and set plotting margins
  current_mar <- par()$mar
  par(mar=c(5.1, 8, 4.1, 2.1))

  # Plot the values
  barplot(
    height = average_by_type$difference,
    names = average_by_type$Type,
    horiz = TRUE, las = 1, xlab = "£", ...
  )

  # Reset plotting margins
  par(mar=current_mar)
}

#' Plot horizontal bar chart of values (prices)
#'
#' Creates horizontal bar chart of values using plotly
#' @param values values (length) for each bar
#' @param names names for each bar
#' @param title title for plot
plot_values_bar_chart_interactive <- function(values, names, title, ...){
  
  # Create the bar chart
  fig <- plot_ly(
    x = values, 
    y = names, 
    type = 'bar', orientation = 'h',
    hovertemplate = '<b>%{y}<b>: £%{x}',
    name = ""
  )
  
  # Add title, x axis label, order names and set size
  fig <- layout(
    fig, 
    title = title,
    xaxis = list(title = "£"),
    yaxis = list(tickfont = list(size = 10),
                 categoryorder = "total ascending")
  )
  
  # Show the plot
  fig
}
