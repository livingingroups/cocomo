library(data.table)
library(ggplot2)
library(gganimate)


plot_headings <-  function(x,y,h_s, animate = TRUE){
  speeds <- h_s[['speeds']]
  heads <- h_s[['heads']]
  dts <- h_s[['dts']]
  n <- length(x)

  static <- data.table(
    x = x,
    y = y,
    t = 1:n
  )

  arrows <- data.table(
    frame = 1:n,
    xstart = x[1:n],
    ystart = y[1:n],
    xend = x[1:n+dts],
    yend = y[1:n+dts]
  )

  refs <- data.table(
      frame = 1:length(x),
      xstart = x,
      ystart = y,
      r_xend = x + speeds*dts,
      r_yend = y
  )

  res = 100
  o_theta = seq(-pi, pi, length.out = res)

  shade <- data.table(
      frame = rep(1:length(x), each = res+3),
      b = rep(c(11, 13, o_theta, 12), length(x))
  )

  shade <- shade[
  (
    b * heads[frame] > 0 &
    abs(b) < abs(heads[frame])
  )| b > 10]

  shade[
    !is.na(b),
    `:=`(
      x = .x[frame] + speeds[frame] * cos(b) * dts[frame],
      y = .y[frame] + speeds[frame] * sin(b) * dts[frame]
    ),
    env = list(.x = x, .y = y)
  ]

  shade[
    b == 11, # origin
    `:=`(
      x = .x[frame],
      y = .y[frame]
    ),
    env = list(.x = x, .y = y)
  ]

  shade[
    b == 12, # tip of arrow
    `:=`(
      x = .x[frame+dts[frame]],
      y = .y[frame+dts[frame]]
    ),
    env = list(.x = x, .y = y)
  ]

  shade[
    b == 13, # tip of reference
    `:=`(
      x = .x[frame] + speeds[frame]*dts[frame],
      y = .y[frame]
    ),
    env = list(.x = x, .y = y)
  ]

  shade <- shade[!is.na(x)]

  fudge = .1

  labels <- data.table(
    frame = rep(1:length(x), 2),
    target = rep(c('angle', 'speed'), each = length(x))
  )

  labels[
    target == 'angle',
    `:=`(
      x = .x + fudge,
      y = .y + fudge,
      content= paste('', round(heads/pi, 2), 'pi')
    ),
    env = list(.x = x, .y = y)
  ]
  labels[
    target == 'speed',
    `:=`(
      x = .x + speeds*dts/2,
      y = .y - fudge,
      content= as.character(round(speeds*dts, 2))
    ),
    env = list(.x = x, .y = y)
  ]

  path_full <- ggplot() + coord_fixed() +
    geom_polygon(data=shade, aes(x=x,y=y), fill = "lightgrey", color=FALSE) +
    geom_path(data = static, aes(x=x, y=y)) +
    geom_point(data = static[,.(x=x,y=y,frame=t)], aes(x=x, y=y), color = "darkblue") +
    geom_segment(
      data = arrows,
      aes(
        x = xstart,
        y = ystart,
        xend = xend,
        yend = yend
      ),
      arrow = arrow(),
      color = 'blue'
    ) +
    geom_segment(
      data = refs,
      aes(
        x = xstart,
        y = ystart,
        xend = r_xend,
        yend = r_yend
      ),
      color = 'darkgrey'
    ) + geom_label(data = labels, aes(x=x,y=y,label=content))

    if(animate) path_full <- path_full + transition_time(frame)

  return(path_full)

}

x <- c(  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  10,  10,  10,   9,   8,   7,   6,   5,   4,   4,   5,   6,   6,   6,   6,   6,   7,   8,   9,  10)
y <- c(  0,   1,   2,   1,   2,   3,   3,   3,   3,   2,   1,   2,   3,   4,   5,   6,   7,   8,   8,   8,   8,   7,   6,   7,   8,   9,  10,  10,  10,  10,  10)
ts <- 4


devtools::load_all()
temp_headings <- get_heading_and_speed_temporal(x, y, t_window = ts)
spatial_headings <- get_heading_and_speed_spatial(x, y, R = 1.5)

animate = FALSE 
if(animate){
  ex <- plot_headings(
    x,y,spatial_headings, animate
  )
  anim_save('example.gif', ex, nframes = 31*2, fps = 3*2)
} else {
  plot(plot_headings(
    x,y,temp_headings, animate
  ))
  dev.off()
}
