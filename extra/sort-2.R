# Sys.setenv('R_KEEP_PKG_SOURCE'='yes')

set.seed(1220)
x <- runif(10)
library(watcher)
raw <- watch(insert_sort, c('i', 'j', 'x'))(x)
dat <- simplify_data(attr(raw, 'watch.data'))

# Extract x data and augment with the corresponding scalar `j` loop index

xs <- dat$x
xs <- transform(
  xs, j=dat[['.scalar']][['j']][.id],
  ix=rep_len(seq_along(x), length(val))
)
xs[['j']][is.na(xs[['j']])] <- 0
labs <- as.data.frame(dat$.scalar)
y.max <- max(xs[['val']])
code.txt <- attr(raw, 'watch.code')
dat.code <- expand_text(code.txt, dat)

library(gganimate)
library(ggplot2)
dpi <- 72
width <- 600
height <- width / 3 * 2
x.size <- max(xs$ix) + 1

p <- ggplot(xs, aes(x=ix, y=val)) +
  geom_col(aes(fill=I(ifelse(ix==j, 'red', 'grey35')))) +
  geom_label(
    data=labs, aes(x=i, label='i', y=-.08*y.max),
    size=8, label.padding=unit(0.5, "lines"), family='mono'
  ) +
  geom_label(
    data=labs, aes(x=j, label='j', y=-.2*y.max),
    size=8, label.padding=unit(0.5, "lines"), family='mono'
  ) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  coord_cartesian(clip = 'off', xlim=c(0.5,max(xs$ix)+.5)) +
  geom_text(
    data=dat.code,
    aes(
      label=code, x=-x.size - 4,
      y=y.raw/(length(code.txt)*2.1),
      color=I(ifelse(highlight, 'red', 'grey35')),
      fontface=I(ifelse(highlight, 'bold', 'plain'))
    ), hjust=0, vjust=0.5, size=5, family='mono'
  ) +
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
    plot.margin=unit(c(.1, .1, .1, (width/1.75)/dpi), "inches"),
    panel.grid=element_blank()
  ) +
  NULL

p.anim <- p + transition_manual(.id)

anim_save(
  '~/Downloads/sort-2.gif', nframes=length(dat$.scalar$.id), p.anim,
  width=width, height=height, fps=3
)

# res <- animate(
#   p.anim,
#   nframes = nrow(dat[['.scalar']]), device = "png",
#   renderer = file_renderer(
#     "~/Downloads/sort-anim-2/", prefix = "gganim-img", overwrite = TRUE
#   ),
#   width=width, height=height
# )

# stop()
# ffmpeg -framerate 5 -pattern_type glob -i '*.png' -pix_fmt yuv420p out.mp4 &&
#   open out.mp4
