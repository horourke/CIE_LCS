# O'Rourke & Hilley (2024)
# Code for creating CIE plots in example 2

install.packages("ggtext")
library(ggplot2)
library(ggtext)

# for bb, list the values of b you want on the Y axis
# in example 2, there are 4 estimates of b
bb <- c(-0.231, -0.175, -0.127, -0.073)
# repeats the above values in a vector for both values of Z
b <- rep(bb, times=2)

# This allows Z to be a factor in the plot
# Edit to have as many 'MOD0's as z = 0 CIEs
# Edit to have as many 'MOD1's as z = 1 CIEs
Z <- c('MOD0', 'MOD0', 'MOD0', 'MOD0', 'MOD1', 'MOD1', 'MOD1', 'MOD1')

# for Z, list as many values of 0 and 1 as you have CIEs
# in example 2, there are 4 CIEs each for 0 and 1
Z <- c(0, 0, 0, 0, 1, 1, 1, 1)

# For ab, list your CIEs with Z = 0 first and Z = 1 second
ab <- c(0.091, 0.069, 0.050, 0.029, -0.093, -0.070, -0.051, -0.030)

# create data frame for plot
dat <- as.data.frame(cbind(ab,b,Z))

# labels for x and y
ylab <- parse(text = "italic(~a * ~b[CIE])")
xlab <- parse(text = "italic(~b[t])")

# title
titlecie <-expression(italic('Comparison of CIEs Across Groups for LCSMM Model with Freely Estimated Coupling for b'))

# plot
ggplot(dat, aes(x = b, y = ab, group = Z)) +
  #geom_line(aes(linetype=Z, color=Z), size=1.5) +
  geom_line(aes(linetype = as.factor(Z)), size = 2) +
  geom_point(size = 4) +
  theme(text=element_text(size=16, family="serif"),
        axis.line = element_line(color='black'),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)
        ) +
  scale_x_continuous(name = xlab, breaks = seq(-.28,0,.02)) +
  scale_y_continuous(name = ylab, breaks = seq(-.1,.1,0.025)) +
  labs(x = xlab, 
       y = ylab, 
       title = titlecie,
       linetype = "Z"
       )


# Set up file directory path to save plot as PNG file
filepath <- "C:/"

# save plot as PNG file
ggsave(
  "ex2.png",
  plot = last_plot(),
  path = filepath
)
