rm(list=ls())

library(tidyverse)


t <- seq(-pi,pi,0.01)
x <- 16*0.25*(3*sin(t) - sin(3*t))
y <- 13*cos(t) - 5*cos(2*t) - cos(4*t)


# Letters -----------------------------------------------------------------

# Plot I----

point1 <- 0
point2 <- 0
I <- tibble(point1, point2)

IPlot <- I %>% 
  ggplot(aes(point1, point2)) +
  theme_classic() +
  geom_errorbar(ymin=-1, ymax=1, width=0.2, size=4, color="pink") +
  expand_limits(x=c(-.5,.5), y=c(-1.5,1.5)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
IPlot


# Plot L ------------------------------------------------------------------

x2 <- seq(1,100,.1)
y2 <- 1/x2
L <- tibble(x2,y2)

LPlot <- L %>% 
  ggplot(aes(x2,y2)) +
  theme_classic() +
  geom_line(size = 4, color = "pink", lineend = "round") +
  expand_limits(x=c(-90,200), y=c(0,1.1)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
LPlot


# Plot O ------------------------------------------------------------------

# # Troubleshoot this later
# x3 <- seq(-3, 3, 0.1)
# y3.1 <- sqrt(9-(x3^2))
# y3.2 <- -sqrt(9-x3^2)
# O <- tibble(x3, y3.1, y3.2)
# 
# 
# OPlot <- O %>% 
#   ggplot(aes(x3, y3.1)) +
#   geom_line() +
#   ggplot(aes(x3, y3.2)) +
#   geom_line()
# OPlot

CirclePlot <- I %>% 
  ggplot(aes(point1, point2)) +
  theme_classic() +
  geom_point(size = 150, color = "pink") +
  geom_point(size = 142, color = "white") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
CirclePlot

# Plot V ------------------------------------------------------------------

x4 <- seq(-3, 3, 0.1)
y4 <- abs(x4)
V <- tibble(x4,y4)

VPlot <- V %>% 
  ggplot(aes(x4, y4)) + 
  theme_classic() +
  geom_line(size = 4, color = "pink", lineend = "round") +
  expand_limits(x=c(-4.5, 4.5), y=c(0, 3)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
VPlot


# Plot E ------------------------------------------------------------------

y6 <- seq(-pi, pi, 0.001)
x6 <- -3*abs(sin(y6))
E <- tibble(x6, y6)

plot(x6, y6)

EPlot <- E %>% 
  ggplot(aes(x6, y6)) + 
  theme_classic() +
  expand_limits(x=c(-5, 3), y=c(-pi, pi)) +
  geom_point(color = "pink", size = 4) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
EPlot


# Plot U ------------------------------------------------------------------

x5 <- seq(-3.1, 3, 0.1)
y5 <- (4*x5^4) + x5^3 + x5^2
U <- tibble(x5, y5)

UPlot <- U %>% 
  ggplot(aes(x5, y5)) +
  theme_classic() +
  geom_line(size = 4, color = "pink") +
  expand_limits(x=c(-5, 5), y=c(0, 400)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
UPlot

# Plot full message on one chart (iteration 2)


# Hearts ------------------------------------------------------------------

Heart1 <- tibble(x,y)

Heart1Plot <- Heart1 %>% 
  ggplot(aes(x,y)) +
  theme_classic() +
  geom_polygon(fill = "pink") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  
  coord_polar(start = pi)
Heart1Plot


r <- 2 - 2*sin(t) + sin(t)*(sqrt(abs(cos(t))))/(sin(t)+1.4)

Heart2 <- tibble(t,r)

Heart2Plot <- Heart2 %>% 
  ggplot(aes(t,r)) +
  theme_classic() +
  geom_polygon(fill = "pink") +
  coord_polar(start = pi/2) +
  scale_fill_manual(values = "pink") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
Heart2Plot



# References --------------------------------------------------------------


# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/curve.html
# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/splinefun.html
# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/par.html
# https://ggplot2.tidyverse.org/reference/coord_polar.html
# https://stackoverflow.com/questions/39773933/how-to-get-a-really-periodic-polar-surface-plot-with-ggplot
# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_errorbar
# https://www.lenfisherscience.com/36-all-you-need-is-love-mathematical-style/
# http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations
# http://jwilson.coe.uga.edu/emt668/EMAT6680.Folders/Maddox/Maddox.2/Maddox.2.html#:~:text=As%20a%20increases%2C%20the%20sides,a%20is%20equal%20to%20zero.
# https://www.rdocumentation.org/packages/ggforce/versions/0.3.2/topics/geom_circle
# https://community.rstudio.com/t/circle-in-ggplot2/8543/3
# https://community.rstudio.com/t/how-to-stack-two-images-horizontally-in-r-markdown/18941/3
# https://community.rstudio.com/t/how-to-stack-two-images-horizontally-in-r-markdown/18941/11
# https://stackoverflow.com/questions/28748301/how-can-i-left-align-latex-equations-in-r-markdown
# https://www.overleaf.com/learn/latex/Line_breaks_and_blank_spaces
# https://texblog.org/2012/08/29/changing-the-font-size-in-latex/
