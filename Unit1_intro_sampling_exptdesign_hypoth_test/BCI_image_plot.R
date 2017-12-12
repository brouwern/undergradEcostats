?BCI
library(vegan)
data(BCI)
data(BCI.env)


image(x = 1:50, y=1:50, z= BCI.env$EnvHet)
      
      xlim, ylim, col = heat.colors(12),
      add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,
      breaks, oldstyle = FALSE, useRaster, ...)