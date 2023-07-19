
library(magick)
library(grid)

r     <- 100
theta <- seq(30, 360, 60) * pi/180 
x     <- .mid + .mm(r * cos(theta))
y     <- .mid + .mm(r * sin(theta))


jpg <- jpeg::readJPEG("working/blondie.jpg")
image <- ig_raster(
  image  = jpg, 
  x      = .mid,
  y      = .mid,
  width  = .mm(2 * 1.0 * r), 
  height = .mm(2 * 1.0 * r)
)



hex      <- polygonGrob(x, y, gp = gpar(fill = 'black'))
hex_mask <- grid::as.mask(hex, type = 'alpha')
image$vp$mask <- hex_mask


pdf("working/logo.pdf", width = 10, height = 10)
grid.newpage()
grid.draw(image)
grid.polygon(x, y, gp = gpar(lwd = 15, col = '#483058'))
dev.off()


system("convert -density 300 working/logo.pdf -resize 25% working/logo.png")
