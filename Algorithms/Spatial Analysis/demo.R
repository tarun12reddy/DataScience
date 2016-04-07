library(sp)
library(maptools)
library(maps)
library(rgdal)

japan <- map("world", "usa", plot = FALSE)
p4s <- CRS("+proj=longlat +ellps=WGS84")
SLjapan <- map2SpatialLines(japan, proj4string = p4s)
slot_SLjapan <- slot(SLjapan, "lines")

sapply(slot_SLjapan, function(x) dim(slot(slot(x, "Lines")[[1]], "coords"))[1])

llCRS <- CRS("+proj=longlat +ellps=WGS84")
auck_shore <- MapGen2SL("auckland_mapgen.dat", llCRS)
islands_auck <- sapply(lns, function(x) {
  crds <- slot(slot(x, "Lines")[[1]], "coords")
  identical(crds[1, ], crds[nrow(crds), ])
})
table(islands_auck)

islands_sl <- auck_shore[islands_auck]
list_of_Lines <- slot(islands_sl, "lines")
list_of_Polygons <- lapply(list_of_Lines, function(x) {
  Polygons(list(Polygon(slot(slot(x, "Lines")[[1]], "coords"))), ID = slot(x, "ID"))
})
islands_sp <- SpatialPolygons(list_of_Polygons, proj4string = CRS("+proj=longlat +ellps=WGS84"))
summary(islands_sp)
slot(islands_sp, "plotOrder")
order(sapply(slot(islands_sp, "polygons"), function(x) slot(x, "area")), decreasing = TRUE)


library(maps)
library(maptools)
state.map <- map("state", plot = FALSE, fill = TRUE)
IDs <- sapply(strsplit(state.map$names, ":"), function(x) x[1])
state.sp <- map2SpatialPolygons(state.map, IDs = IDs, 
                                proj4string = CRS("+proj=longlat +ellps=WGS84"))


sat <- read.table("http://www.asdar-book.org/datasets/state.sat.data_mod.txt", row.names = 5, header = TRUE)
state_polygon_ids <- sapply(slot(state.sp, "polygons"), function(x) slot(x, "ID"))
id <- match(row.names(sat), state_polygon_ids)
row.names(sat)[is.na(id)]
state.spdf <- SpatialPolygonsDataFrame(state.sp, sat[!is.na(id), ])

DC <- "district of columbia"
not_dc <- !(row.names(slot(state.spdf, "data")) == DC)
state.spdf1 <- state.spdf[not_dc, ]
length(slot(state.spdf1, "polygons"))



DC <- "district of columbia"
not_dc <- !(row.names(slot(state.spdf, "data")) == DC)
state.spdf1 <- state.spdf[not_dc, ]
length(slot(state.spdf1, "polygons"))


length(slot(manitoulin_sp, "polygons"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"),
       function(x) slot(x, "hole"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"),
       function(x) slot(x, "ringDir"))

download.file("http://www.asdar-book.org/datasets/high.RData", "high.RData")
library(high.RData)
manitoulin_sp <- high$SP

bb <- bbox(manitoulin_sp)
cs <- c(0.01, 0.01)
cc <- bb[, 1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
manitoulin_grd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)

p4s <- CRS(proj4string(manitoulin_sp))
manitoulin_SG <- SpatialGrid(manitoulin_grd, proj4string = p4s)
summary(manitoulin_SG)

library(rgdal)
class(auck_el1)
auck_el1 <- readGDAL("70042108.tif")
slot(auck_el1, "grid")
slot(auck_el1, "grid.index")
slot(auck_el1, "coords")
slot(auck_el1, "bbox")
object.size(auck_el1)
object.size(slot(auck_el1, "data"))

auck_el2 <- as(auck_el1, "SpatialPixelsDataFrame")
object.size(auck_el2)
object.size(slot(auck_el2, "grid.index"))
object.size(slot(auck_el2, "coords"))
sum(is.na(auck_el1$band1)) + nrow(slot(auck_el2, "coords"))
prod(slot(slot(auck_el2, "grid"), "cells.dim"))

data(meuse.grid)
mg_SP <- SpatialPoints(cbind(meuse.grid$x, meuse.grid$y))
summary(mg_SP)

mg_SPix0 <- SpatialPixels(mg_SP)
summary(mg_SPix0)
prod(slot(slot(mg_SPix0, "grid"), "cells.dim"))


data(meuse)
coordinates(meuse) <- c("x", "y")
plot(meuse)
title("points")

cc <- coordinates(meuse)
m.sl <- SpatialLines(list(Lines(list(Line(cc)), "1")))
plot(m.sl)
title("lines")

data(meuse.riv)
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), "meuse.riv"))
meuse.sr <- SpatialPolygons(meuse.lst)
plot(meuse.sr, col = "grey")
title("polygons")


data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixels")
image(meuse.grid, col = "grey")
title("grid")

image(meuse.grid, col = "lightgrey")
plot(meuse.sr, col = "grey", add = TRUE)
plot(meuse, add = TRUE)

layout(matrix(c(1, 2), 1, 2))
plot(meuse.sr, axes = TRUE)
plot(meuse.sr, axes = FALSE)
axis(1, at = c(178000 + 0:2 * 2000), cex.axis = 0.7)
axis(2, at = c(326000 + 0:3 * 4000), cex.axis = 0.7)


plot(meuse.sr)
SpatialPolygonsRescale(meuse.sr, scale = 1000, fill = c("transparent", "black"), plot.grid = FALSE)
text(locator(1), "0")
text(locator(1), "1 km")
SpatialPolygonsRescale(layout.north.arrow(), offset = locator(1), 
                       scale = 400, plot.grid = FALSE)


wrld <- map("world", interior = FALSE, xlim = c(-179, 179), ylim = c(-89, 89), plot = FALSE)
wrld_p <- pruneMap(wrld, xlim = c(-179, 179))
llCRS <- CRS("+proj=longlat +ellps=WGS84")
wrld_sp <- map2SpatialLines(wrld_p, proj4string = llCRS)
prj_new <- CRS("+proj=moll")

wrld_proj <- spTransform(wrld_sp, prj_new)
wrld_grd <- gridlines(wrld_sp, easts = c(-179, seq(-150, 150, 50), 179.5), norths = seq(-75, 75, 15), ndiscr = 100)
wrld_grd_proj <- spTransform(wrld_grd, prj_new)
at_sp <- gridat(wrld_sp, easts = 0, norths = seq(-75, 75, 15), offset = 0.3)
at_proj <- spTransform(at_sp, prj_new)
plot(wrld_proj, col = "grey60")
plot(wrld_grd_proj, add = TRUE, lty = 3, col = "grey70")
text(coordinates(at_proj),pos = at_proj$pos, offset = at_proj$offset, labels = parse(text = as.character(at_proj$labels)), cex = 0.6)

###########################################################################
###########################################################################
###########################################################################
#1. Doing it in R
#2. Perf
#3. Explore how GeoServer can be involved
#4. 