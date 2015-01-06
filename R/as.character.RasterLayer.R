##' @export
as.character.RasterLayer <- function(x, ...) {
  paste0("RasterLayer (nrow:", raster::nrow(x), "; ncol:",
         raster::ncol(x), "; res:",
         paste0(round(raster::res(x), 3), collapse=","),")")
}
