##' @export
as.character.RasterLayer <- function(x, ...) {
  paste0("RasterLayer (nrow:", nrow(x), "; ncol:", ncol(x), "; res:", paste0(round(res(x), 3), collapse=","),")")
}
