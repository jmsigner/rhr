##' @export
as.character.RasterLayer <- function(x) {
  paste0("RasterLayer (nrow:", nrow(x), "; ncol:", ncol(x), "; res:", paste0(res(x), collapse=","),")")
}
