#' Color schemes for the \code{dockless} package
#'
#' Creates either sequential or categorical color schemes in the dockless style.
#'
#' @param n the number of colors to be in the palette. Ignored if 
#' \code{categorical} is set to \code{TRUE}.
#' @param categorical logical. If \code{TRUE}, a categorical color scheme will be
#' produced. If \code{FALSE}, a sequential color scheme will be produced.
#' @return Returns an vector of colors.
#' @export
dockless_colors = function(n = 10, categorical = FALSE) {
  
  if (categorical) {
    c('orange', 'deepskyblue', 'magenta', 'lawngreen' )
  } else {
    color_function = grDevices::colorRampPalette(
      c('#fbd38c', '#fac56a', '#f9b847', '#f9ab24', '#f39c06', '#d08605',
        '#ad6f04', '#8a5903', '#684302', '#452c01')
    )
    
    color_function(n)
  }
  
}