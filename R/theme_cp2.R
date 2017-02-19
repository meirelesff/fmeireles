#' My ggplot2 theme, 2
#'
#' A ggplot2 theme for everyday use, without axes.
#'
#'
#' @param tam.fonte Font size
#' @param fonte Font type
#'
#' @import ggplot2
#'
#' @export

theme_cp2 <- function (tam.fonte = 12, fonte = "sans") {

  (ggplot2::theme_minimal(base_size = tam.fonte, base_family = fonte) +
     ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0, margin = ggplot2::margin(0, 0, 20, 0)),
                    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(0, 15)),
                    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(15)),
                    axis.title = ggplot2::element_text(size = tam.fonte),
                    title = ggplot2::element_text(size = tam.fonte),
                    legend.position = "bottom",
                    legend.title = ggplot2::element_blank(),
                    legend.key = ggplot2::element_blank(),
                    axis.text = ggplot2::element_text(color = "gray10")))
}
