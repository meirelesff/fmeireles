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
     ggplot2::theme(legend.position = "bottom",
                    legend.title = element_blank(),
                    legend.key = ggplot2::element_blank(),
                    axis.text = element_text(color = "gray10")))
}
