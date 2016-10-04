#' My ggplot2 blog theme
#'
#' A ggplot2 theme for my blog plots.
#'
#'
#' @param tam.fonte Font size
#' @param fonte Font type
#'
#' @import ggplot2
#'
#' @export

theme_blog <- function (tam.fonte = 14, fonte = "sans") {

  (ggplot2::theme_minimal(base_size = tam.fonte, base_family = fonte) +
     ggplot2::theme(panel.grid.minor = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(colour = "grey80", size = 0.25),
                    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 20, 0, 0)),
                    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(20, 0, 0, 0)),
                    plot.caption = ggplot2::element_text(margin = ggplot2::margin(10, 0, 0, 0)),
                    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.5)),
                    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.1))))
}
