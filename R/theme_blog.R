#' My ggplot2 blog theme
#'
#' A ggplot2 theme for my blog plots.
#'
#'
#' @param tamanho Font size
#' @param fonte Font type
#' @param title.fonte Title's font type
#'
#' @import ggplot2
#'
#' @export

theme_blog <- function(tamanho = 14, fonte = "sans", title.fonte = NULL){

  if(is.null(title.fonte)) title.fonte <- fonte

  (ggplot2::theme_minimal(base_size = tamanho, base_family = fonte) +
     ggplot2::theme(panel.grid.minor = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(colour = "grey80", size = 0.25),
                    axis.text = element_text(size = ggplot2::rel(0.75)),
                    axis.title.y = ggplot2::element_text(size = ggplot2::rel(0.9), margin = ggplot2::margin(0, 20, 0, 0)),
                    axis.title.x = ggplot2::element_text(size = ggplot2::rel(0.9), margin = ggplot2::margin(20, 0, 0, 0)),
                    plot.caption = ggplot2::element_text(size = ggplot2::rel(0.6), margin = ggplot2::margin(10, 0, 0, 0)),
                    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.4), margin = ggplot2::margin(0, 0, 13, 0), family = title.fonte),
                    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.85), margin = ggplot2::margin(0, 0, 15, 0)),
                    legend.position = "bottom",
                    legend.title = element_blank(),
                    legend.margin = ggplot2::margin(12, 0, 0, 0)))
}



#' My ggplot2 blog scale colour
#'
#' A scale_colour function
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @rdname scale_blog
#' @export

scale_color_blog <- function(...){

  ggplot2::discrete_scale("colour", "meublog", blog_pal(), ...)
}



#' My ggplot2 blog fill colour
#'
#' A scale_fill function
#'
#' @rdname scale_blog
#' @export

scale_fill_blog <- function(...){

  ggplot2::discrete_scale("fill", "meublog", blog_pal(), ...)
}



#' My ggplot2 blog palette
#'
#' A palette of colours
#'
#' @rdname scale_blog
#' @export
blog_pal <- function() {

  function(n) {
    colors <- c("orange", "black", "orange4", "orangered3", "seagreen4", "green", "blue4")
    unname(colors[seq_len(n)])
  }
}


