####
# Miscellaneous formatting functions ====
#  functions to format tables and graphs
# ===


#' @title Apply theme to a flextable
#' @description Apply theme to a flextable
#' @param x a flextable object
#' @param font_size font size to use, part = "all"
#' @param add_w extra width to add in inches
#' @param add_h extra height to add in inches
#' @param add_h_header extra header height to add in inches
#' @examples
#' ft <- flextable::flextable(iris)
#' ft <- ft_theme(ft)
#' @export
ft_theme <- function(x, font_size = 11, add_w = 0.1, add_h = 0.05, add_h_header = 0.05){

  num_header_rows <- length(x$header$rowheights)
  header_font_color <- "#0083A9"
  top_bottom_border_color <- "#0083A9"
  body_border_color <- "#d9d9d9"
  x <- x %>% flextable::border_remove()

  x <- x %>% flextable::hline_bottom(border = officer::fp_border(color = top_bottom_border_color, width = 1),
                                  part = "header") %>%
    flextable::color(color = header_font_color, part = "header") %>%
    flextable::hline(border = officer::fp_border(color = body_border_color), part = "body") %>%
    flextable::empty_blanks() %>%
    flextable::hline_bottom(border = officer::fp_border(color = top_bottom_border_color, width = 2), part = "body") %>%
    flextable::hline_top(border = officer::fp_border(color = top_bottom_border_color, width = 2), part = "header") %>%
    flextable::autofit(add_w = add_w, add_h = add_h) %>%
    flextable::padding(padding.right = 4, part = "all") %>%
    flextable::padding(padding.top = 2, padding.bottom = 4, part = "header") %>%
    flextable::vline_left(border = officer::fp_border(color = "white", style = "solid", width = 1)) %>%
    flextable::vline_right(border = officer::fp_border(color = "white", style = "solid", width = 1)) %>%
    flextable::fontsize(size = font_size, part = "all")

  x$header$rowheights <- x$header$rowheights + add_h_header
  if (num_header_rows > 1) {
    x <- x %>% flextable::align(1, align = "center", part = "header")
  }
  x
}


#' @export
#' @title Add a title in headers
#'
#' @description Add a title in the flextable's header part. It can
#' be inserted at the top or the bottom of header part.
#'
#' @details
#' An horizontal and a vertical merge are performed automatically.
#' Use \code{\link{merge_none}} to drop theses merging instructions.
#'
#' @param x a \code{flextable} object
#' @param title a string containing the title of the table
#' @param font_size integer to use for font size of the title
#' @examples
#' ft <- flextable::flextable( utils::head( iris ),
#'   col_keys = c("Species", "Sepal.Length", "Petal.Length", "Sepal.Width", "Petal.Width") )
#' ft_wtitle <- add_title_header(ft, "Iris data set")
add_title_header <- function(x, title, font_size = 12){

  x_ <- x
  first_row_height <- x$body$rowheights[1]
  save_spans_cols <- x$header$spans$columns
  non_blank_keys <- x$col_keys[!(x$col_keys %in% x$blanks)]

  border_color <- "#0083A9"

  call_list <- list(x=x, top=TRUE)
  call_list[[colnames(x$body$dataset)[1]]] <- title

  x <- do.call(flextable::add_header, call_list) %>% flextable::merge_h(part = "header")
  x <- x %>% flextable::border(i = 1, border.top = officer::fp_border(color = "transparent"),
                               border.bottom = officer::fp_border(color = border_color, width = 2),
                               part = 'header') %>%
    flextable::fontsize(i = 1, part = 'header', size = font_size) %>%
    flextable::height(i = 1, part = 'header', height = first_row_height) %>%
    flextable::bold(i = 1, bold = TRUE, part = "header") %>%
    flextable::align(i = 1, align = 'left', part = 'header')

  x$header$spans$rows[1,] <- c(ncol(x_$body$dataset), rep(0, ncol(x_$body$dataset)-1))
  x$header$spans$columns[2:(nrow(save_spans_cols) + 1),] <- save_spans_cols

  x
}


#' Default Color Scheme
#'
#' List of default pop colors
#'
#' Convenience list of pop colors in #RGB format.  Just type
#' names(pop_colors) to see the list of colors included.
#'
#' @examples
#'
#' \dontrun{ggplot() + geom_line(aes(x=-5:5, y=(-5:5)^2), colour=pop_colors$gold)}
#'
#' @export
pop_colors <- list(blue="#0083A9",
                   brown="#4F4525",
                   cream="#D3CD88",
                   darkblue="#0039A6",
                   darkcream="#7E7830",
                   darkgray="#808080",
                   darkgreen="#3E5B00",
                   gold="#F0AB00",
                   green="#7AB800",
                   lightblue="#5EB6E4",
                   lightergray="#F2F2F2",
                   lightgray="#BFBFBF",
                   lightpurple="#BC5FCD",
                   maroon="#822433",
                   midnightblue="#003F72",
                   purple="#6E267B",
                   red="#E11B22",
                   teal="#0083A9",
                   yellow="#FFE600")

#' Custom ggplot2 theme
#'
#' Use ggplot2's theming facility to set defaults
#'
#' This function sets the default look (colors, linetypes, etc.) of ggplot2 to
#' match color scheme.  Exhibits not made with ggplot2 will not
#' be affected.
#'
#' @param base_size default font size
#' @param base_family font family to use. Default is "sans".
#'
#' @seealso \code{\link{theme_bw}}, \code{\link{theme_set}}
#' @examples
#'
#' gg_pop_theme()
#'
#' @export
gg_pop_theme <- function(base_size=12, base_family="sans") {
  (ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
     ggplot2::theme(panel.border = ggplot2::element_blank(),
                    plot.background = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                    axis.line = ggplot2::element_line(colour = "black"),
                    panel.grid.major = ggplot2::element_line(color = "#F2F2F2",
                                                             linetype = "dashed"), panel.grid.major.x = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_line(), panel.grid.minor.x = ggplot2::element_blank(),
                    panel.grid.minor.y = ggplot2::element_blank(), strip.background = ggplot2::element_rect(colour = "white",
                                                                                                            size = 0.5), legend.key = ggplot2::element_blank(),
                    strip.text = ggplot2::element_text(color = "#0083A9"),
                    axis.line.x = ggplot2::element_line(color = "#F2F2F2",
                                                        size = 0.5), axis.line.y = ggplot2::element_line(color = "#F2F2F2",
                                                                                                         size = 0.5), axis.ticks = ggplot2::element_line(color = "#F2F2F2"),
                    plot.title = ggplot2::element_text(hjust = 0, size = 11,
                                                       color = "#0083A9")))
}

#' Set ggplot2 to use custom color scheme
#'
#' Use ggplot2's theming facility to set defaults
#'
#' This function sets the default look (colors, linetypes, etc.) of ggplot2 to
#' match color scheme.  Exhibits not made with ggplot2 will not
#' be affected.
#'
#' @seealso
#' \code{\link{theme_set}},
#' \code{\link{update_geom_defaults}},
#' \code{\link{gg_pop_theme}}
#' @examples
#'
#' gg_set_theme()
#'
#' @export
gg_set_theme <- function() {
  ggplot2::update_geom_defaults("point", list(colour = "#0083A9",
                                              size = 1.5))
  ggplot2::update_geom_defaults("line", list(colour = "#0083A9",
                                             size = 1))
  ggplot2::update_geom_defaults("abline", list(colour = "black",
                                               size = 1))
  ggplot2::update_geom_defaults("hline", list(colour = "black",
                                              size = 1))
  ggplot2::update_geom_defaults("smooth", list(colour = "#E11B22"))
  ggplot2::update_geom_defaults("bar", list(colour = "white",
                                            fill = "#0083A9"))
  ggplot2::update_geom_defaults("col", list(colour = "white",
                                            fill = "#0083A9"))
  ggplot2::update_geom_defaults("boxplot", list(colour = "white",
                                                fill = "#0083A9"))
  ggplot2::update_geom_defaults("hline", list(colour = "#808080",
                                              size = 1))
  ggplot2::update_geom_defaults("vline", list(colour = "#808080",
                                              size = 1))
  ggplot2::theme_set(gg_pop_theme())
}


#' Format number in comma style
#'
#' @param x numeric vector
#' @param digits number of digits to keep
#' @param ... additional parameters passed to [formatC]
#'
#' @return character vector of formatted numbers
#' @export
#'
#' @examples
#' number_format(10000.12, digits = 2)
#'
number_format <- function(x, digits = 0, ...){

  stopifnot(is.numeric(x))

  formatC(x, big.mark = ",", digits = digits, format = "f", ...)

}

#' Format number in percent style
#'
#' @param x numeric vector
#' @param digits number of digits to keep
#' @param ... additional parameters passed to [formatC]
#'
#' @return character vector of formatted percentages
#' @export
#'
#' @examples
#' pct_format(0.8546, digits = 2)
#'
pct_format <- function(x, digits = 2, ...){

  stopifnot(is.numeric(x))

  paste0(number_format(x * 100, digits = digits, ...), "%")

}
