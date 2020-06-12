
theme_ngcustom <- function(base_size = 11,
                           strip_text_size = 12,
                           strip_text_margin = 5,
                           subtitle_size = 10,
                           subtitle_margin = 10,
                           plot_title_size = 14,
                           plot_title_margin = 10,
                       ...) {
    modtheme <- ggplot2::theme_minimal(base_family = "IBMPlexSans",
                                       base_size = base_size, ...)
    modtheme$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                                 margin=margin(b=strip_text_margin),
                                                 family="IBMPlexSans-Medium")
    modtheme$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                                    margin=margin(b=subtitle_margin),
                                                    family="IBMPlexSans")
    modtheme$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                                 margin=margin(b=plot_title_margin),
                                                 family="IBMPlexSans-Bold")
    modtheme
}