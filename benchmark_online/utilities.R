themeBenchmark <- function()
{
  ggplot2::theme_minimal(base_size=12) +
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold", hjust=0.5,
                                                   margin=ggplot2::margin(b=12)),
                   axis.title.x=ggplot2::element_text(
                     margin=ggplot2::margin(t=12)),
                   axis.title.y=ggplot2::element_text(
                     margin=ggplot2::margin(r=12)),
                   axis.text.y=ggplot2::element_text(angle=90, hjust=0.5,
                                                     vjust=0.5),
                   panel.grid.major.x=ggplot2::element_line(
                     color="grey85", linewidth=0.3),
                   panel.grid.major.y=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
                   plot.margin=ggplot2::margin(24, 24, 24, 24))
}

implot <- function(x, title, color)
{
  x <- x[-1,]
  labels <- paste0(x$Model, " (", format(x$Date, "%Y"), ")")
  x$Model <- factor(x$Model, levels=x$Model)
  ggplot2::ggplot(x, ggplot2::aes(x=100*Improve, y=Model)) +
    ggplot2::geom_col(width=0.72, fill=color) +
    ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(0, 0.1))) +
    ggplot2::scale_y_discrete(labels=setNames(labels, levels(x$Model))) +
    ggplot2::labs(title=title, x="Speed improvement (%)", y=NULL) +
    themeBenchmark() +
    ggplot2::theme(axis.text.y=ggplot2::element_text(angle=0))
}
