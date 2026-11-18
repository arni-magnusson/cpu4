getCache <- function(x)
{
  L3 <- as.numeric(x[["L3"]])
  L2 <- as.numeric(x[["L2"]])
  size <- if(L3 > 0) L3 else L2
  size
}

themeLaptops <- function()
{
  ggplot2::theme_minimal(base_size=12) +
    ggplot2::theme(plot.title=ggplot2::element_text(
                     face="bold", hjust=0.5,
                     margin=ggplot2::margin(b=12)),
                   axis.title.x=ggplot2::element_text(
                     margin=ggplot2::margin(t=12)),
                   axis.title.y=ggplot2::element_text(
                     margin=ggplot2::margin(r=12)),
                   axis.text.y=ggplot2::element_text(angle=90,
                                                     vjust=0.5,
                                                     hjust=0.5),
                   panel.grid.major.x=ggplot2::element_blank(),
                   panel.grid.major.y=ggplot2::element_line(
                     color="grey85", linewidth=0.3),
                   panel.grid.minor=ggplot2::element_blank(),
                   plot.margin=ggplot2::margin(24, 24, 24, 24))
}

geomLaptopPoints <- function()
{
  ggplot2::geom_point(ggplot2::aes(fill=Form), shape=21,
                      color="#333333", size=2.5, stroke=0.5,
                      show.legend=FALSE)
}

scaleLaptopFill <- function()
{
  ggplot2::scale_fill_manual(values=c(Workstation="#0072B2",
                                      Intermediate="#D55E00",
                                      Ultraportable="#009E73"),
                             guide="none")
}
