outliers <- function(x, span=0.75, dist=0.5, plot=FALSE, xlab="", ylab="", ...)
{
  # Fit loess model
  iter <- 1:length(x)
  trend <- loess(x~iter, span=span)$fitted

  # Calculate absolute residual
  absresid <- abs(x - trend)

  # Identify outliers
  index <- which(absresid > dist)
  values <- x[index]
  expected <- trend[index]

  # Repair outliers
  repaired <- replace(x, index, expected)

  # Plot
  if(plot)
  {
    plot(x, xlab=xlab, ylab=ylab, ...)
    lines(trend, lwd=2, ...)
    points(index, values, pch=4, cex=1.5, lwd=3, col=2)
    points(index, expected, pch=16, cex=1.5, col=3)
  }

  out <- list(trend=trend, repaired=repaired, index=index, values=values,
              expected=expected)
  out
}

processor.order <- c("p1gen8", "p3560", "p15gen1", "m4700")
processor.labels <- c(
  "p1gen8"="Lenovo P1 Gen 8\nIntel u9-285H",
  "p3560"="Dell Precision 3560\nIntel i7-1165G7",
  "p15gen1"="Lenovo P15 Gen 1\nIntel i7-10875H",
  "m4700"="Dell Precision M4700\nIntel i7-3740QM"
)
processor.colors <- c(
  "p1gen8"="#009E73",
  "p3560"="#0072B2",
  "p15gen1"="#D55E00",
  "m4700"="#CC79A7"
)
state.order <- c("idle", "single", "main", "full")
state.legend.order <- rev(state.order)
state.labels <- c(idle="Idle", single="Single", main="Main", full="Full")
state.colors <- c(
  idle="#999999",
  single="#D55E00",
  main="#0072B2",
  full="#009E73"
)
variable.order <- c("Freq", "Temp", "Watt")

themeCPUMon <- function()
{
  theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold", hjust=0.5,
                                  margin=margin(b=12)),
          axis.title.x=element_text(margin=margin(t=12)),
          axis.title.y=element_text(margin=margin(r=12)),
          axis.text.y=element_text(angle=90, hjust=0.5, vjust=0.5),
          panel.grid.major=element_line(color="grey85", linewidth=0.3),
          panel.grid.minor=element_blank(),
          panel.spacing.x=grid::unit(0.6, "cm"),
          panel.spacing.y=grid::unit(0.6, "cm"),
          plot.margin=margin(24, 24, 24, 24),
          strip.text=element_text(face="bold"))
}

sensor_data <- function(machines)
{
  data <- list()
  for(machine in processor.order)
    for(state in state.order)
      for(variable in variable.order)
      {
        value <- machines[[machine]][[state]][[variable]]
        if(variable == "Freq")
          value <- value / 1000
        data[[length(data)+1]] <- data.frame(
          iteration=seq_along(value),
          value=value,
          machine=machine,
          state=state,
          variable=variable
        )
      }

  data <- do.call(rbind, data)
  data$machine <- factor(data$machine, levels=processor.order)
  data$state <- factor(data$state, levels=state.order)
  data$variable <- factor(data$variable, levels=variable.order)
  data
}

sensor_breaks <- function(limits)
{
  maximum <- max(limits, na.rm=TRUE)
  if(maximum > 10 && maximum <= 150)
    c(0, 25, 50, 75, 100)
  else
    pretty(limits, n=5)
}

plot_comparison <- function(machines)
{
  data <- sensor_data(machines)

  ggplot(data, aes(x=iteration, y=value, color=machine)) +
    geom_point(size=0.8, alpha=0.65) +
    facet_grid(rows=vars(variable), cols=vars(state), scales="free_y",
               switch="y",
               labeller=labeller(
                 variable=as_labeller(c(
                   Freq="Frequency (GHz)",
                   Temp="Temperature (\u02DAC)",
                   Watt="Power (W)"
                 )),
                 state=as_labeller(state.labels)
               )) +
    scale_color_manual(values=processor.colors,
                       breaks=processor.order,
                       labels=processor.labels) +
    scale_x_continuous(limits=c(0, 300), breaks=seq(0, 300, 50),
                       expand=expansion(mult=c(0, 0))) +
    scale_y_continuous(limits=c(0, NA), breaks=sensor_breaks,
                       expand=expansion(mult=c(0, 0.1))) +
    labs(x="Iteration", y=NULL, color=NULL) +
    themeCPUMon() +
    theme(legend.position="right",
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.key.spacing.y=grid::unit(0.3, "cm"),
          legend.text=element_text(size=8),
          strip.placement="outside",
          strip.background=element_blank(),
          strip.text.x=element_text(face="bold"),
          strip.text.y.left=element_text(angle=90, face="plain", size=10)) +
    guides(color=guide_legend(override.aes=list(size=1.6, alpha=1)))
}

plot_each <- function(machines)
{
  data <- sensor_data(machines)
  data$machine <- factor(data$machine, levels=rev(processor.order))

  ggplot(data, aes(x=iteration, y=value, color=state)) +
    geom_point(size=0.8, alpha=0.65) +
    facet_grid(rows=vars(variable), cols=vars(machine), scales="free_y",
               switch="y",
               labeller=labeller(
                 variable=as_labeller(c(
                   Freq="Frequency (GHz)",
                   Temp="Temperature (\u02DAC)",
                   Watt="Power (W)"
                 )),
                 machine=as_labeller(processor.labels)
               )) +
    scale_color_manual(values=state.colors, breaks=state.legend.order,
                       labels=state.labels) +
    scale_x_continuous(limits=c(0, 300), breaks=seq(0, 300, 50),
                       expand=expansion(mult=c(0, 0))) +
    scale_y_continuous(limits=c(0, NA), breaks=sensor_breaks,
                       expand=expansion(mult=c(0, 0.1))) +
    labs(x="Iteration", y=NULL, color=NULL) +
    themeCPUMon() +
    theme(legend.position="right",
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=9),
          strip.placement="outside",
          strip.background=element_blank(),
          strip.text.x=element_text(face="bold", size=9),
          strip.text.y.left=element_text(angle=90, face="plain", size=10)) +
    guides(color=guide_legend(override.aes=list(size=1.6, alpha=1)))
}

read_turbostat <- function(file, n=300, burn=0)
{
  # Parse args
  if(length(burn) == 1)
    burn <- c(burn, 0)  # first and last rows to remove

  # Read file
  txt <- readLines(file)
  beg <- grep("^Avg_MHz", txt)
  out <- read.table(text=txt, header=TRUE, skip=beg-1)

  # Subset cols
  out <- out[c("Bzy_MHz", "PkgTmp", "PkgWatt")]
  names(out) <- c("Freq", "Temp", "Watt")

  # Subset rows
  if(burn[1] > 0)
    out <- tail(out, -burn[1])  # remove first rows
  if(burn[2] > 0)
    out <- head(out, -burn[2])  # remove last rows
  if(is.numeric(n) && n > 0 && is.finite(n))  # return n rows
    out <- out[1:n,]

  out
}
