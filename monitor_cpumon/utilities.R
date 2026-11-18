means <- function(x)
{
  freq <- sapply(x[grep("Freq", names(x))], mean)
  temp <- sapply(x[grep("Temp", names(x))], mean)
  fan <- sapply(x[grep("Fan", names(x))], mean)
  c(freq, temp, fan)
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
variable.order <- c("Freq", "Temp", "Fan")

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
        value <- machines[[machine]][[state]][[paste0(variable, "1")]]
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
                   Fan="Fan (rpm)"
                 )),
                 state=as_labeller(c(
                   idle="Idle",
                   single="Single",
                   main="Main",
                   full="Full"
                 ))
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

plot_cores <- function(cores)
{
  data <- list()
  for(machine in processor.order)
  {
    values <- as.matrix(cores[[machine]])
    for(state in state.order)
      data[[length(data)+1]] <- data.frame(
        core=seq_len(nrow(values)),
        value=values[, state] / 1000,
        machine=machine,
        state=state
      )
  }

  data <- do.call(rbind, data)
  data$machine <- factor(data$machine, levels=processor.order)
  data$state <- factor(data$state, levels=state.order)

  ggplot(data, aes(x=core, y=value, color=state, group=state)) +
    geom_line(linewidth=0.7) +
    geom_point(size=2, show.legend=FALSE) +
    facet_wrap(vars(machine), ncol=2, scales="free_x",
               labeller=as_labeller(processor.labels)) +
    scale_color_manual(values=state.colors, breaks=state.legend.order,
                       labels=state.labels) +
    scale_x_continuous(breaks=1:16, expand=expansion(mult=c(0.02, 0.02))) +
    scale_y_continuous(limits=c(0, NA), breaks=sensor_breaks,
                       expand=expansion(mult=c(0, 0.1))) +
    labs(x="Core", y="Frequency (GHz)", color=NULL) +
    themeCPUMon() +
    theme(legend.position="right",
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
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
                   Fan="Fan (rpm)"
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

remove_first <- function(x, burn=0, length=300)
{
  x <- x[(1:length)+burn,]
  x$Time <- x$Time - min(x$Time)
  row.names(x) <- NULL
  x
}

summary_machine <- function(x, machine=deparse(substitute(x)))
{
  peak <- function(y)
  {
    c(freq=mean(y$Freq1),
      temp=mean(y$Temp1),
      fan=mean(y$Fan1))
  }
  out <- sapply(x, peak)
  out <- xtab2taf(t(out), "state")
  out <- data.frame(machine=machine, out)
  out
}
