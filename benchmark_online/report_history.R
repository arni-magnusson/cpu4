# Plot long-term historical trends

# Before: history.csv (data)
#         history_analysis.csv, history_fit.csv, history_rate.csv (output)
# After:  history_companies_nm.png, history_companies_speed.png,
#         history_data.png, history_fit.png, history_rate.png,
#         history_time.png (report)

library(TAF)

mkdir("report")

# Read data
history <- read.taf("data/history.csv", colClasses=c(Date="Date"))
history.analysis <- read.taf("output/history_analysis.csv",
                             colClasses=c(Date="Date"))
history.fit <- read.taf("output/history_fit.csv", colClasses=c(Date="Date"))
history.rate <- read.taf("output/history_rate.csv")

# Prepare for plotting
score <- "Score (PassMark single thread)"
intel <- "History (Intel)"

# Intel and AMD process
history.nodes <- aggregate(Date ~ Company + nm, history, min)
taf.png("history_companies_nm", width=1200)
print(
  ggplot(history, aes(x=Date, y=nm, color=Company)) +
    geom_line(data=history.nodes, aes(group=Company), linewidth=0.8,
              show.legend=FALSE) +
    geom_point(size=2) +
    scale_color_manual(values=benchmark.colors[c("Intel", "AMD")],
                       breaks=c("Intel", "AMD")) +
    scale_y_log10(limits=c(2.6, 200),
                  breaks=c(3, 4, 5.8, 7.2, 10, 14, 22, 28, 35, 45, 65, 90,
                           130, 180),
                  labels=round(c(3, 4, 5.8, 7.2, 10, 14, 22, 28, 35, 45,
                                65, 90, 130, 180))) +
    labs(title="Semiconductor technology over time", x=NULL,
         y="Process node (nm)", color=NULL) +
    themeBenchmark() +
    theme(legend.position=c(0.98, 0.98),
          legend.justification=c(1, 1),
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
)
dev.off()

# Intel and AMD speed
taf.png("history_companies_speed", width=1200)
print(
  ggplot(history, aes(x=Date, y=Speed, color=Company)) +
    geom_point(size=2) +
    scale_color_manual(values=benchmark.colors[c("Intel", "AMD")],
                       breaks=c("Intel", "AMD")) +
    scale_y_log10() +
    labs(title="History (Intel and AMD)", x=NULL, y=score, color=NULL) +
    themeBenchmark() +
    theme(legend.position=c(0.02, 0.98),
          legend.justification=c(0, 1),
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
)
dev.off()

# Intel observations
taf.png("history_data", width=1200)
print(
  ggplot(history.analysis, aes(x=Date, y=Speed, shape=Period)) +
    geom_point(color=benchmark.colors[["Intel"]], size=2) +
    scale_shape_manual(values=c(Early=1, Late=16)) +
    scale_y_log10() +
    labs(title=intel, x=NULL, y=score, shape=NULL) +
    themeBenchmark() +
    theme(legend.position=c(0.02, 0.98),
          legend.justification=c(0, 1),
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
)
dev.off()

# Intel trends
taf.png("history_fit", width=1200)
print(
  ggplot(history.analysis, aes(x=Date, y=Speed, shape=Period)) +
    geom_line(data=history.fit, aes(group=Period),
              color=benchmark.colors[["Intel"]], linewidth=0.8) +
    geom_point(color=benchmark.colors[["Intel"]], size=2) +
    scale_shape_manual(values=c(Early=1, Late=16)) +
    scale_y_log10() +
    labs(title=intel, x=NULL, y=score, shape=NULL) +
    themeBenchmark() +
    theme(legend.position=c(0.02, 0.98),
          legend.justification=c(0, 1),
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
)
dev.off()

# Intel annual improvement
taf.png("history_rate", width=1200)
print(
  ggplot(history.rate, aes(x=Period, y=100*Annual, fill=Period)) +
    geom_col(width=0.72) +
    scale_fill_manual(values=c(Early="#8FBBD6", Late="#0072B2"),
                      guide="none") +
    scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
    labs(title=intel, x=NULL, y="Annual improvement (%)") +
    themeBenchmark() +
    theme(panel.grid.major.x=element_blank())
)
dev.off()

# Intel doubling time
taf.png("history_time", width=1200)
print(
  ggplot(history.rate, aes(x=Period, y=Doubling, fill=Period)) +
    geom_col(width=0.72) +
    scale_fill_manual(values=c(Early="#8FBBD6", Late="#0072B2"),
                      guide="none") +
    scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
    labs(title=intel, x=NULL, y="Doubling time (yr)") +
    themeBenchmark() +
    theme(panel.grid.major.x=element_blank())
)
dev.off()
