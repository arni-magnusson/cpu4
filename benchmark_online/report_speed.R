# Plot speed (single thread) over time

# Before: i7_high.csv, i7_low.csv, i7_medium.csv, r7_high.csv,
#         r7_low.csv (output)
# After:  speed_all.png, speed_high.png, speed_low.png,
#         speed_medium.png (report)

library(TAF)

mkdir("report")

# Read tables
i7.high <- read.taf("output/i7_high.csv", colClasses=c(Date="Date"))
i7.medium <- read.taf("output/i7_medium.csv", colClasses=c(Date="Date"))
i7.low <- read.taf("output/i7_low.csv", colClasses=c(Date="Date"))
r7.high <- read.taf("output/r7_high.csv", colClasses=c(Date="Date"))
r7.low <- read.taf("output/r7_low.csv", colClasses=c(Date="Date"))

# Prepare for plotting
score <- "Score (PassMark single thread)"

intel.high <- transform(i7.high[-1,], Company="Intel", Series="Intel high")
amd.high <- transform(r7.high, Company="AMD", Series="AMD high")
intel.medium <- transform(i7.medium[-1,], Company="Intel",
                          Series="Intel medium")
intel.low <- transform(i7.low[-1,], Company="Intel", Series="Intel low")
amd.low <- transform(r7.low, Company="AMD", Series="AMD low")

plot.speed <- function(data, title)
{
  ggplot(data, aes(x=Date, y=Speed, color=Company, group=Series)) +
    geom_line(linewidth=0.8) +
    geom_point(shape=1, size=2) +
    scale_color_manual(values=benchmark.colors[c("Intel", "AMD")],
                       breaks=intersect(c("Intel", "AMD"), unique(data$Company))) +
    scale_x_date(limits=as.Date(c("2010-01-01", "2027-01-01")),
                 date_breaks="5 years", date_labels="%Y",
                 expand=expansion(mult=0)) +
    scale_y_log10(limits=c(700, 4400),
                  breaks=c(1000, 2000, 3000, 4000),
                  expand=expansion(mult=0)) +
    labs(title=title, x="Date", y=score, color=NULL) +
    themeBenchmark() +
    theme(legend.position=c(0.02, 0.98),
          legend.justification=c(0, 1),
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
}

taf.png("speed_all", width=1200)
speed.all <- rbind(intel.high, amd.high, intel.medium, intel.low, amd.low)
print(plot.speed(speed.all, "Intel and AMD (high/medium/low power)"))
dev.off()

taf.png("speed_high", width=1200)
print(plot.speed(rbind(intel.high, amd.high), "Intel and AMD (high power)"))
dev.off()

taf.png("speed_low", width=1200)
print(plot.speed(rbind(intel.low, amd.low), "Intel and AMD (low power)"))
dev.off()

taf.png("speed_medium", width=1200)
print(plot.speed(intel.medium, "Intel (medium power)"))
dev.off()
