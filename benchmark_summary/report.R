# Produce plots and tables for report
#
# Before: catage.csv, passmark.csv, regression.csv (output)
# After:  benchmark.png, catage.csv, passmark.csv, regression.csv,
#         regression.png, speed.png (report)

library(TAF)
library(ggplot2)

mkdir("report")

# Read data
catage <- read.taf("output/catage.csv")
passmark <- read.taf("output/passmark.csv")
regression <- read.taf("output/regression.csv")

# Establish processor order
ord <- catage$processor[order(catage$speed)]
catage <- catage[match(ord, catage$processor),]
passmark <- passmark[match(ord, passmark$processor),]
regression <- regression[match(ord, regression$processor),]
catage$processor <- ordered(catage$processor, catage$processor)
passmark$processor <- ordered(passmark$processor, passmark$processor)
regression$processor <- ordered(regression$processor, regression$processor)

processor.levels <- c("i7-3740QM", "i7-10875H", "i7-1165G7", "u9-285H")
processor.labels <- c(
  "i7-3740QM"="Dell Precision M4700\nIntel i7-3740QM",
  "i7-10875H"="Lenovo P15 Gen 1\nIntel i7-10875H",
  "i7-1165G7"="Dell Precision 3560\nIntel i7-1165G7",
  "u9-285H"="Lenovo P1 Gen 8\nIntel u9-285H"
)
catage$processor <- factor(catage$processor, levels=processor.levels)
passmark$processor <- factor(passmark$processor, levels=processor.levels)
regression$processor <- factor(regression$processor, levels=processor.levels)

themeBenchmark <- function()
{
  theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold", hjust=0.5,
                                  margin=margin(b=12)),
          axis.title.x=element_text(margin=margin(t=12)),
          axis.title.y=element_text(margin=margin(r=12)),
          panel.grid.major=element_line(color="grey85", linewidth=0.3),
          panel.grid.minor=element_blank(),
          plot.margin=margin(24, 24, 24, 24))
}

# Plot benchmark results
taf.png("benchmark")
passmark.long <- rbind(
  data.frame(processor=passmark$processor, source="Local",
             score=passmark$local),
  data.frame(processor=passmark$processor, source="Online",
             score=passmark$online)
)
passmark.long$source <- factor(passmark.long$source,
                               levels=c("Local", "Online"))
print(
  ggplot(passmark.long, aes(x=processor, y=score, fill=source)) +
    geom_col(position=position_dodge(width=0.8, reverse=TRUE), width=0.72) +
    scale_fill_manual(values=c(Local="#0072B2", Online="#D55E00"),
                      breaks=c("Local", "Online"),
                      labels=c("Local laptop", "Online average")) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
    scale_x_discrete(labels=processor.labels) +
    coord_flip() +
    labs(title="Synthetic benchmark results", x=NULL,
         y="Score (PassMark single thread)", fill=NULL) +
    themeBenchmark() +
    theme(axis.text.y=element_text(angle=0),
          panel.grid.major.x=element_blank(),
          legend.position=c(0.98, 0.07),
          legend.justification=c(1, 0),
          legend.direction="vertical",
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
)
dev.off()

# Plot regression results
taf.png("regression")
regression.labels <- as.character(regression$processor)
print(
  ggplot(regression, aes(x=passmark, y=actual)) +
    geom_abline(slope=regression$slope[1], intercept=0, linetype=2,
                color="#D55E00") +
    geom_point(color="#0072B2", size=2.5) +
    geom_label(aes(label=regression.labels), vjust=-0.5, size=3.0,
               fill="white", linewidth=0,
               label.padding=unit(0.1, "lines")) +
    scale_x_continuous(limits=c(0.9, 1.1) * range(regression$passmark),
                       expand=expansion(mult=c(0.02, 0.02))) +
    scale_y_continuous(limits=c(0.95, 1.05) * range(regression$actual),
                       expand=expansion(mult=c(0.02, 0.02))) +
    labs(title="Actual speed vs. expectation based on synthetic benchmark",
         x="Synthetic benchmark score",
         y="Actual speed (model iterations / sec)") +
    themeBenchmark() +
    theme(axis.text.y=element_text(angle=0, hjust=0.5, vjust=0.5))
)
dev.off()

# Plot speed results
taf.png("speed")
speed.long <- rbind(
  data.frame(processor=catage$processor, source="Actual", speed=catage$speed),
  data.frame(processor=catage$processor,
             source="Expectation, based on local synthetic benchmark",
             speed=catage$expected)
)
speed.long$source <- factor(
  speed.long$source,
  levels=c("Actual", "Expectation, based on local synthetic benchmark")
)
print(
  ggplot(speed.long, aes(x=processor, y=speed, fill=source)) +
    geom_col(position=position_dodge(width=0.8, reverse=TRUE), width=0.72) +
    scale_fill_manual(values=c(Actual="#0072B2",
                               `Expectation, based on local synthetic benchmark`=
                                 "#D55E00"),
                      breaks=c("Actual",
                               "Expectation, based on local synthetic benchmark"),
                      labels=c("Actual", "Expected")) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
    scale_x_discrete(labels=processor.labels) +
    coord_flip() +
    labs(title="Actual speed vs. expectation based on synthetic benchmark",
         x=NULL, y="Speed (model iterations / sec)", fill=NULL) +
    themeBenchmark() +
    theme(axis.text.y=element_text(angle=0),
          panel.grid.major.x=element_blank(),
          legend.position=c(0.98, 0.07),
          legend.justification=c(1, 0),
          legend.direction="vertical",
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
)
dev.off()

# Round tables
catage <- rnd(catage, 2:4, c(1,1,2))
catage$difference <- paste0(100 * catage$difference, "%")
passmark <- rnd(passmark, "difference", 2)
passmark$difference <- paste0(100 * passmark$difference, "%")
regression <- rnd(regression, c(2,4,5), c(1, 5, 1))

# Write tables
write.taf(catage, dir="report")
write.taf(passmark, dir="report")
write.taf(regression, dir="report")
