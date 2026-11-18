# Produce plots and tables for report

# Before: catage.csv (output)
# After:  speed.csv, speed.png (report)

library(TAF)
library(ggplot2)

mkdir("report")

# Read data
catage <- read.taf("output/catage.csv")

# Order machines from oldest to newest so the newest appears at the top
catage$machine <- factor(catage$machine,
                         levels=c("m4700", "p15gen1", "p3560", "p1gen8"))
catage$job <- factor(catage$job, levels=c("short", "sustained"))

# Plot
taf.png("speed")
print(
  ggplot(catage, aes(x=speed, y=machine, fill=job)) +
    geom_col(position=position_dodge(width=0.8, reverse=TRUE), width=0.72) +
    scale_fill_manual(values=c(short="#0072B2", sustained="#D55E00"),
                      breaks=c("short", "sustained"),
                      labels=c("Short", "Sustained")) +
    scale_x_continuous(expand=expansion(mult=c(0, 0.1))) +
    scale_y_discrete(labels=c(
      m4700="Dell Precision M4700\nIntel i7-3740QM",
      p15gen1="Lenovo P15 Gen 1\nIntel i7-10875H",
      p3560="Dell Precision 3560\nIntel i7-1165G7",
      p1gen8="Lenovo P1 Gen 8\nIntel u9-285H"
    )) +
    labs(title="Speed measurements", x="Speed (model iterations / sec)",
         y=NULL, fill=NULL) +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold", hjust=0.5,
                                  margin=margin(b=12)),
          axis.title.x=element_text(margin=margin(t=12)),
          panel.grid.major=element_line(color="grey85", linewidth=0.3),
          panel.grid.minor=element_blank(),
          plot.margin=margin(24, 24, 24, 24),
          legend.position=c(0.98, 0.02),
          legend.justification=c(1, 0),
          legend.direction="vertical",
          legend.text=element_text(size=10),
          legend.key.spacing.y=grid::unit(0.08, "cm"),
          legend.background=element_rect(fill="white", color=NA),
          legend.box.background=element_blank())
)
dev.off()

# Prepare summary table
speed <- aggregate(speed~machine, catage, mean)
speed$relative <- speed$speed / min(speed$speed)
speed <- rnd(speed, 2:3, 1:2)

# Write table
write.taf(speed, dir="report")
