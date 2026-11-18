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

# Plot
taf.png("speed")
print(
  ggplot(catage, aes(x=speed, y=machine, fill=job)) +
    geom_col(position=position_dodge(width=0.8), width=0.75, color="black") +
    scale_fill_manual(values=c(short="gray30", sustained="gray90"),
                      breaks=c("short", "sustained"),
                      labels=c("Short", "Sustained")) +
    scale_x_continuous(expand=expansion(mult=c(0, 0.05))) +
    labs(title="Speed measurements", x="Speed (model iterations / sec)",
         y=NULL, fill=NULL) +
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          legend.position="top")
)
dev.off()

# Prepare summary table
speed <- aggregate(speed~machine, catage, mean)
speed$relative <- speed$speed / min(speed$speed)
speed <- rnd(speed, 2:3, 1:2)

# Write table
write.taf(speed, dir="report")
