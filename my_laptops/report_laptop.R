# Plot laptop variables

# Before: specs.csv (output)
# After:  laptop_display.png, laptop_memory.png, laptop_power.png,
#         laptop_storage.png, laptop_thick.png, laptop_weight.png,
#         screen_density.png, screen_pixels.png, specs.csv, specs.md,
#         storage_memory_prop.png, storage_memory_text.png,
#         thick_watt.png (report)

library(TAF)
library(knitr)
library(ggplot2)

mkdir("report")

# Read data
specs <- read.taf("output/specs.csv", colClasses=c(Date="Date"))

# Plot laptop variables
taf.png("laptop_display")
print(
  ggplot(specs, aes(x=Date, y=Disp)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_continuous(limits=c(13, 16.2), breaks=13:16) +
    labs(title="Laptop: Display", x=NULL, y="Screen size (in)") +
    themeLaptops()
)
dev.off()

taf.png("laptop_memory")
print(
  ggplot(specs, aes(x=Date, y=RAM)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(0.4, 75),
                  breaks=c(0.5, 1, 2, 4, 8, 16, 32, 64),
                  labels=c("0.5", "1", "2", "4", "8", "16", "32", "64")) +
    labs(title="Laptop: Memory", x=NULL, y="Memory (GB)") +
    themeLaptops()
)
dev.off()

taf.png("laptop_power")
print(
  ggplot(specs, aes(x=Date, y=Power)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    coord_cartesian(ylim=lim(specs$Power)) +
    labs(title="Laptop: Power adapter", x=NULL, y="Power (W)") +
    themeLaptops()
)
dev.off()

taf.png("laptop_storage")
print(
  ggplot(specs, aes(x=Date, y=Storage)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(0.03, 4),
                  breaks=c(0.05, 0.1, 0.2, 0.5, 1, 2, 4),
                  labels=c("0.05", "0.1", "0.2", "0.5", "1", "2", "4")) +
    labs(title="Laptop: Storage drive", x=NULL, y="Storage (TB)") +
    themeLaptops()
)
dev.off()

taf.png("laptop_thick")
print(
  ggplot(specs, aes(x=Date, y=Height)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_continuous(limits=lim(specs$Height),
                       expand=expansion(mult=0)) +
    labs(title="Laptop: Thickness", x=NULL, y="Height (cm)") +
    themeLaptops()
)
dev.off()

taf.png("laptop_weight")
print(
  ggplot(specs, aes(x=Date, y=Weight)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    labs(title="Laptop: Weight", x=NULL, y="Weight (kg)") +
    themeLaptops()
)
dev.off()

# Plot screen variables
taf.png("screen_density")
print(
  ggplot(specs, aes(x=Date, y=ppi)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(80, 300)) +
    labs(title="Laptop: Pixel density", x=NULL, y="Density (PPI)") +
    themeLaptops()
)
dev.off()

taf.png("screen_pixels")
print(
  ggplot(specs, aes(x=Date, y=h)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(720, 2550), breaks=c(768, 1080, 1440, 2400)) +
    labs(title="Laptop: Screen resolution", x=NULL, y="Vertical pixels") +
    themeLaptops()
)
dev.off()

# Relationship between variables
taf.png("storage_memory_prop")
specs$Percentage <- specs$RAM / (1000*specs$Storage)
print(
  ggplot(specs, aes(x=Date, y=Percentage)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    coord_cartesian(ylim=lim(specs$Percentage, 1.15)) +
    labs(title="Memory tends to be around 2% of storage", x=NULL,
         y="Memory / Storage") +
    themeLaptops()
)
dev.off()

taf.png("storage_memory_text")
specs$Percentage <- specs$RAM / (1000*specs$Storage)
n <- nrow(specs)
storage.labels <- data.frame(
  Date=c(specs$Date[1], specs$Date[1], specs$Date[2], specs$Date[2],
         specs$Date[n], specs$Date[n]),
  Percentage=c(specs$Percentage[1]+0.003, specs$Percentage[1]-0.003,
               specs$Percentage[2]+0.003, specs$Percentage[2]-0.003,
               specs$Percentage[n]+0.003, specs$Percentage[n]-0.003),
  label=c(paste("512 MB", "RAM", sep="\n"),
          paste("40 GB", "HDD", sep="\n"),
          paste("4 GB", "RAM", sep="\n"),
          paste("100 GB", "HDD", sep="\n"),
          paste("64 GB", "RAM", sep="\n"),
          paste("2 TB", "SSD", sep="\n"))
)
print(
  ggplot(specs, aes(x=Date, y=Percentage)) +
    geomLaptopPoints() +
    geom_hline(yintercept=0.02, linetype=3, color="#D55E00") +
    geom_text(data=storage.labels,
              aes(x=Date, y=Percentage, label=label),
              inherit.aes=FALSE, color="grey35", size=3.0, hjust=0.5,
              lineheight=1.1) +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    coord_cartesian(ylim=lim(specs$Percentage, 1.15)) +
    labs(title="Memory tends to be around 2% of storage", x=NULL,
         y="Memory / Storage") +
    themeLaptops()
)
dev.off()

taf.png("thick_watt")
p1gen8 <- specs[specs$Laptop == "Lenovo-P1-Gen8",]
t61 <- specs[specs$Laptop == "Lenovo-ThinkPad-T61",]
thickness.labels <- rbind(
  data.frame(Height=p1gen8$Height, W=p1gen8$W+6,
             label=paste("new", "premium", "aluminium", sep="\n")),
  data.frame(Height=t61$Height, W=t61$W-6,
             label=paste("old", "workstation", "plastic", sep="\n"))
)
print(
  ggplot(specs, aes(x=Height, y=W)) +
    geom_smooth(method="lm", formula=y~x, se=FALSE,
                color="#D55E00", linewidth=0.7) +
    geomLaptopPoints() +
    geom_text(data=thickness.labels,
              aes(x=Height, y=W, label=label),
              inherit.aes=FALSE, color="grey35", size=3.2) +
    scaleLaptopFill() +
    labs(title="Processor power and laptop thickness",
         x="Thickness (cm)", y="Thermal design power (W)") +
    themeLaptops()
)
dev.off()

# Format specs
specs$Hybrid[specs$Hybrid == "-"] <- ""
specs$Cores <- paste0(specs$Cores, specs$Hybrid)
cols <- c("Date", "Laptop", "Model", "Cores", "Turbo", "RAM", "Storage","W")
specs <- specs[cols]
names(specs)[names(specs) == "Date"] <- "Year"
names(specs)[names(specs) == "Model"] <- "Processor"
names(specs)[names(specs) == "RAM"] <- "Memory"
specs$Year <- as.integer(format(specs$Year, "%Y"))
specs$Turbo <- format(round(specs$Turbo / 1000, 1))
specs$Memory <- formatC(specs$Memory)
specs$Storage <- formatC(specs$Storage)

# Write to Markdown format
specs.md <- kable(specs, "pipe", align="lllrrrrr")
writeLines(specs.md, "report/specs.md")

# Write tables
write.taf(specs, dir="report")
