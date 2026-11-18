# Plot CPU variables

# Before: specs.csv, speed_fit.csv (output)
# After:  cpu_cache.png, cpu_cores.png, cpu_efficiency.png, cpu_freq.png,
#         cpu_nm.png, cpu_speed_data.png, cpu_speed_fit.png, cpu_speed_freq.png,
#         cpu_speed_nm.png, cpu_threads.png, cpu_watt.png (report)

library(TAF)
library(ggplot2)

mkdir("report")

# Read data
specs <- read.taf("output/specs.csv", colClasses=c(Date="Date"))
speed.fit <- read.taf("output/speed_fit.csv", colClasses=c(Date="Date"))

# Univariate time series
taf.png("cpu_cache")
print(
  ggplot(specs, aes(x=Date, y=Cache)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(0.4, 32),
                  breaks=c(0.5, 1, 2, 4, 8, 16, 32),
                  labels=c("0.5", "1", "2", "4", "8", "16", "32")) +
    labs(title="CPU: On-chip memory", x=NULL, y="Cache (MB)") +
    themeLaptops()
)
dev.off()

taf.png("cpu_cores")
print(
  ggplot(specs, aes(x=Date, y=Cores)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_continuous(limits=c(0.5, 8.5), breaks=c(1, 2, 4, 6, 8)) +
    labs(title="CPU: Processing units", x=NULL, y="Number of cores") +
    themeLaptops()
)
dev.off()

taf.png("cpu_efficiency")
print(
  ggplot(specs, aes(x=Date, y=Speed/W)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(4.4, 160)) +
    labs(title="CPU: Efficiency", x=NULL, y="Single-thread performance per watt") +
    themeLaptops()
)
dev.off()

taf.png("cpu_freq")
print(
  ggplot(specs, aes(x=Date, y=Turbo/1000)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(2, 6)) +
    labs(title="CPU: Maximum frequency", x=NULL, y="Turbo (GHz)") +
    themeLaptops()
)
dev.off()

taf.png("cpu_nm")
print(
  ggplot(specs, aes(x=Date, y=nm)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(2.5, 150)) +
    labs(title="CPU: Semiconductor technology", x=NULL,
         y="Process node (nm)") +
    themeLaptops()
)
dev.off()

taf.png("cpu_speed_data")
print(
  ggplot(specs, aes(x=Date, y=Speed)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(350, 5000)) +
    labs(title="CPU: Benchmark performance", x=NULL,
         y="Score (PassMark single thread)") +
    themeLaptops()
)
dev.off()

taf.png("cpu_speed_fit")
print(
  ggplot(specs, aes(x=Date, y=Speed)) +
    geom_line(data=speed.fit, aes(x=Date, y=Speed), inherit.aes=FALSE,
              color="#D55E00", linewidth=0.7) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_log10(limits=c(350, 5000)) +
    labs(title="CPU: Performance trend", x=NULL,
         y="Score (PassMark single thread)") +
    themeLaptops()
)
dev.off()

taf.png("cpu_threads")
print(
  ggplot(specs, aes(x=Date, y=Threads)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    scale_y_continuous(limits=c(0.5, 16.5),
                       breaks=c(0.9, 2.1, 4, 6, 8, 10, 12, 14, 16),
                       labels=round) +
    labs(title="CPU: Parallel processing capacity", x=NULL,
         y="Number of threads") +
    themeLaptops()
)
dev.off()

taf.png("cpu_watt")
print(
  ggplot(specs, aes(x=Date, y=W)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_date(date_breaks="5 years", date_labels="%Y") +
    coord_cartesian(ylim=lim(specs$W)) +
    labs(title="CPU: Processor power", x=NULL,
         y="Thermal design power (W)") +
    themeLaptops()
)
dev.off()

# Relationships between variables
taf.png("cpu_speed_freq")
print(
  ggplot(specs, aes(x=Turbo/1000, y=Speed)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_x_log10() +
    scale_y_log10(limits=c(350, 5000)) +
    labs(title="CPU: Frequency and performance", x="Turbo (GHz)",
         y="Score (PassMark single thread)") +
    themeLaptops()
)
dev.off()

taf.png("cpu_speed_nm")
print(
  ggplot(specs, aes(x=sqrt(nm), y=Speed)) +
    geomLaptopPoints() +
    scaleLaptopFill() +
    scale_y_log10(limits=c(350, 5000)) +
    labs(title="CPU: Performance and semiconductor technology",
         x="sqrt(Process node)", y="Score (PassMark single thread)") +
    themeLaptops()
)
dev.off()
