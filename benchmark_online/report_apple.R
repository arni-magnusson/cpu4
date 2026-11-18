# Plot comparison plots for Apple, Intel, and AMD

# Before: apple.csv, companies.csv (data)
#         companies_fit.csv, companies_lag.csv, i7_high.csv,
#         r7_high.csv (output)
# After:  companies_data.png, companies_fit.png, companies_lag.png,
#         companies_macbook.png (report)

library(TAF)

mkdir("report")

# Read tables
apple <- read.taf("data/apple.csv", colClasses=c(Date="Date"))
companies <- read.taf("data/companies.csv", colClasses=c(Date="Date"))
companies.fit <- read.taf("output/companies_fit.csv", colClasses=c(Date="Date"))
companies.lag <- read.taf("output/companies_lag.csv")
i7.high <- read.taf("output/i7_high.csv", colClasses=c(Date="Date"))
r7.high <- read.taf("output/r7_high.csv", colClasses=c(Date="Date"))

# Prepare for plotting
score <- "Score (PassMark single thread)"

# Companies: Apple vs Intel vs AMD
taf.png("companies_data", width=1200)
print(
  ggplot(companies, aes(x=Date, y=Speed, color=Company)) +
    geom_point(size=2) +
    scale_color_manual(values=benchmark.colors,
                       breaks=c("Apple", "Intel", "AMD")) +
    scale_y_log10() +
    labs(title="Processor performance by company", x=NULL, y=score,
         color=NULL) +
    themeBenchmark() +
    theme(legend.position=c(0.08, 0.98),
          legend.justification=c(0, 1),
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
)
dev.off()

# Companies with fitted regression lines
taf.png("companies_fit", width=1200)
print(
  ggplot(companies, aes(x=Date, y=Speed, color=Company)) +
    geom_line(data=companies.fit, linewidth=1, show.legend=FALSE) +
    geom_point(size=2) +
    scale_color_manual(values=benchmark.colors,
                       breaks=c("Apple", "Intel", "AMD")) +
    scale_y_log10() +
    labs(title="Processor performance by company", x=NULL, y=score,
         color=NULL) +
    themeBenchmark() +
    theme(legend.position=c(0.08, 0.98),
          legend.justification=c(0, 1),
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
)
dev.off()

# Lag between companies
taf.png("companies_lag")
companies.lag <- companies.lag[order(companies.lag$Lag),]
companies.lag$Company <- factor(companies.lag$Company,
                                levels=companies.lag$Company)
print(
  ggplot(companies.lag, aes(x=Company, y=Lag, fill=Company)) +
    geom_col(width=0.72) +
    scale_fill_manual(values=benchmark.colors, guide="none") +
    scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
    labs(title="Lag until company reaches the CPU performance of the best company",
         x=NULL, y="Lag (yrs)") +
    themeBenchmark() +
    theme(panel.grid.major.x=element_blank())
)
dev.off()

# MacBook Air vs Pro
apple$Type <- ifelse(grepl("-Pro$", apple$Model), "Pro", "Air")
apple$Type <- factor(apple$Type, levels=c("Pro", "Air"))
taf.png("companies_macbook", width=1200)
print(
  ggplot(apple, aes(x=Date, y=Speed, color=Type, shape=Type)) +
    geom_point(size=2.2) +
    scale_color_manual(values=c(Pro="#0072B2", Air="#D55E00")) +
    scale_shape_manual(values=c(Pro=16, Air=1)) +
    scale_y_log10() +
    labs(title="Apple MacBook", x=NULL, y=score, color=NULL, shape=NULL) +
    themeBenchmark() +
    theme(legend.position=c(0.02, 0.98),
          legend.justification=c(0, 1),
          legend.background=element_rect(fill="white", color=NA),
          legend.key=element_rect(fill="white", color=NA),
          legend.text=element_text(size=10))
)
dev.off()
