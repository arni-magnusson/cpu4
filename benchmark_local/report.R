# Produce plots and tables for report

# Before: info.csv, passmark.csv (data), passmark.csv (output)
# After:  cpu_benchmark.png, cpu_multicore.png, effective_cores.png,
#         memory_benchmark.png, memory_details.png (report)

library(TAF)
library(ggplot2)

mkdir("report")

# Read data
info <- read.taf("data/info.csv")
memory.details <- read.taf("data/passmark.csv")
passmark <- read.taf("output/passmark.csv")

# Establish processor order and labels
machine.levels <- c("m4700", "p15gen1", "p3560", "p1gen8")
processor.labels <- setNames(paste("Intel", gsub(" ", "-", info$processor)),
                             info$machine)
machine.labels <- setNames(paste(info$name, processor.labels, sep="\n"),
                           info$machine)
passmark$machine <- factor(passmark$machine, levels=machine.levels)
benchmark.colors <- c(blue="#0072B2", orange="#D55E00", green="#009E73")

# Single-thread benchmark
single <- passmark
taf.png("cpu_benchmark")
print(
  ggplot(single, aes(x=single, y=machine)) +
    geom_col(width=0.72, fill=benchmark.colors["blue"]) +
    scale_x_continuous(expand=expansion(mult=c(0, 0.1))) +
    scale_y_discrete(labels=machine.labels) +
    labs(title="Single-thread benchmark",
         x="Score (PassMark single thread)", y=NULL) +
    themeBenchmark()
)
dev.off()

# Multithread benchmark
multi <- passmark
taf.png("cpu_multicore")
print(
  ggplot(multi, aes(x=multi, y=machine)) +
    geom_col(width=0.72, fill=benchmark.colors["orange"]) +
    scale_x_continuous(expand=expansion(mult=c(0, 0.1))) +
    scale_y_discrete(labels=machine.labels) +
    labs(title="Multithread benchmark",
         x="Score (PassMark multithread)", y=NULL) +
    themeBenchmark()
)
dev.off()

# Effective cores
effective <- rbind(
  data.frame(machine=passmark$machine, cores=passmark$physcores,
             type="Physical cores"),
  data.frame(machine=passmark$machine, cores=passmark$effcores,
             type="Effective index")
)
effective$type <- factor(effective$type,
                        levels=c("Physical cores", "Effective index"))
taf.png("effective_cores")
print(
  ggplot(effective, aes(x=cores, y=machine, fill=type)) +
    geom_col(position="identity", width=0.72) +
    scale_fill_manual(values=c(`Physical cores`="#E5C4AC",
                               `Effective index`=
                                 unname(benchmark.colors["orange"])),
                      breaks=c("Physical cores", "Effective index")) +
    scale_x_continuous(expand=expansion(mult=c(0, 0.1))) +
    scale_y_discrete(labels=machine.labels) +
    labs(title="Effective core index = multithread score / single-thread score",
         x="Cores", y=NULL, fill=NULL) +
    themeBenchmark() +
    theme(legend.position=c(0.98, 0.02),
          legend.justification=c(1, 0),
          legend.direction="vertical",
          legend.text=element_text(size=10),
          legend.key.spacing.y=grid::unit(0.08, "cm"),
          legend.background=element_rect(fill="white", color=NA),
          legend.box.background=element_blank())
)
dev.off()

# Memory
memory <- merge(memory.details, info[c("machine", "module", "year")])
memory$machine <- factor(memory$machine, levels=machine.levels)
memory.benchmark.labels <- setNames(
  paste0(info$name, "\n", info$module, " (", info$year, ")"),
  info$machine
)
memory.detail.labels <- setNames(
  paste0(info$module, " (", info$year, ")"),
  info$machine
)

taf.png("memory_benchmark")
print(
  ggplot(memory, aes(x=memory, y=machine)) +
    geom_col(width=0.72, fill=benchmark.colors["blue"]) +
    scale_x_continuous(expand=expansion(mult=c(0, 0.1))) +
    scale_y_discrete(labels=memory.benchmark.labels) +
    labs(title="Memory benchmark", x="Score", y=NULL) +
    themeBenchmark()
)
dev.off()

memory.metrics <- rbind(
  data.frame(machine=memory$machine, metric="read", score=memory$read),
  data.frame(machine=memory$machine, metric="write", score=memory$write),
  data.frame(machine=memory$machine, metric="latency", score=memory$latency)
)
memory.metrics$metric <- factor(memory.metrics$metric,
                                levels=c("read", "write", "latency"))
taf.png("memory_details")
print(
  ggplot(memory.metrics, aes(x=score, y=machine, fill=metric)) +
    geom_col(width=0.72, show.legend=FALSE) +
    scale_fill_manual(values=c(read="#CC79A7",
                               write=unname(benchmark.colors["orange"]),
                               latency=unname(benchmark.colors["green"]))) +
    scale_x_continuous(expand=expansion(mult=c(0, 0.1))) +
    scale_y_discrete(labels=memory.detail.labels) +
    facet_wrap(~metric, ncol=1, scales="free_x",
               labeller=as_labeller(c(read="Read (MB/s)",
                                      write="Write (MB/s)",
                                      latency="Latency (ns)"))) +
    labs(x=NULL, y=NULL) +
    themeBenchmark() +
    theme(strip.text=element_text(face="bold"),
          strip.background=element_blank())
)
dev.off()
