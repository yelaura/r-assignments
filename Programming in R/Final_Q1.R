filename <- "C:\\Users\\lye\\Downloads\\atussum_2016.dat"

df <- read.csv(filename)
df_new <- as.data.frame(df$TESEX)
df_new$t010102 <- as.data.frame(df$t010102)

mins <- seq(10,120, by=0.1)
results <- data.frame()

for (n in mins){
  df$insomnia <- df$t010102 >= n
  ans <- chisq.test(df$TESEX, df$insomnia)
  results <- rbind(results, c(n, ans$p.value))
}

names(results) <- c('tol', 'pv')

results$significant <- results$pv < 0.05 

require(ggplot2)
ggplot(results, aes(x=tol, y=pv, color=significant)) + 
  geom_line(size=1, data = results[-results$significant, ]) + 
  geom_line(size=1, data=results[results$significant,]) +
  labs(title="Gender Disparity for Insomnia Depending on Definition", 
       x="Insomnia Definition Minimum (Minutes)",
       y="Chi-Square Independence Test P-Value",
       legend="Significant?") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=18))+
  scale_color_discrete(name="Significant?\n", 
                       labels=c("No", "Yes"))
