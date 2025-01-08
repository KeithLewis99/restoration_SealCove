# BACI plot explained

df <- data.frame(
  time = c("A", "A", "B", "B"),
  trt = c("I", "C", "I", "C"),
  rv = c(5, 2, 5, 2)
)
df


ggplot(df, aes(x = reorder(time, desc(time)), y = rv)) +
  geom_point() +
  geom_segment(aes(x = 1, xend = 2, y = 5, yend = 5), 
               color = "blue", linetype = "dashed", size = 1) +  # First horizontal segment
  geom_segment(aes(x = 1, xend = 2, y = 2, yend = 2), 
               color = "red", linetype = "solid", size = 1) +    # Second horizontal segment
  labs(
    title = "BACI Plot showing relationship between \n parameters and variables",
    x = "Time",
    y = "Response Variable"
  ) +
  ylim(0, 7) +
  theme_minimal() +
  annotate("text", x = 2.3, y = 2, label = "Control") +
  annotate("text", x = 2.3, y = 5, label = "Construction") +
  annotate("text", x = 2, y = 1.5, label = "Intercept") +
  annotate("text", x = 2, y = 4.5, label = "Intercept + trt") +
  annotate("text", x = 1, y = 6, label = "Intercept + trt \n + time + trt:time") +
  annotate("text", x = 1, y = 1.5, label = "Intercept + time")

ggsave("output/plot_baci_explained.png")
