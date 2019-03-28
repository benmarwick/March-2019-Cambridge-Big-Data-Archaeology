library(gtrendsR)
library(ggplot2)
library(here)

res <- gtrends(c("data science", "big data"), geo = c("US"))
plot(res) +
  geom_smooth(alpha = 0.2) +
  ggtitle("Google search trends") +
  theme_bw(base_size = 18)

ggsave(
       filename = "google_trends_data_sci_v_big_data.png",
       path = here('figures'),
       height = 9,
       width = 12)

