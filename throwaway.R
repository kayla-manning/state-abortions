crimson <- '#dc143c'

# ggplot theme

theme_thesis <- function() {
  theme_classic(base_size=12, base_family="Times") %+replace%
    theme(
      plot.title = element_text(size=20,  family="Times", face = "bold"),
      plot.subtitle = element_text(size=12,  family="Times", color="#717171", 
                                   face = "italic",
                                   margin = margin(t = 0, r = 0, b = 1, l = 0)),
      plot.caption = element_text(size=8,  family="Times", hjust = 1),
      axis.text.x = element_text(size=10,  family="Times"),
      axis.title.x = element_text(size=14, family="Times", 
                                  margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                  size=14, family="Times", angle=90, face ='bold'),
      legend.title = element_text(size=10, family="Times"),
      legend.text = element_text(size=10, family="Times"),
      legend.position = "bottom",
      axis.ticks = element_blank()
    )
}

# testing

within_all + labs(title = 'example title', 
                  x = 'testing x-axis', 
                  y = 'testing y-axis',
                  subtitle = 'placeholder subtitle') + theme_thesis()
