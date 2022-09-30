crimson <- '#dc143c'

# ggplot theme

theme_thesis <- function() {
  theme_grey(base_family="Times") +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = 'black'))
}

# testing

within_all + labs(title = 'Example title', 
                  x = 'testing x-axis', 
                  y = 'testing y-axis',
                  subtitle = 'placeholder subtitle') + theme_classic(base_family="Times")

