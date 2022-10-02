crimson <- '#dc143c'

# ggplot theme

theme_thesis <- function() {
  theme_grey(base_family="Times") +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = 'black'))
}


