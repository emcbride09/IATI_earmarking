require(googlesheets)
require(tidyverse)
library(scales)

gs_auth(new_user = TRUE)

mysheets <- gs_ls()

iati_em <- gs_title("IATI_earmarking")

n <- iati_em %>% gs_read(ws = "Sheet1")

n <- n %>% rename(earmarkingtype = `Earmarking Type`,
                  orgttype = `Org Type`,
                  emamount = `Total Amount`,
                  year = Year,
                  annualtotal = `Annual Total`)


n$annualtotal <- as.numeric(n$annualtotal)
n$emamount <- as.numeric(n$emamount)
n$orgttype <- as.factor(n$orgttype)
n$earmarkingtype <- as.factor(n$earmarkingtype)

#using code from https://gist.github.com/hrbrmstr/035f998517de2384e9962cff7df874bd

#gg <- ggplot(data=df, aes(y=bucket, yend=bucket))
#setting scipen removes scientific notation
options(scipen = 10)

plot1 <- ggplot(n, aes(x = orgttype, xend = orgttype)) +
  labs(title = 'Donor Funds Earmarking 2015-2018', 
       subtitle = 'Breakdown of Earmarking Type From Initial Funds Release', 
       x = 'Organisation Type',
       y = '') 

#budget bars with low alpha

plot2 <- plot1 + geom_segment(aes(y = annualtotal, yend=0, color= "Total budget"), alpha = 0.2, size = 30) +
  scale_color_manual(values = '#CEE0DC')

#earmarking bars

gm <- plot2 + geom_col(aes(y = emamount, fill = earmarkingtype), size = 5) +
  scale_fill_manual(values = c('#DB2B39', '#99D5C9', '#3AAFB9', '#074F57', '#CEE0DC'))

iati_plot_base <- gm + facet_grid(cols = vars(year)) +
  scale_y_continuous(breaks = c(10000000, 20000000, 30000000), labels = c("$10 million", "$20 million", "$30 million")) 


#theme for bar plot

iati_plot_base + theme(
  axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, family = 'Arial', size = 11, color = 'Black'),
  axis.text.y = element_text(family = 'Arial', size = 11, color = 'Black'),
  plot.background = element_rect(fill = '#8AA1B1'),
  panel.background = element_rect(fill = '#8AA1B1'),
  panel.grid.major.y = element_line(colour = 'grey20'),
  panel.grid.minor.y = element_line(colour = 'grey50'),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  strip.background = element_rect(fill = '#8AA1B1'),
  strip.text = element_text(size = 20, family = 'Arial')
  
) + guides(colour = guide_legend(overide.aes = list(size = 1)))
  
  
  

