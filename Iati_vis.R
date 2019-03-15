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
       subtitle = 'Breakdown of Earmarking Type From Donor Funds by Year', 
       x = 'Organisation Type',
       y = '') 

#budget bars with low alpha

plot2 <- plot1 + geom_segment(aes(y = annualtotal, yend=0, color= "Total budget"), alpha = 0.2, 
                              size = 15) +
  scale_color_manual(values = '#CEE0DC')

#earmarking bars

gm <- plot2 + geom_col(aes(y = emamount, fill = earmarkingtype), width = 0.2) +
  scale_fill_manual(values = c('#cae7b9', '#f3de8a', '#eb9486', '#7e7f9a', '#97a7b3'), 
                    name = 'Earmarking Level')

iati_plot_base <- gm + facet_grid(cols = vars(year)) +
  scale_y_continuous(breaks = c(10000000, 20000000, 30000000), 
                     labels = c("$10 million", "$20 million", "$30 million")) 


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
  strip.text = element_text(size = 20, family = 'Arial'),
  legend.background = element_blank(),
  legend.key = element_blank(),
  plot.title = element_text(size = 30),
  plot.subtitle = element_text(size = 16, face = 'italic'),
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 20),
  panel.border = element_rect(colour="black",size=1, fill = NA)
  
) + guides(colour = guide_legend(title = NULL,
                                 order = 0,
                                 override.aes = list(size = 7)
                                 ),
           fill = guide_legend(order = 1)
           )

ggsave("bar_by_year.png", plot = last_plot, width = 1300, height = 681, units = 'mm') 

#------------------------------------------



unem <- n %>% filter(earmarkingtype == 'Unearmarked')
tightly <- n %>% filter(earmarkingtype == 'Tightly earmarked')

line_iati <- unem %>% ggplot(aes(x = year, y = emamount, fill = orgttype)) + geom_col(position = 'dodge') +
  labs(title = 'Unearmarked funds', subtitle = 'Funds by organisation that are delivered without any prescriptive earmarking')
 

line_iati

emtype <- unique(n$earmarkingtype)

for (i in emtype){
  
  options(scipen = 10)
  
  df <- n %>% filter(earmarkingtype == i)
  
  print(ggplot(df, aes(x = year, y = emamount, fill = orgttype)) + geom_col(position = 'dodge') +
          scale_fill_manual(values = c('#cae7b9', '#f3de8a', '#eb9486', '#7e7f9a', '#97a7b3')) +
    labs(title = i, subtitle = paste('Funds by organisation that are issued with an earmarking level of:', i),
         x = 'Year', y = 'Earmarked funds') +
      theme(axis.text.x = element_text(vjust = 1, hjust = 1, family = 'Arial', size = 11, color = 'Black'),
        axis.text.y = element_text(family = 'Arial', size = 11, color = 'Black'),
        plot.background = element_rect(fill = 'grey85'),
        panel.background = element_rect(fill = 'grey85'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = '#8AA1B1'),
        strip.text = element_text(size = 20, family = 'Arial'),
        legend.background = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 16, face = 'italic'),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20),
        panel.border = element_rect(colour="black",size=1, fill = NA)
        
      ) + guides(fill = guide_legend(title = 'Org Type',
                                       order = 0,
                                       override.aes = list(size = 7)
      )
    )
  )
  ggsave(filename = paste(i,".png"), plot = last_plot())
}




