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
options(scipen = -10 )
plot1 <- ggplot(n, aes(x = orgttype, xend = orgttype))

#gg <- gg + geom_segment(aes(x=budgeted, xend=0, color="Budgeted"), size=10)

plot2 <- plot1 + geom_segment(aes(y = annualtotal, yend=0, color= "Total budget"), alpha = 0.1, size = 14)

#gg <- gg + geom_segment(aes(x=actual, xend=0, color="Actual"), size=5)

gm <- plot2 + geom_segment(aes(y = emamount, yend=0, color = earmarkingtype), size = 5)

iati_plot_base <- gm + facet_grid(cols = vars(year))

iati_plot_base

+ theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

