FROM rocker/r-ver:4.2.3
RUN R -e "install.packages(c('ggplot2', 'data.table', 'haven', 'checkmate', 'lme4'))"
