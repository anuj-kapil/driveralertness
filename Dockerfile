# Only run this after making packrat/packrat.lock by
# running manage_packages.R

FROM rocker/rstudio:3.6.1

RUN apt-get update && apt-get install zlib1g-dev

COPY ./packrat/packrat.lock packrat/

RUN install2.r packrat

RUN Rscript -e 'packrat::restore()'

# Modify Rprofile.site so R loads packrat library by default
RUN echo '.libPaths("/packrat/lib/x86_64-pc-linux-gnu/3.6.1")' >> /usr/local/lib/R/etc/Rprofile.site