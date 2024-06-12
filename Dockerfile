FROM openanalytics/r-ver:4.4.0
LABEL maintainer="rosswilson-nz"
ARG shinyapp CMOR-Evidence-table

RUN R -q -e "install.packages(c('shiny', 'rmarkdown', 'DT', 'tidyverse', 'abind'))"

RUN mkdir /opt/app
COPY app.R /opt/app/
COPY data/ /opt/app/data
COPY scripts/ /opt/app/scripts
COPY www/ /opt/app/www
COPY reports/ /opt/app/reports

COPY Rprofile.site /usr/local/lib/R/etc/

RUN useradd shiny

WORKDIR /opt/app
USER shiny

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/opt/app')"]
