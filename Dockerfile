FROM openanalytics/r-ver:4.6.0 as builder
RUN echo 'options(shiny.port = 3838, shiny.host = "0.0.0.0")' >> /usr/local/lib/R/etc/Rprofile.site
RUN mkdir -p /opt/shiny/app
COPY app.R /opt/shiny/app/
COPY data/ /opt/shiny/app/data
COPY scripts/ /opt/shiny/app/scripts
COPY www/ /opt/shiny/app/www
COPY reports/ /opt/shiny/app/reports

RUN apt-get update \
  && apt-get install -y \
    libwebpmux3 \
    # needs a sans font (eg dejavu, or helvetica/arial) or default sans-seri
    # reverts to numbus-sans
    #fonts-dejavu-web \
    #fonts-freefont-otf \
    #fonts-clear-sans \
    fonts-cantarell \
  && rm -rf /var/lib/apt/lists/* \
  && R -q -e "install.packages(c('shiny', 'rmarkdown', 'DT', 'tidyverse', 'abind'))"

RUN chmod -R og+rx /opt/shiny/app

## the following to make sure docker COPYs the symlink, not dereference it
RUN mkdir /opt/shiny/etc \
  && ln -s /usr/share/zoneinfo/Etc/UTC /opt/shiny/etc/localtime \
  && cp -r /etc/fonts /opt/shiny/etc/fonts

################################################

#FROM gcr.io/distroless/base-debian13:debug-nonroot
#FROM gcr.io/distroless/cc-debian13:debug-nonroot
FROM gcr.io/distroless/cc-debian13:nonroot
#FROM gcr.io/distroless/base-debian13:nonroot
LABEL developer="rosswilson-nz" \
      maintainer="wimg"
COPY --from=builder /opt/shiny/app /opt/shiny/app
COPY --from=builder /usr/local/bin/Rscript /usr/local/bin/R /usr/local/bin/
COPY --from=builder /usr/local/lib/R /usr/local/lib/R
COPY --from=builder /usr/lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu
COPY --from=builder /etc/alternatives /etc/alternatives
COPY --from=builder /usr/share/fonts /usr/share/fonts
COPY --from=builder /var/cache/fontconfig /var/cache/fontconfig
COPY --from=builder /opt/shiny/etc /etc
COPY fonts.conf /etc/fonts/conf.d/00-local.conf
COPY --from=builder /usr/bin/uname /usr/bin/which /usr/bin/sh /usr/bin/bash /usr/bin/rm /usr/bin/ls /usr/bin/wc /usr/bin/sed /usr/bin/
WORKDIR /opt/shiny/app
## these should be set by R launch script
#ENV R_HOME=/usr/local/lib/R
#ENV R_HOME_DIR=/usr/local/lib/R
#ENV R_VERSION=4.5.3
#ENV LD_LIBRARY_PATH=/usr/local/lib/R/lib:/usr/local/lib:/usr/lib/x86_64-linux-gnu
ENV PATH=/usr/local/bin:/usr/bin:/bin
EXPOSE 3838
USER 65532
SHELL ["/bin/sh", "-c"]
ENTRYPOINT [""]
#CMD ["/usr/local/lib/R/bin/exec/R", "-f", "/opt/shiny/shiny.R"]
CMD ["R", "-q", "-e", "shiny::runApp('/opt/shiny/app')"]

