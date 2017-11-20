FROM rocker/shiny

# Add application codebase
ADD ./ /srv/shiny-server/anomaly_detection
RUN chmod a+rwx -R /srv/shiny-server/anomaly_detection

# Temporal fix for RStan package
# http://discourse.mc-stan.org/t/error-when-installing-rstan-2-16-2/1730
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux
RUN echo "\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function" >> /etc/R/Makeconf
RUN echo "\nCXXFLAGS+=-DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION" >> /etc/R/Makeconf
RUN echo "\nCPPFLAGS+=-DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION" >> /etc/R/Makeconf

# Install additional Ubuntu packages
RUN sudo apt-get update && sudo apt-get -y install libssl-dev

# Install additional R packages
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('shinyBS')"
RUN R -e "install.packages('hiny')"
RUN R -e "install.packages('hinyBS')"
RUN R -e "install.packages('hinydashboard')"
RUN R -e "install.packages('dygraphs')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('smoother')"
RUN R -e "install.packages('prophet')"
RUN R -e "install.packages('Rssa')"
RUN R -e "install.packages('tringr')"
RUN R -e "install.packages('parallel')"
