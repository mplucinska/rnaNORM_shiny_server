# Base R Shiny image
FROM rocker/shiny:4.4

USER root

# Make a directory in the container
RUN mkdir /home/rnaNORM_shiny

RUN apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev

# Install R dependencies
RUN Rscript -e "install.packages(c('ggplot2','readr', 'shinyWidgets', 'shinyjs', 'future','multiprocess', 'promises', 'shinycssloaders', 'DT','shinythemes','ggthemes', 'plotly','shinyBS'), repos='http://cran.us.r-project.org/')"


RUN Rscript -e "install.packages(c('ggthemes', 'plotly'), repos='http://cran.us.r-project.org/')"

RUN Rscript -e "install.packages(c('future'), repos='http://cran.us.r-project.org/')"

RUN set -xe \
    && apt-get update -y \
    && apt-get install -y python3-pip

RUN pip install pysam typing numpy scipy

# Copy the Shiny app code
COPY rnaNORM_shiny  /home/rnaNORM_shiny

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/home/rnaNORM_shiny', host = getOption('shiny.host', '0.0.0.0'), port=8180)"]
