# Use a specific version of the rocker/shiny image that matches the R version in renv.lock
FROM rocker/shiny:4.4.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    pandoc \
    pandoc-citeproc

# Install renv package globally
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Copy the renv.lock file and the renv directory into the Docker image
COPY renv.lock /srv/shiny-server/aetr-app/renv.lock
COPY renv /srv/shiny-server/aetr-app/renv

# Copy the app code into the Docker image
COPY aetr-app /srv/shiny-server/aetr-app

# Set the working directory
WORKDIR /srv/shiny-server/aetr-app

# Verify the presence of renv.lock and renv directory
RUN ls -l /srv/shiny-server/aetr-app

# Restore R packages using renv
RUN R -e "renv::restore(lockfile = '/srv/shiny-server/aetr-app/renv.lock')"

# Expose the Shiny app port
EXPOSE 3838

# Start the Shiny server
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/aetr-app/app.R', host = '0.0.0.0', port = 3838)"]
