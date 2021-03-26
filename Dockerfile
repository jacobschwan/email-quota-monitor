# Base image, see https://hub.docker.com/r/rocker/r-ver
FROM rocker/r-ver:4.0.2

# Install package dependencies
RUN apt-get -y update && apt-get install -y \
   libcurl4-openssl-dev \
   libssl-dev \
   libicu-dev \
   && apt-get clean \
   && rm -rf /var/lib/apt/lists/
   
# Install further R packages
RUN install2.r --error \
# --repos http://cloud.r-project.org \
    curl \
    magrittr \
    httr \
    stringr \
    purrr \
    pushoverr \
    glue \
    scales \
    dplyr \
   && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
   
COPY check_quota.R /app/check_quota.R

CMD ["Rscript", "/app/check_quota.R"]
