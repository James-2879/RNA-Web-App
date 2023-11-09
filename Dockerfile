FROM docker-registry.organisation.com/shiny-r41-dev:latest

LABEL author="James Swift"
LABEL maintainer="James Swift"
LABEL name="RNA Web App"
LABEL version="1.0"

COPY . /srv/shiny-server/rna-web-app/
# install Python OpenSearch API module, using secrets from .env file
RUN --mount=type=secret,id=docker_env . /run/secrets/docker_env \ && pip install --extra-index-url https://${PYPI_USER}:${PYPI_PASS}@${PYPI_URL} opensearch-data-fetcher==0.7.0
RUN pip install -r /srv/shiny-server/rna-web-app/requirements.txt
# RUN Rscript /srv/shiny-server/rna-web-app/install_requirements.R
RUN sudo apt update && sudo apt install -y libharfbuzz-dev libfribidi-dev
# this is so the version of tidyverse in the R environment can be updated

EXPOSE 3838
CMD shiny-server 2>&1
RUN R -e "install.packages(c('sendmailR', 'shinybusy', 'shinyWidgets', 'V8', 'prettydoc', 'docstring', 'tidyverse', 'RMySQL'), dependencies=TRUE)"
