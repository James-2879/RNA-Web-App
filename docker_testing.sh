echo "Check all necessary R packages are added to Dockerfile."
echo ""
echo "> Stopping container if running"
docker container stop rna-web-app-container
echo ""
echo "> Removing container if present"
docker container rm rna-web-app-container
echo ""
echo "> Removing image if present"
docker image rm rna-web-app
echo ""
echo "> Building image"
DOCKER_BUILDKIT=1 docker build -t rna-web-app --secret id=docker_env,src=.env .
echo ""
echo "> Launching container"
docker run -d --name rna-web-app-container rnaseq-portal
echo ""
echo "> Launching shiny-server webpage"
google-chrome http://172.17.0.2:3838/rna-web-app/
# docker exec -it rna-web-app-container /bin/bash
