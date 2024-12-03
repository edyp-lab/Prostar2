set -ex
# SET THE FOLLOWING VARIABLES
# Version number
VERSION=2.0

# image name
IMAGE=ghcr.io/edyp-lab/prostar2

# run build with the Docker file
docker build -t $IMAGE:$VERSION .
#docker build -t $IMAGE:$VERSION .

# tag it
docker tag $IMAGE:$VERSION $IMAGE:latest
#docker tag $IMAGE:$VERSION $IMAGE


docker login ghcr.io -u samWieczorek --password-stdin < ~/my_Docker_password.txt

# push it
docker push $IMAGE:$VERSION
docker push $IMAGE:latest