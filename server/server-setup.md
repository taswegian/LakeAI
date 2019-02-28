## Lake AI - Cloud instance

### Software Packages
`sudo apt-get update`

#### R
`sudo apt-get install r-base r-base-dev`

#### GDAL
`sudo apt-get install libgdal-dev libproj-dev`

#### RGDAL
`R`
`install.packages('rgdal')`

#### Docker

`sudo apt-get update`

```
sudo apt-get install \
     apt-transport-https \
     ca-certificates \
     curl \
     gnupg2 \
     software-properties-common
```

`curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -`

`sudo apt-key fingerprint 0EBFCD88`

```
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/debian \
   $(lsb_release -cs) \
   stable"
```

`sudo apt-get update`

`sudo apt-get install docker-ce`

```
mkdir -p /mnt/lake_ai_data; mount -o discard,defaults /dev/disk/by-id/scsi-0DO_Volume_lake_ai_data /mnt/lake_ai_data; echo /dev/disk/by-id/scsi-0DO_Volume_lake_ai_data /mnt/lake_ai_data ext4 defaults,nofail,discard 0 0 | sudo tee -a /etc/fstab
```

```
docker login
# docker pull aeleish/debian-r-gdal
docker pull aeleish/debian-r-jupyter
```

```
docker run -it -p 8008:8888 --name lakeAI --mount type=bind,source=/mnt/lake_ai_data,target=/data aeleish/debian-r-jupyter:latest
```

```
# docker run -it --name lakeAI --mount type=bind,source=/mnt/lake_ai_data,target=/data aeleish/debian-r-gdal:latest
```