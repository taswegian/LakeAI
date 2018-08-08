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
mkdir -p /mnt/volume-nyc3-01; mount -o discard,defaults /dev/disk/by-id/scsi-0DO_Volume_volume-nyc3-01 /mnt/volume-nyc3-01; echo /dev/disk/by-id/scsi-0DO_Volume_volume-nyc3-01 /mnt/volume-nyc3-01 ext4 defaults,nofail,discard 0 0 | sudo tee -a /etc/fstab
```

```
docker login
docker pull aeleish/debian-r-gdal

```
docker run -it --name lakeAI --mount type=bind,source=/mnt/volume-nyc3-01,target=/data aeleish/debian-r-gdal:latest
```

