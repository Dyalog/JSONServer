# Docker Folder


This folder contains files that will create a Docker image containing :



- Ubuntu (using a standard image from Docker)
- DyalogAPL (downloaded from my.dyalog.com - requires credentials) 
- JSONServer (from https://github.com/Dyalog/JSONServer)
 
When launched, the image will start the JSONServer sample application which is included in the JSONServer repository.




Files contained in this folder (in the order you might want to use them):

|File     |Description   |
|-----|---------|
| README.MD | This file (well done, you are reading it already!) |
| Dockerfile   | Defines the docker image |
| entry.sh | Script which runs when the image starts |
| build | Build the Docker image |
| run | Run the Docker image |
| cleanup | Stop and remove all traces of the image |

