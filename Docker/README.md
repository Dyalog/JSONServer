# Dyalog JSON Server in a Docker container
## Usage
### Running the container:
#### From DockerHub
Run the container with the following command, changing the path to your source. The dyalog/jsonserver docker image will be pulled from DockerHub automatically if necessary.
```sh
docker run -it --rm -p 8080:8080 -v /absolute/path/to/APLProjectFolder/:/app dyalog/jsonserver
```  
Removing ```-v /absolute/path/to/APLProjectFolder/:/app``` from the above will run the [sample code](https://github.com/Dyalog/JSONServer/tree/master/Sample)  
  
_Further explanation_ of the ```docker run``` syntax and options can be found [here](https://docs.docker.com/engine/reference/commandline/run/#description).
#### Build from here
The docker image can be built from the DockerFile found in this directory. Example:  
```sh
docker build -t dyalog-jsonserver .
```  
This will still pull the dyalog/dyalog:17.1-dbg container from DockerHub. However, the *DockerFile* and *run* scripts from here are used, so you can modify them if you so wish.  
The container can be run using the same script as above, replacing ```dyalog/jsonserver``` with ```dyalog-jsonserver```.
### Access the Web Interface

Once the container is running, you will be able to navigate to http://localhost:8080, you will see a web form to query the REST Server.

