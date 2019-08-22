# Dyalog JSON Server
## Usage
### Running the container:
Run the container with the following command changing the path to your source
```sh
docker run -p 8080 -v /path/to/source/code:/code dyalog/jsonserver:latest
```
### Access the Web Interface

Once the container is running, you will be able to navigate to http://localhost:8080, you will see a web form to query the REST Server.
