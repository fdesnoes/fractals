FROM ciceron/ada-servlet:latest
MAINTAINER Frederic Desnoes


#RUN apt-get update && apt-get install -y libgnat-7
WORKDIR /app/

COPY . /app/
EXPOSE 8080
CMD ["/app/bin/fractals"]

