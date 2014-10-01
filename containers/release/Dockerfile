FROM ubuntu:trusty

# install postgresql client
RUN apt-get update && \
    apt-get install -y libgmp10 libpq5=9.3.4-1

WORKDIR /data
ENTRYPOINT ["/opt/glados/glados", "Production"]
ADD glados /opt/glados/glados
