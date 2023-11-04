FROM ubuntu:20.04

RUN apt-get update
RUN apt-get install -y gcc make libbz2-dev zlib1g-dev libncurses5-dev libncursesw5-dev liblzma-dev 
RUN apt-get install -y bzip2

#python3 installation
RUN apt-get install build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev -y
COPY ./Python-3.11.2.tar.xz /tmp/Python-3.11.2.tar.xz
RUN tar -xf /tmp/Python-3.11.2.tar.xz

WORKDIR /Python-3.11.2
RUN ./configure --enable-optimizations
RUN make install

RUN pip3 install pyDataverse
WORKDIR /

COPY dataverse_controller.py /
COPY main.py /
COPY json_parser.py /
COPY constant.py /
COPY results /results 
COPY metadata.json / 
