FROM ubuntu:latest

RUN apt-get update

RUN apt-get --yes install curl
RUN apt-get --yes install git
RUN apt-get --yes install zsh
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
RUN curl -sSL https://get.haskellstack.org/ | sh
