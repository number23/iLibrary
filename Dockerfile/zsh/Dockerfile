# VERSION 1 - EDITION 1
# Author: number23
FROM ubuntu
MAINTAINER number23 number23.cn@email.com

COPY ./.zshrc /root/.zshrc
COPY ./.aliasrc /root/.aliasrc

RUN apt-get update -y \
    && apt-get install -y zsh most \
    && apt-get clean \
    && chsh -s /usr/bin/zsh

ENV SHELL /usr/bin/zsh

WORKDIR /root
ENTRYPOINT /usr/bin/zsh
