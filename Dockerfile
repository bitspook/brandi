from ubuntu:latest

WORKDIR /app

COPY ./build/brandi /bin/brandi

CMD brandi
