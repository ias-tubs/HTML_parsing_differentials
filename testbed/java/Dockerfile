FROM openjdk:11
ADD https://github.com/ufoscout/docker-compose-wait/releases/download/2.9.0/wait /wait
RUN chmod +x /wait


WORKDIR /work

COPY . /work

RUN bash ./gradlew clean jar
#ENTRYPOINT ["/usr/local/openjdk-11/bin/java"]
CMD /wait && /work/app/build/libs/app.jar
