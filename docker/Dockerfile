FROM java:8 

WORKDIR /code

Add Echo.java /code
RUN javac Echo.java

EXPOSE 9999
ENTRYPOINT exec java -cp ./ Echo 
