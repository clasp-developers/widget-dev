# spy-ipykernel
Spy on ipykernel JSON traffic

Command to use spy

docker run -v $HOME/Dev/spy-ipykernel/ipykernel:/usr/local/lib/python2.7/dist-packages/ipykernel -p 8888:8888 -p 4005:4005 -it --entrypoint bash drmeister/cando
