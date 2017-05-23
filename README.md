# widget-dev

Develop cl-jupyter-widgets and spy on ipykernel JSON traffic

Command to use spy

docker run -v $HOME/Dev/spy-ipykernel/ipykernel:/usr/local/lib/python2.7/dist-packages/ipykernel -p 8888:8888 -p 4005:4005 -it drmeister/cando

## Command to use to install cl-jupyter-widgets

docker run -v $HOME/Dev/widget-dev/ipykernel:/usr/local/lib/python2.7/dist-packages/ipykernel  -v $HOME/Dev/widget-dev/cl-jupyter-widgets:/home/app/quicklisp/local-projects/cl-jupyter-widgets -p 8888:8888 -p 4005:4005 -it drmeister/cando
