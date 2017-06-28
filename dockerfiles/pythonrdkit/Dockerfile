FROM ubuntu:16.10
MAINTAINER Christian Schafmeister <meister@temple.edu>

# add LLVM repo
RUN apt-get update && apt-get -y upgrade && apt-get install -y wget curl
RUN curl http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN echo 'deb http://apt.llvm.org/yakkety/ llvm-toolchain-yakkety-4.0 main'\
  >/etc/apt/sources.list.d/llvm4.list

# Copied from the original dependencies
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  zlib1g-dev libbz2-dev libexpat-dev

# install clasp runtime dependencies
RUN apt-get -y install \
  libgc1c2 llvm-4.0-runtime libgmpxx4ldbl \
  libzmq3-dev \
  && apt-get clean

# add app user
RUN groupadd -g 9999 app && useradd -u 9999 -g 9999 -ms /bin/bash app
ENV HOME=/home/app

RUN apt-get update && apt-get -y install git-core 
USER app

# install Jupyter and its dependencies; configure jupyter kernel
USER root
RUN apt-get update && apt-get -y install git-core 
RUN apt-get -y install python2.7 python-pip python-dev ipython ipython-notebook
RUN pip install --upgrade pip && pip install jupyter
# set ownership and permissions before changing to app user
USER app

# Install nglview for python 2
USER root
RUN python -m pip install pip --upgrade
RUN python -m pip install numpy
RUN python -m pip install nglview==0.6.2.3
RUN python -m pip install ipywidgets==5.2.2
RUN python -m pip install pytraj==1.0.9
RUN python -m jupyter nbextension enable --py --sys-prefix widgetsnbextension
RUN python -m jupyter nbextension install --py --sys-prefix nglview
RUN python -m jupyter nbextension enable --py --sys-prefix nglview
# ADD dockerfiles/jupyter/kernel.json /home/app/.local/share/jupyter/kernels/lisp/kernel.json
ADD dockerfiles/jupyter/jupyter_notebook_config.py /home/app/.jupyter/jupyter_notebook_config.py

RUN chown -R app:app /home/app/.jupyter
RUN chown -R app:app /home/app/.local
USER app

WORKDIR $HOME

ENTRYPOINT [ "jupyter", "notebook" ]
CMD        [ "--no-browser", "--ip=0.0.0.0", "--port=8888" ]
