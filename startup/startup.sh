#! /bin/bash
cp /home/app/startup/pprint.py /opt/clasp/lib/python3.6/pprint.py
if [ -e /home/app/save-cache/saved-cache.tar ]
then
    rm -rf /opt/clasp/lib/cache/common-lisp/*
    echo "Restoring cache"
    tar xf /home/app/save-cache/saved-cache.tar
fi
/opt/clasp/bin/cando-jupyter-lab.sh
