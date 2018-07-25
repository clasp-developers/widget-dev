#! /bin/bash
cp /home/app/startup/pprint.py /home/app/miniconda/lib/python3.6/pprint.py
jupyter lab --no-browser --allow-root --ip=0.0.0.0 --port=8888
