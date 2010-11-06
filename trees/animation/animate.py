import os, sys
import cPickle as pickle

if __name__ == "__main__":
    fname = pickle.load(sys.stdin)

    os.system("convert -delay 100 -loop 1 "+fname+"*.png "+fname+".gif")

