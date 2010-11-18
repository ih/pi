import os
#from optparse import OptionParser
#usage = "usage: %prog [options] expt_dir"
#parser = OptionParser(usage)
ssh_server="irvinh@cardinal.stanford.edu:cgi-bin/"
#web_server="http://www.stanford.edu/~irvinh/cgi-bin/tree/"

# (options, args) = parser.parse_args()

# expt_dir=os.path.abspath(args[0])+'/'

os.system('scp -r tugofwar '+ssh_server)
