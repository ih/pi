#!/usr/bin/python

#TO DO
#move aws_clt_dir, ssh_server, web_server to an external config file
#fix collect to work when using sandbox (need to add .text to the expt label
import os
from optparse import OptionParser

#setup and run an experiment.


#parse command line options:
usage = "usage: %prog [options] expt_dir"
parser = OptionParser(usage)
parser.add_option("-m","--mode", dest="run_mode", default="run",
                  help='Run mode: "run" to run the experiment, "collect" to get results from server.')
parser.add_option("--sandbox", dest="sandbox", action="store_true", default=True,
                  help="Use MTurk sandbox (for testing).")
parser.add_option("--live", dest="sandbox", action="store_false",
                  help="Use MTurk production site (for actually running the experiment).")
parser.add_option("--dryrun", dest="dryrun",  action="store_true", default=False,
                  help="Use this option to do everything except submiting the HIT to MTurk.")
parser.add_option("--label", dest="expt_label",
                  help="The label for the experiment (default is last directory name in experiment path).")

(options, args) = parser.parse_args()

if len(args)!=1:
    parser.error("incorrect number of arguments")
else:
    expt_dir=os.path.abspath(args[0])+'/'
    if options.expt_label==None:
        options.expt_label=expt_dir.rsplit('/',2)[1]

#these should be turned into options (with defaults):
num_Ss='15'
pay_per_S='0.2'
title='a test'
description='css test'
keywords='css test'

aws_clt_dir="/home/ih/Downloads/aws-mturk-clt-1.3.0/bin"
ssh_server="irvinh@cardinal.stanford.edu:cgi-bin/"
web_server="http://www.stanford.edu/~irvinh/cgi-bin/testTree/"



if options.run_mode=="run":
    #copy the cgi scripts and scenarios to the server (server should already have an 'exptData' directory that www can write to):
    print "Copying experiment files to remote server:"
    os.system('scp -r '+expt_dir+' '+ssh_server)

    #now make the mturk control files (.input, .properties, .question):
    print "Building MTurk control files:"
    f=open(expt_dir+'Expt_hit.input', 'w')
    print >>f, 'dummy\n1'
    f.close()

    f=open(expt_dir+'Expt_hit.properties', 'w')
    print >>f, "title:"+title
    print >>f, "description:"+description
    print >>f, "keywords:"""+keywords
    print >>f, "reward:"+pay_per_S
    print >>f, "assignments:"+num_Ss
    print >>f, "annotation:"+options.expt_label
    # this Assignment Duration value is 30 * 60 = 0.5 hour
    print >>f, "assignmentduration:1800"
    # this HIT Lifetime value is 60*60*24*7 = 7 days
    print >>f, "hitlifetime:604800"
    # this Auto Approval period is 60*60*24*15 = 15 days
    print >>f, "autoapprovaldelay:172800"
    f.close()

    f=open(expt_dir+'Expt_hit.question', 'w')
    print >>f, '<?xml version="1.0"?>\n<ExternalQuestion xmlns="http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2006-07-14/ExternalQuestion.xsd">'
    if options.sandbox:
        print >>f, '<ExternalURL>'+web_server+'Test.cgi?exptlabel="'+options.expt_label+'"&amp;sandbox="true"</ExternalURL>'
    else:
        print >>f, '<ExternalURL>'+web_server+'TestSubmit.cgi?exptlabel="'+options.expt_label+'"&amp;sandbox="false"</ExternalURL>'
    print >>f, '<FrameHeight>450</FrameHeight>\n</ExternalQuestion>'
    f.close()

    #now use aws clt loadHit to sumit the hit to MTurk:
    #for some reason running loadHits.sh from os.system doesn't work. here's a workaround:
    #make script to:
    # change to aws command line tools directory,
    # submit the HIT,
    # move the .success file to the expt directory (it has hitId in it),
    # switch back to the experiment directory:
    f=open('run.sh', 'w')
    cur_path=os.getcwd()
#    print >>f, "export JAVA_HOME=/Library/Java/Home"
    print >>f, "cd "+aws_clt_dir
    if options.sandbox:
        print >>f, './loadHITs.sh -sandbox -label '+options.expt_label+' -input '+expt_dir+'Expt_hit.input -question '+expt_dir+'Expt_hit.question -properties '+expt_dir+'Expt_hit.properties'
    else:
        print >>f, './loadHITs.sh -label '+options.expt_label+' -input '+expt_dir+'Expt_hit.input -question '+expt_dir+'Expt_hit.question -properties '+expt_dir+'Expt_hit.properties'
    print >>f, 'mv '+options.expt_label+'.success '+expt_dir
    print >>f, 'cd '+cur_path
    f.close()
    if options.dryrun!=True:
        os.system('bash run.sh')
    os.system('rm run.sh')

elif options.run_mode=="collect":
    #copy the experiment results to the exptData directory (need to make directory first?):
    os.system('mkdir '+expt_dir+'exptData/')
    print 'scp '+ssh_server+'exptData/*.'+options.expt_label+' '+expt_dir+'exptData/'
    os.system('scp '+ssh_server+'exptData/*.'+options.expt_label+' '+expt_dir+'exptData/')






