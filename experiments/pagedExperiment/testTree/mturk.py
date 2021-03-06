#common functions and variables across files

previousStimuliAnswerField = "previousStimuliAnswerField"
previousStimuliNumberField = "previousStimuliNumberField"


def printHead():
    print "Content-type: text/html\n"
    print '<head>'
    print '<script type="text/javascript" src="support.js"></script>'
    print '<link rel="stylesheet" href="categorization.css" type="text/css">'
    print '</head>'
    print '<body>'



def fillForm(form):
    if 'assignmentId' in form:
        assignmentId = form["assignmentId"].value
    else:
        assignmentId = '123'

    if 'hitId' in form:
        hitId = form["hitId"].value
    else:
        hitId = 'hit3'

    if 'workerId' in form:
        workerId = form["workerId"].value
    else:
        workerId = 'worker9'

    if 'sandbox' in form:
        sandbox = form["sandbox"].value
    else:
        sandbox = 'nosand'

    if 'exptlabel' in form:
        exptlabel = form["exptlabel"].value
    else:
        exptlabel = 'expt5'
    return (assignmentId,hitId,workerId,sandbox,exptlabel)

def printHidden(assignmentId, hitId, workerId, sandbox, exptlabel):
    #hidden stuff to be saved with data:
    print '<input type="hidden" name="assignmentId" value=', assignmentId, '>'
    print '<input type="hidden" name="hitId" value=', hitId, '>'
    print '<input type="hidden" name="workerId" value=', workerId, '>'
    print '<input type="hidden" name="sandbox" value=', sandbox, '>'
    print '<input type="hidden" name="exptlabel" value=', exptlabel, '>'


def submit(data, assignmentId, form, sandbox):
    #redirect on to mturk:

    print "<h2>Your data has been saved, thank you for participating!</h2><br>\n"
    print "<h2>If you are not automatically redirected, please click here:</h2><br>\n"

    if sandbox!="false":
        print '<form name="submitterForm" method="POST" action="http://workersandbox.mturk.com/mturk/externalSubmit">' #this is the sandbox url
    else:
        print '<form name="submitterForm" method="POST" action="http://www.mturk.com/mturk/externalSubmit">' #this is the live url

    print '<input type="hidden" id="assignmentId" name="assignmentId" value="' + assignmentId + '">'
    print '<input type="hidden" name="data" value="'+data+'">'
    print '<input type="hidden" name="raw_data" value="', str(form), '">'
    print '<input type="submit" value="Return to MTurk.">'
    print '</form>'

    #This should automatically redirect, instead of requiring Ss to press another button:
    print """<script type="text/javascript" language="JavaScript"><!-- 
    document.submitterForm.submit(); 
    //--></script>"""

def printClose():
    print '</body>'
    print '</html>'
