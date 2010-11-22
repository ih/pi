#!/usr/bin/python

#TO DO
#-connect to mturk code
#-write question generator for trees and tug of war

import cgi, random, glob
from questions import loadQuestion, storePreviousQuestion, finalQuestionNumber, disclaimer
#from config import previousStimuliAnswerField, previousStimuliNumberField
from mturk import *

def str2list(strRep):
    return map(int, strRep[1:-1].split(','))

def main():
    form = cgi.FieldStorage()

    (assignmentId,hitId,workerId,sandbox,exptlabel) = fillForm(form)

    dataFileName = assignmentId+hitId+workerId+sandbox+exptlabel+".txt"

    #get the current order of the stimulus
    if 'stimOrder' in form:
#        print form["stimOrder"].value
        stimOrder = str2list(form["stimOrder"].value)
        question = loadQuestion(stimOrder[0])
        currentQuestionNumber = finalQuestionNumber - len(stimOrder)+1
    else:
        stimOrder = range(finalQuestionNumber)
        random.shuffle(stimOrder)
        question = disclaimer
        currentQuestionNumber = 0


    if previousStimuliNumberField in form:
        previousStimuliNumber = int(form[previousStimuliNumberField].value)
        if len(stimOrder) < finalQuestionNumber:
            storePreviousQuestion(previousStimuliNumber, form[previousStimuliAnswerField].value, dataFileName)

    printHead()
            
    if currentQuestionNumber < finalQuestionNumber:
        print '<form name="question" method="POST" action="Test.cgi" onsubmit="return checkForm()">'
    else:
        print '<form method="POST" action="TestSubmit.cgi">'

    print str(currentQuestionNumber)+'/'+str(finalQuestionNumber)

    print question
    #the submitted question becomes the previous question answered
    print '<input type="hidden" name="'+previousStimuliNumberField+'" value=',stimOrder[0],'>'

    printHidden(assignmentId, hitId, workerId, sandbox, exptlabel)
    
    if currentQuestionNumber == 0:
        print '<input type="hidden" name="stimOrder" value="'+str(stimOrder)+ '">'
        print '<input type="submit" value="Agree">'

    else:
        print '<input type="hidden" name="stimOrder" value="'+str(stimOrder[1:])+ '">'
        print '<input type="submit" value="submit">'
    print '</form>'
    print '</body>'
main()

    
        
