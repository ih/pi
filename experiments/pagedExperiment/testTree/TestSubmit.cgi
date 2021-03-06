#!/usr/bin/python
import cgi, random, glob
from questions import storePreviousQuestion, finalQuestionNumber
#from config import previousStimuliAnswerField, previousStimuliNumberField
from mturk import *

def main():
    form = cgi.FieldStorage()
    
    (assignmentId,hitId,workerId,sandbox,exptlabel) = fillForm(form)

    dataFileName = assignmentId+hitId+workerId+sandbox+exptlabel

    storePreviousQuestion(form[previousStimuliNumberField].value, form[previousStimuliAnswerField].value, dataFileName)

    f = open(dataFileName)
    data = f.read()
    printHead()
#    print data
    #load data file and pass contents to submit
    printHidden(assignmentId, hitId, workerId, sandbox, exptlabel)
    
    submit(data, assignmentId, form, sandbox)
    printClose()
    
main()
