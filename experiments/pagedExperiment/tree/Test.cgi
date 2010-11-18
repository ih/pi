#!/usr/bin/python

#TO DO
#-recreate tree experiment
#-make answer file names unique
#-make form variables uniform across files
#-write generateQuestions?
#-print currentQuestionNumber/totalNumber
#-connect to mturk code
#-write question generator for trees and tug of war

import cgi, random, glob
from questions import loadQuestion, storePreviousQuestion, finalQuestionNumber, disclaimer

def str2list(strRep):
    return map(int, strRep[1:-1].split(','))

def main():
    form = cgi.FieldStorage()

    if 'previousQuestionNumber' in form:
        previousQuestionNumber = int(form["previousQuestionNumber"].value)
        if previousQuestionNumber > 0:
            storePreviousQuestion(form["previousQuestionAnswer"].value, previousQuestionNumber)
        currentQuestionNumber = int(previousQuestionNumber)+1
    else:
	currentQuestionNumber = 0

    if 'stimOrder' in form:
        print form["stimOrder"].value
        stimOrder = str2list(form["stimOrder"].value)
        question = loadQuestion(stimOrder[0])
    else:
        question = disclaimer
        
    print "Content-type: text/html\n"
    print '<head>'
    print '<script type="text/javascript" src="support.js"></script>'
    print '<body>'

    if currentQuestionNumber < finalQuestionNumber:
        print '<form method="POST" action="Test.cgi">'
    else:
        print '<form method="POST" action="TestSubmit.cgi">'

    print str(currentQuestionNumber)+'/'+str(finalQuestionNumber)

    print question
    #the submitted question becomes the previous question answered
    print '<input type="hidden" name="previousQuestionNumber" value=',currentQuestionNumber,'>'
    
    if currentQuestionNumber == 0:
        stimOrder = range(finalQuestionNumber)
        random.shuffle(stimOrder)
        print '<input type="hidden" name="stimOrder" value="'+str(stimOrder)+ '">'
        print '<input type="submit" value="Agree">'

    else:
        print '<input type="hidden" name="stimOrder" value="'+str(stimOrder[1:])+ '">'
        print '<input type="submit" value="submit">'
    print '</form>'
    print '</body>'
main()

    
        
