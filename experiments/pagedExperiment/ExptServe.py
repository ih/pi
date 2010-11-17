#!/usr/bin/python

#TO DO
#-write loadQuestion
#-write storeLastQuestion
#-print currentQuestionNumber/totalNumber
import cgi, random, glob

def loadQuestion(questionNumber):
    if questionNumber == 1:
        question = "this is question "+str(questionNumber)
        answer1 = '<input type="radio" name="lastQuestionAnswer" value="1">'
        answer2 = '<input type="radio" name="lastQuestionAnswer" value="2">'
    if questionNumber == 2:
        question = "this is question "+str(questionNumber)
        answer1 = '<input type="radio" name="lastQuestionAnswer" value="3">'
        answer2 = '<input type="radio" name="lastQuestionAnswer" value="4">'
        

def storeLastQuestion(value, questionNumber):
    f = open("answers.txt", 'a')
    f.write(str((questionNumber,value)))


def main():
    form = cgi.FieldStorage()

    if 'lastQuestionNumber' in form:
        lastQuestionNumber = form["lastQuestionNumber"].value
        storeLastQuestion(form["lastQuestionAnswer"].value, lastQuestionNumber)
        currentQuestionNumber = lastQuestionNumber+1
    else:
	currentQuestionNumber = 1

    question = loadQuestion(currentQuestionNumber)

    print "Content-type: text/html\n"

    #this will eventually be dependent on the question number
    print '<form method="POST" action="ExptServe.cgi">'

    #eventually print currentQuestionNumber/totalNumber
    print currentQuestionNumber

    print question
    #the submitted question becomes the last question answered
    print '<input type="hidden" name="lastQuestionNumber" value=', currentQuestionNumber, '>'
    print '<input type="submit" value="Submit HIT">'
    print '</form>'
    
main()

    
        
