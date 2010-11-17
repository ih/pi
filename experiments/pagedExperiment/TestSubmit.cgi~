import cgi, random, glob
from questions import storeLastQuestion, lastQuestionNumber

def main():
    form = cgi.FieldStorage()

    storeLastQuestion(form["lastQuestionAnswer"].value, lastQuestionNumber)
print "Content-type: text/html\n"		
print "ALL DONE!"
