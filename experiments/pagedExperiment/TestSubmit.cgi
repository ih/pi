#!/usr/bin/python
import cgi, random, glob
from questions import storePreviousQuestion, finalQuestionNumber


def main():
    form = cgi.FieldStorage()

    storePreviousQuestion(form["previousQuestionAnswer"].value, finalQuestionNumber)

    print "Content-type: text/html\n"
    print "you are done"
    
main()
