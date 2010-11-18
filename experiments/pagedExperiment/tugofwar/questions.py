#to do
#answers should be saved with tree id info and user id info
#make sure program does not break if the user does not answer a question

from tugofwar import generateQuestion, finalQuestionNumber
#finalQuestionNumber = 5

disclaimer = "You are going to be a participant."


def loadQuestion(questionNumber):
    return generateQuestion(questionNumber)

def storePreviousQuestion(value, questionNumber):
    f = open("answerz.txt", 'a')
    f.write(str((questionNumber,value)))


#-write template for question
