#to do
#answers should be saved with tree id info and user id info
#make sure program does not break if the user does not answer a question

from treeQuestions import generateQuestion, finalQuestionNumber
#finalQuestionNumber = 5

disclaimer = "You are going to be a participant."


def loadQuestion(questionNumber):
    return generateQuestion(questionNumber)


def storePreviousQuestion(stimuli, answer, fname):
    f = open(fname, 'a')
    f.write(str((stimuli,answer)))


#-write template for question
