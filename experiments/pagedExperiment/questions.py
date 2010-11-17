#to do
#answers should be saved with tree id info and user id info
#make sure program does not break if the user does not answer a question

from treeQuestions import generateQuestion
finalQuestionNumber = 5
disclaimer = "You are going to be a participant."

question1 ='this is question alpha'+'<input type="radio" name="previousQuestionAnswer" value="1">'+'<input type="radio" name="previousQuestionAnswer" value="2">'

question2 = '<img src="treeImages/training0.png"/> this is question beta'+'<input type="radio" name="previousQuestionAnswer" value="7">'+'<input type="radio" name="previousQuestionAnswer" value="6">'

#questions = [disclaimer]+generateQuestions()

def loadQuestion(questionNumber):
    if questionNumber == 0:
        return disclaimer
    else:
        return generateQuestion(questionNumber-1)

def storePreviousQuestion(value, questionNumber):
    f = open("answerz.txt", 'a')
    f.write(str((questionNumber,value)))


#-write template for question
