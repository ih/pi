#-To Do

#- make order of shuffle persistent
#- use javascript to cycle through training examples

import random,string
finalQuestionNumber = 5

#this is actually called for every page load so the shuffle gets lost each time...
def generateQuestions():
    stimuliNumbers = range(finalQuestionNumber) 
    random.shuffle(stimuliNumbers)
    return [generateQuestion(stimuli) for stimuli in stimuliNumbers]

def generateQuestion(stimuliNumber):
    
    f = open('stimuli/'+str(stimuliNumber)+'/story.txt')
    training = 'Given the following matches, please answer the following question' 

    story = f.read()
    
    answers = '<p> <input type="radio" name="previousQuestionAnswer" value="1"> strong <p> <input type="radio" name="previousQuestionAnswer" value="2"> weak <p>'

    return training+story+answers

    
