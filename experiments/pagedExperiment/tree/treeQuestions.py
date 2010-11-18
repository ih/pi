#-To Do

#- make order of shuffle persistent
#- use javascript to cycle through training examples

import random,string
finalQuestionNumber = 5
trainingNumber = 3

#this is actually called for every page load so the shuffle gets lost each time...
def generateQuestions():
    stimuliNumbers = range(finalQuestionNumber) 
    random.shuffle(stimuliNumbers)
    return [generateQuestion(stimuli) for stimuli in stimuliNumbers]

def generateQuestion(stimuliNumber):
#    link="""<a href="#" onclick="change_example('%d');">Click here to toggle visibility of element #foo</a>""" % tree['id']

    training = 'Here you see examples of plant %d, a newly discovered kind of plant that grows in extreme envioronments.:<p>' % stimuliNumber

    trainingImages = makeTrainingImageLinks(stimuliNumber)
    
    testQuestion = '<p>How likely is it that the following plant is the same kind of plant as the plants above.?<p>'

    testImage = '<img src="stimuli/%d/testing.gif">' % stimuliNumber

    answers = '<p> <input type="radio" name="previousQuestionAnswer" value="1"> certainly the same kind <p> <input type="radio" name="previousQuestionAnswer" value="2"> certainly not the same kind <p>'

    return training+trainingImages+testQuestion+testImage+answers

def makeTrainingImageLinks(stimuliNumber):
    return string.join(['<img src="stimuli/%d/training%d.gif">' % (stimuliNumber,i) for i in range(trainingNumber)])
    
