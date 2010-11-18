import random,string,copy,os

stimuliSize = 5
maxMatches = 4

def generateStimuli():
    [os.system("mkdir "+str(i)) for i in range(stimuliSize)]
    [generateStory(str(i)) for i in range(stimuliSize)]

    

def generateStory(directory):
    numMatches = random.randint(1,maxMatches)
    contestants = ['alice', 'bob', 'eve', 'arthur', 'merlin', 'carol']
    target = random.choice(contestants)
    f = open(directory+"/story.txt",'w')
    f.write(string.join([generateMatch(contestants) for i in range(numMatches)])+"<p> How strong is "+target+"?<p>")




def generateMatch(contestants1):
    contestants = copy.deepcopy(contestants1)
    team1Size = random.randint(1,len(contestants)-1)
    team1 = random.sample(contestants, team1Size)
    [contestants.remove(x) for x in team1]
    team2 = contestants

    return "<p>team 1: "+string.join(team1)+ " beats team 2: "+string.join(team2)


    
