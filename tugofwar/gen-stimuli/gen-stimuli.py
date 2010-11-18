import random

stimuliSize = 5
maxMatches = 4

def generateStory():
    numMatches = random.randint(1,4)
    matches = [generateMatch() for i in numMatches]


def generateMatch():
    contestants = ['alice', 'bob', 'eve', 'arthur', 'merlin', 'carol']
    target = random.choice(contestants)
    team1Size = random.randint(1,len(contestants)-1)
    team1 = random.sample(contestants, team1Size)
    [contestants.remove(x) for x in team1]
    team2 = contestants

    return "<p>team 1: "+string.join(team1)+ "beats team 2: "+string.join(team2)+"<p> How strong is "+target+"?<p>"


    
