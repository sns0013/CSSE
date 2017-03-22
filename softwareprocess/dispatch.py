def dispatch(values=None):
    #Validate parm
    if(values == None):
        return {'error': 'parameter is missing'}
    if(not(isinstance(values,dict))):
        return {'error': 'parameter is not a dictionary'}
    if (not('op' in values)):
        values['error'] = 'no op  is specified'
        return values
        #Perform designated function
    if(values['op'] == 'adjust'):
        adjust(values)
        return values    #<-------------- replace this with your implementation
    elif(values['op'] == 'predict'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'correct'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'locate'):
        return values    #This calculation is stubbed out
    else:
        values['error'] = 'op is not a legal operation'
        return values

def adjust(values):
    if(not['observation' in values]):
        values['error'] = 'Observation is missing'
        return

    observation = values['observation']

    if(not('d' in observation)):
        values['error'] = 'Observation is invalid'
        return

    observationSplit = observation.split('d')
    observationX = int(observationSplit[0])
    observationYY = float(observationSplit[1])

    if(observationX < 0 or observationX > 89):
        values['error'] = 'Observation is invalid'
        return

    if(observationYY < 0.0 or observationYY > 59.9):
        values['error'] = 'Observation is invalid'
        return





