import math

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


    if 'observation' in values:
        observation = values['observation']
        if(not('d' in observation)):
            values['error'] = 'Observation is invalid'
            return values

        observationSplit = observation.split('d')
        observationX = int(observationSplit[0])
        observationYY = float(observationSplit[1])

        if(observationX < 0 or observationX > 89):
            values['error'] = 'Observation is invalid'
            return values

        if(observationYY < 0.0 or observationYY > 59.9):
            values['error'] = 'Observation is invalid'
            return values
    else:
        values['error'] = 'Observation is missing'
        return values

    defaultHeight = '0'
    if 'height' in values:
        height = values['height']
        try:
            height = values['height']
            long(height)
        except ValueError:
            values['error'] = 'Height is invalid'
            return values

        height = int(values['height'])
        if(height < 0):
            values['error'] = 'Height is invalid'
            return values

    else:
        height = defaultHeight

    defaultTemperature = '72'
    if 'temperature' in values:
        temperature = values['temperature']
        try:
            temperature = values['temperature']
            int(temperature)
        except ValueError:
            values['error'] = 'Temperature is invalid'
            return values

        temperature = int(values['temperature'])
        if(temperature < -20 or temperature > 120):
            values['error'] = 'Temperature is invalid'
            return values
    else:
        temperature = defaultTemperature

    defaultPressure = '1010'
    if 'pressure' in values:
    pressure = values['pressure']

        try:
            pressure = values['pressure']
            int(pressure)
        except ValueError:
            values['error'] = 'Pressure is invalid'
            return values

        pressure = int(values['pressure'])
        if(pressure < 100 or pressure > 1100):
            values['error'] = 'Pressure is invalid'
            return values
    else:
        pressure = defaultPressure

    defaultHorizon = 'natural'
    if 'horizon' in values:
        horizon = values['horizon']
        if(not(horizon.lower() == 'natural' or horizon.lower() == 'artificial')):
            values['error'] = 'Horizon is invalid'
            return values
    else:
        horizon = defaultHorizon

    if 'altitude' in values:
        values['error'] = 'Altitude is already present'
        return values


    aDip = calculateDip(horizon, height)

    aRefraction = calculateRefraction(pressure, temperature, observation)
    altitude = calculateAdjustedAltitude(aDip, aRefraction, observation)
    splitAltitude = str(altitude).split('.')
    formatedAltitude = '%d'%(int(splitAltitude[0])) + 'd' + '%.1f'%((altitude - int(splitAltitude[0])) * 60)
    values['altitude'] = formatedAltitude

def convertToCelcius(temperature):
    celcius = (int(temperature) - 32) * (5.0/9.0)
    return celcius

def calculateDip(horizon, height):
    if horizon.lower() == 'natural':
        dip = ((-.97) * math.sqrt(long(height))) / 60
    else:
        dip = 0
    return dip

def calculateRefraction(pressure, temperature, observation):
    celcius = convertToCelcius(temperature)
    observationSplit = observation.split('d')
    observationX = int(observationSplit[0])
    observationYY = float(observationSplit[1])
    obsDegrees = observationX + observationYY / 60
    refraction = (-0.00452 * int(pressure)) / (273 + celcius) / math.tan(math.radians(obsDegrees))
    return refraction

def calculateAdjustedAltitude(aDip, aRefraction, observation):
    observationSplit = observation.split('d')
    observationX = int(observationSplit[0])
    observationYY = float(observationSplit[1])
    obsDegrees = observationX + observationYY / 60
    adjustedAltitude = obsDegrees + aRefraction + aDip
    return adjustedAltitude
