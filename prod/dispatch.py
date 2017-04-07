import math
import StarCatalog as SC
import datetime


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
        return values
    elif(values['op'] == 'predict'):
        predict(values)
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
            float(height)
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
#------------------------------Assignment 4
def predict(values):

    if 'latitude' in values or 'longitude' in values:
        values['error'] = "Lat and Long cannot be in dictionary"
        return values

    if 'body' in values:
        starValues = SC.getStar(values['body'])
        if starValues == 'error':
            values['error'] = 'Star not in Catalog'
            return values
        else:
            starValues = starValues.split(',')
            sideRealAngle = starValues[0]
            declination = starValues[1]
            values['latitude'] = declination
    else:
        values['error'] = "Body is missing"

    defaultDate = "2001-01-01"
    if 'date' in values:
        date = values['date']
        if(not('-' in date)):
            values['error'] = 'Date is invalid'
            return values
        else:
            dateSplit = date.split('-')
            year = int(dateSplit[0])
            month = int(dateSplit[1])
            day = int(dateSplit[2])

            if(day < 1 or day > 31):
                values['error'] = 'Date is invalid'
                return values

            if(month < 1 or month > 12):
                values['error'] = 'Date is invalid'
                return values

            if(year <= 2001):
                values['error'] = 'Date is invalid'
                return values
    else:
        date = defaultDate

    defaultTime = "00:00:00"
    if 'time' in values:
        time = values['time']
        if(not(':' in time)):
            values['error'] = 'Time is invalid'
            return values
        else:
            timeSplit = time.split(':')
            hour = int(timeSplit[0])
            minute = int(timeSplit[1])
            second = int(timeSplit[2])

            if(hour < 0 or hour > 24):
                values['error'] = 'Time is invalid'
                return values

            if(minute < 0 or minute > 59):
                values['error'] = 'Time is invalid'
                return values

            if(second < 0 or second > 59):
                values['error'] = 'Time is invalid'
                return values
    else:
        time = defaultTime

    #cumProg = CalculateCumProg(year)
    #leapProg = CalculateLeapProg(year)
    #PM = PMRotation(cumProg, leapProg)
    #obsRot = observationRotation(date, time)
    #AriesGHA = total(PM, obsRot)
    #calculateGHA(AriesGHA, sideRealAngle)



def CalculateCumulativeProgress(date):
    decrease = 14.31667 / 60
    referenceYear = 2001
    dateSplit = date.split('-')
    year = int(dateSplit[0])
    difference = year - referenceYear
    progress = difference * decrease
    return progress

def CalculateLeapProg(date):
    totalProgression = 59.0 / 60
    referenceYear = 2001
    dateSplit = date.split('-')
    year = int(dateSplit[0])
    difference = year - referenceYear
    leapYear = int(difference/4)
    leapProg = leapYear * totalProgression
    return leapProg

def PMRotation(cumProg, leapProg):
    GHAAries = 6042.6
    pmRot = GHAAries - cumProg + leapProg
    return pmRot

def observationRotation(date, time):
    timeSplit = time.split(':')
    hour = int(timeSplit[0])
    minute = int(timeSplit[1])
    second = int(timeSplit[2])

    dateSplit = date.split('-')
    year = int(dateSplit[0])
    month = int(dateSplit[1])
    day = int(dateSplit[2])

    EarthRotational = 86164.1
    aries = datetime.datetime(year, 01, 01, 00, 00, 00)
    star = datetime.datetime(year, month, day, hour, minute, second)

    dateDiff = (star - aries)
    dateDiff = dateDiff.total_seconds()
    fractRot = dateDiff / EarthRotational
    splitFractRot = str(fractRot).split('.')


    return  (fractRot - int(splitFractRot[0])) * 360

def total(PM, obsRot):
    return PM + obsRot

def calculateGHA(AriesGHA, SHA):
    splitSHA = SHA.split('d')
    degrees = int(splitSHA[0])
    minutes = float(splitSHA[1])

    return AriesGHA + (((minutes / 60) + degrees) *60)





