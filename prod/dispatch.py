import math
import StarCatalog as SC
import datetime
#3

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
        correct(values)
        return values    #This calculation is stubbed out
    elif(values['op'] == 'locate'):
        return values    #This calculation is stubbed out
    else:
        values['error'] = 'op is not a legal operation'
        return values
#21

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

        height = float(values['height'])
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
        pressure = values['pressure'] #75

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
#80

def convertToCelcius(temperature):
    celcius = (int(temperature) - 32) * (5.0/9.0)
    return celcius
#3

def calculateDip(horizon, height): #6
    if horizon.lower() == 'natural':
        dip = ((-.97) * math.sqrt(long(height))) / 60
    else:
        dip = 0
    return dip

def calculateRefraction(pressure, temperature, observation): #8
    celcius = convertToCelcius(temperature)
    observationSplit = observation.split('d')
    observationX = int(observationSplit[0])
    observationYY = float(observationSplit[1])
    obsDegrees = observationX + observationYY / 60
    refraction = (-0.00452 * int(pressure)) / (273 + celcius) / math.tan(math.radians(obsDegrees))
    return refraction

def calculateAdjustedAltitude(aDip, aRefraction, observation): #7
    observationSplit = observation.split('d')
    observationX = int(observationSplit[0])
    observationYY = float(observationSplit[1])
    obsDegrees = observationX + observationYY / 60
    adjustedAltitude = obsDegrees + aRefraction + aDip
    return adjustedAltitude
#------------------------------Assignment 4
def predict(values):

    if 'lat' in values or 'long' in values:
        values['error'] = "Lat and Long cannot be in dictionary"
        return values

    sideRealAngle = 0
    if 'body' in values:

        body = values['body']
        if(body.isdigit()):
            values['error'] = 'Body is invalid'
            return values

        starValues = SC.getStar(values['body'])
        if starValues == 'error':
            values['error'] = 'Star not in Catalog'
            return values
        else:
            starValues = starValues.split(',')
            sideRealAngle = starValues[0]
            declination = starValues[1]
            values['lat'] = declination
    else:
        values['error'] = "Body is missing"
        return values

    defaultDate = "2001-01-01"
    if 'date' in values:
        date = values['date']
        if(not('-' in date)):
            values['error'] = 'Date is invalid'
            return values
        else:
            dateSplit = date.split('-') #29
            if(len(dateSplit) < 3):
                values['error'] = 'Date is invalid'
                return values

            try:
                year = int(dateSplit[0])
                month = int(dateSplit[1])
                day = int(dateSplit[2])
            except ValueError:
                values['error'] = 'Date is invalid'
                return values

            year = int(dateSplit[0])#30
            month = int(dateSplit[1])
            day = int(dateSplit[2])#32

            try:
                datetime.date(year, month, day)
            except ValueError:
                values['error'] = 'Date is invalid'
                return values

            if(day < 1 or day > 31):#33
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
            timeSplit = time.split(':')#51

            if(len(timeSplit) < 3):
                values['error'] = 'Time is invalid'
                return values

            try:
                hour = int(timeSplit[0])
                minute = int(timeSplit[1])
                second = int(timeSplit[2])
            except ValueError:
                values['error'] = 'Time is invalid'
                return values

            hour = int(timeSplit[0])#52
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

    cumProg = CalculateCumulativeProgress(date)#66
    leapProg = CalculateLeapProg(date)

    PM = PMRotation(cumProg, leapProg)
    obsRot = observationRotation(date, time)
    AriesGHA = total(PM, obsRot)
    GHA = calculateGHA(AriesGHA, sideRealAngle)
    splitGHA = str(GHA).split('.')
    minutes = ((GHA - int(splitGHA[0])) * 60)
    splitMin = str(minutes).split('.')
    dec = splitMin[1]
    minutes = splitMin[0] + "." + dec[:1]
    formatLongitude = '%d'%(int(splitGHA[0]) % 360) + 'd' + '%.1f'%(float(minutes))
    values['long'] = formatLongitude
    return values #79 - 3b



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
    GHAAries = 100.71
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

def calculateGHA(AriesGHA, sideRealAngle):
    splitSHA = sideRealAngle.split('d')
    degrees = int(splitSHA[0])
    minutes = float(splitSHA[1])

    return AriesGHA + ((minutes / 60) + degrees)

def correct(values):

    if 'correctedDistance' in values or 'correctedAzimuth' in values:
        values['error'] = 'corrected value already present'
        return values

    if 'lat' in values:
        lat = values['lat']
        if(not('d' in lat)):
            values['error'] = 'Lat is invalid'
            return values

        latSplit = lat.split('d')
        try:
            latX = int(latSplit[0])
            latYY = float(latSplit[1])
        except ValueError:
            values['error'] = 'assumedLong is invalid'
            return values

        if(latX < -89 or latX > 89):
            values['error'] = 'Lat is invalid'
            return values

        if(latYY < 0.0 or latYY > 59.9):
            values['error'] = 'Lat is invalid'
            return values
    else:
        values['error'] = 'Lat is missing'
        return values

    if 'long' in values:
        long = values['long']
        if(not('d' in long)):
            values['error'] = 'long is invalid'
            return values

        longSplit = long.split('d')
        try:
            longX = int(longSplit[0])
            longYY = float(longSplit[1])
        except ValueError:
            values['error'] = 'long is invalid'
            return values

        if(longX < 0 or longX > 359):
            values['error'] = 'long is invalid'
            return values

        if(longYY < 0.0 or longYY > 59.9):
            values['error'] = 'long is invalid'
            return values
    else:
        values['error'] = 'long is missing'
        return values


    if 'altitude' in values:
        altitude = values['altitude']
        if(not('d' in altitude)):
            values['error'] = 'altitude is invalid'
            return values

        altitudeSplit = altitude.split('d')
        try:
            altitudeX = int(altitudeSplit[0])
            altitudeYY = float(altitudeSplit[1])
        except ValueError:
            values['error'] = 'altitude is invalid'
            return values

        if(altitudeX < 1 or altitudeX > 89):
            values['error'] = 'altitude is invalid'
            return values

        if(altitudeYY < 0.0 or altitudeYY > 59.9):
            values['error'] = 'altitude is invalid'
            return values
    else:
        values['error'] = 'altitude is missing'
        return values


    if 'assumedLat' in values:
        assumedLat = values['assumedLat']
        if(not('d' in assumedLat)):
            values['error'] = 'assumedLat is invalid'
            return values

        assumedLatSplit = assumedLat.split('d')
        try:
            assumedLatX = int(assumedLatSplit[0])
            assumedLatYY = float(assumedLatSplit[1])
        except ValueError:
            values['error'] = 'assumedLat is invalid'
            return values

        if(assumedLatX < -89 or assumedLatX > 89):
            values['error'] = 'assumedLat is invalid'
            return values

        if(assumedLatYY < 0.0 or assumedLatYY > 59.9):
            values['error'] = 'assumedLat is invalid'
            return values
    else:
        values['error'] = 'assumedLat is missing'
        return values

    if 'assumedLong' in values:
        assumedLong = values['assumedLong']
        if(not('d' in assumedLong)):
            values['error'] = 'assumedLong is invalid'
            return values

        assumedLongSplit = assumedLong.split('d')
        try:
            assumedLongX = int(assumedLongSplit[0])
            assumedLongYY = float(assumedLongSplit[1])
        except ValueError:
            values['error'] = 'assumedLong is invalid'
            return values

        if(assumedLongX < 0 or assumedLongX > 359):
            values['error'] = 'assumedLong is invalid'
            return values

        if(assumedLongYY < 0.0 or assumedLongYY > 59.9):
            values['error'] = 'assumedLong is invalid'
            return values
    else:
        values['error'] = 'assumedLong is missing'
        return values

    lha = calculatedLHA(values)
    correctedAlt = calculateCorrectAlt(values, lha)

    calculateCorrectedDistance(values, correctedAlt)

    return values

def calculatedLHA(values):
    long = values['long']
    assumedLong = values['assumedLong']

    longSplit = long.split('d')
    assumedLongSplit = assumedLong.split('d')

    degrees = int(longSplit[0]) + int(assumedLongSplit[0])
    minutes = (float(longSplit[1]) + float(assumedLongSplit[1]))/60

    degrees = degrees + minutes
    degreesSplit = str(degrees).split('.')
    lha = str(int(degreesSplit[0]) % 360) + "d" + str(round((degrees - float(degreesSplit[0])) * 60, 1))

    return lha

def calculateCorrectAlt(values, lha):
    assumedLat = values['assumedLat']
    lat = values['lat']

    lhaSplit = lha.split('d')
    lhaX = int(lhaSplit[0])
    lhaYY = float(lhaSplit[1]) / 60

    lhaValue = lhaX + lhaYY

    assumedLatSplit = assumedLat.split('d')
    assumedLatX = int(assumedLatSplit[0])
    assumedLatYY = float(assumedLatSplit[1]) / 60

    if(assumedLatX < 0):
        assumedLatValue = assumedLatX - assumedLatYY
    else:
        assumedLatValue = assumedLatX + assumedLatYY

    latSplit = lat.split('d')
    latX = int(latSplit[0])
    latYY = float(latSplit[1]) / 60

    if(latX < 0):
        latValue = latX - latYY
    else:
        latValue = latX + latYY


    assumedLatRadians = math.radians(assumedLatValue)
    latRadians = math.radians(latValue)
    lhaRadians = math.radians(lhaValue)

    intermediate = (math.sin(latRadians) * math.sin(assumedLatRadians)) + (math.cos(latRadians) * math.cos(assumedLatRadians) * math.cos(lhaRadians))


    correctAlt = math.asin(intermediate)
    correctAlt = math.degrees(correctAlt)

    correctAltSplit = str(correctAlt).split('.')
    degrees = int(correctAltSplit[0])

    correctAltFormat = str(degrees % 360) + "d" + str(round((correctAlt - degrees) * 60, 1))

    return correctAltFormat

def calculateCorrectedDistance(values, correctedAlt):
    altitude = values['altitude']
    altitudeSplit = altitude.split('d')
    altitudeX= int(altitudeSplit[0])
    altitudeYY= float(altitudeSplit[1]) / 60

    altValue = altitudeX + altitudeYY

    correctedAltSplit = correctedAlt.split('d')
    correctedAltX= int(correctedAltSplit[0])
    correctedAltYY= float(correctedAltSplit[1]) / 60

    correctedAltValue = correctedAltX + correctedAltYY

    correctedDist = round((altValue - correctedAltValue) * 60, 0)

    values['correctedDistance'] = str(int(correctedDist))
    return correctedDist







