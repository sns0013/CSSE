import unittest
import prod.dispatch as DP
import math
import prod.StarCatalog as SC
class DispatchTest(unittest.TestCase):


# -----------------------------------------------------------------------
# ---- Acceptance Tests
# 100 Observation
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs:      angle ->    xdy.y where x .GE. 0 and .LT. 90
#                                             d is a character
#                                             y.y .GE. 0.0 and .LT. 60.0
#                                 mandatory, unvalidated
#        outputs:    none
#    Happy path analysis:
#        angle:      nominal value    x=60
#                    low bound        x=0
#                    high bound       x=89
#
#                    nominal value    y.y=45.0
#                    low bound        y.y=0.0
#                    high bound       y.y=59.9
#
#                    correct format    angle = 30d1.5
#
#    Sad path analysis:
#        angle:       unformatted angle          angle="5650.9"
#                     out-of-bounds x    x=-1; angle=90
#                     out-of-bounds y.y    x=-1.0; angle=60.0
#                     missing angle
#
# Happy path
    def test100_010_ShouldAccept_NominalValueX(self):
        sighting = {'op':'adjust', 'observation':'60d1.5'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test100_020_ShouldAccept_LowBoundX(self):
        sighting = {'op':'adjust', 'observation':'0d1.5'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test100_030_ShouldAccept_HighBoundX(self):
        sighting = {'op':'adjust', 'observation':'89d1.5'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test100_040_ShouldAccept_NominalValueX(self):
        sighting = {'op':'adjust', 'observation':'60d45.5'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test100_050_ShouldAccept_LowBoundYY(self):
        sighting = {'op':'adjust', 'observation':'60d0.0'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test100_060_ShouldAccept_HighBoundYY(self):
        sighting = {'op':'adjust', 'observation':'60d59.9'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test100_070_ShouldAccept_CorrectFormat(self):
        sighting = {'op':'adjust', 'observation':'30d1.5'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

# Sad path
    def test100_910_ShouldAddError_UnformattedAngle(self):
        sighting = {'op':'adjust', 'observation':'5650.9'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test100_920_ShouldAddError_OutOfBoundsX(self):
        sighting = {'op':'adjust', 'observation':'-1d1.5'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test100_930_ShouldAddError_OutOfBoundX(self):
        sighting = {'op':'adjust', 'observation':'90d1.5'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test100_940_ShouldAddError_NominalValueX(self):
        sighting = {'op':'adjust', 'observation':'60d-1'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test100_950_ShouldAddError_OutOfBoundYY(self):
        sighting = {'op':'adjust', 'observation':'60d60.0'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test100_960_ShouldAddError_MissingAngle(self):
        sighting = {'op':'adjust', 'observation':''}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

# 200 Height
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs:      height ->    integer where height .GE. 0
#                                 optional, unvalidated
#        outputs:    none
#    Happy path analysis:
#        height:     nominal value    height=60
#                    low bound        height=0
#                    missing
#
#    Sad path analysis:
#        height:       non-int height          angle="a"
#                     out-of-bounds height    height=-1;
#
# Happy path

    def test200_010_ShouldAccept_NominalValueHeight(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test200_020_ShouldAccept_LowBoundHeight(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'0'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test200_030_ShouldAccept_MissingHeight(self):
        sighting = {'op':'adjust', 'observation':'60d20.5'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)
# Sad path
    def test200_910_ShouldAddError_NonIntHeight(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'a'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test200_920_ShouldAddError_OutOfBoundsHeight(self):
        sighting = {'op':'adjust', 'observation':'60d20.5', 'height':'-1'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

# 300 Temperature
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs:      temperature ->    integer where temperature .GE. -20 and .LE. 120
#                                 optional, unvalidated
#        outputs:    none
#    Happy path analysis:
#        temperature:      nominal value    temperature=60
#                    low bound        temperature=-20
#                    high bound       temperature=120
#                    missing
#
#    Sad path analysis:
#        temperature:       non integer temperature          temperature="a"
#                     out-of-bounds x    temperature=-21; temperature=121
#
#
# Happy path
    def test300_010_ShouldAccept_NominalValueTemperature(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test300_020_ShouldAccept_LowBoundTemperature(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'-20'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test300_030_ShouldAccept_HighBoundTemperature(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'120'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)
        self.assertEquals(result['temperature'], '120')

    def test300_040_ShouldAccept_MissingTemperature(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)
        #self.assertEquals(result['temperature'], '72')
# Sad path
    def test300_910_ShouldAddError_InvalidValueTemperature(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'a'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test300_920_ShoulAddError_LowOutOfBoundTemperature(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'-21'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test300_930_ShouldAddError_HighOutOfBoundTemperature(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'121'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

# 400 Pressure
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs:      pressure ->    integer where pressure .GE. 100 and .LE. 1100
#                                 optional, unvalidated
#        outputs:    none
#    Happy path analysis:
#        pressure:      nominal value    pressure=160
#                    low bound        pressure=100
#                    high bound       pressure=1100
#                    missing
#
#    Sad path analysis:
#        pressure:       non int pressure          pressure="b"
#                     out-of-bounds pressure    pressure=99; pressure=1101
#
# Happy path
    def test400_010_ShouldAccept_NominalValuePressure(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test400_020_ShouldAccept_LowBoundPressure(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'100'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test400_030_ShouldAccept_HighBoundPressure(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'1100'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)
        self.assertEquals(result['pressure'], '1100')

    def test400_040_ShouldAccept_MissingPressure(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)
        #self.assertEquals(result['pressure'], '1010')
# Sad path
    def test400_910_ShouldAddError_NonIntPressure(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'b'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test400_920_ShouldAddError_LowOutOfBoundPressure(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'99'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test400_930_ShouldAddError_HighOutofBoundPressure(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'1101'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

# 500 Horizon
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs:      altitudeRelativeTo ->    case sensitive string where altitudeRelativeTo = "natural" or "artificial"
#                                 optional, unvalidated
#        outputs:    none
#    Happy path analysis:
#        altitudeRelativeTo:
#                    low bound        altitudeRelativeTo="natural"
#                    high bound       altitudeRelativeTo="artificial"
#                    missing
#
#    Sad path analysis:
#        angle:       non string angle          angle="5650.9"
#                     out-of-bounds altitudeRelativeTo    altitudeRelativeTo = "Natural" altitudeRelativeTo = "Artificial"
#
#
# Happy path
    def test500_010_ShouldAccept_LowBoundHorizon(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160', 'horizon':'natural'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)
        self.assertEquals(result['horizon'], 'natural')

    def test500_020_ShouldAccept_HighBoundHorizon(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160', 'horizon':'artificial'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test500_030_ShouldAccept_missingHorizon(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)
        #self.assertEquals(result['horizon'], 'natural')

    def test500_040_ShouldAddError_LowBoundCasedHorizon(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160', 'horizon':'Natural'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test500_050_ShouldAddError_HighBoundCasedHorizon(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160', 'horizon':'Artificial'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

# Sad path
    def test500_930_ShouldAddError_NonStringHorizon(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160', 'horizon':'123'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

# 600 Adjust
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs: observation validated
#                height validated
#                temperature validated
#                pressure validated
#                horizon validated
#        calculates:dip if horizon = natural dip = ((-.97) * sqrt(height))/60
#                          horizon = artificial dip = 0
#                   refraction
#                   altitude : rounded and needed in a certain format
#
#        outputs:    values
#    Happy path analysis:
#       correct value for all calculations
#    Sad path analysis:
#       Altitude already exists in values
# Happy path
    def test600_010_CalculateDipFromNatural(self):
        sighting = {'op':'adjust', 'observation':'45d15.2', 'height':'6', 'temperature':'71', 'pressure':'1010', 'horizon':'Natural'}
        result = DP.dispatch(sighting)
        height = result['height']
        programDip = DP.calculateDip(result['horizon'], int(height))
        calculatedDip = -0.039600084
        self.assertEquals(round(programDip, 9), calculatedDip)

    def test600_020_CalculatesDipFromArtificial(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160', 'horizon':'Artificial'}
        result = DP.dispatch(sighting)
        height = result['height']
        programDip = DP.calculateDip(result['horizon'], int(height))
        calculatedDip = 0
        self.assertEquals(programDip, calculatedDip)

    def test600_030_CalculateRefraction(self):
        sighting = {'op':'adjust', 'observation':'45d15.2', 'height':'6', 'temperature':'71', 'pressure':'1010', 'horizon':'Natural'}
        result = DP.dispatch(sighting)
        pressure = result['pressure']
        temperature = result['temperature']
        observation = result['observation']
        programRefraction = DP.calculateRefraction(pressure, temperature, observation)
        calculatedRefraction = -0.015356
        self.assertEquals(round(programRefraction, 6), calculatedRefraction)


    def test600_040_CalculateAltitude(self):
        sighting = {'op':'adjust', 'observation':'45d15.2', 'height':'6', 'temperature':'71', 'pressure':'1010', 'horizon':'Natural'}
        result = DP.dispatch(sighting)
        calculatedAltitude = '45d11.9'
        self.assertEquals(result['altitude'], calculatedAltitude)

    def test600_050_CalculateAltitude(self):
        sighting = {'op':'adjust', 'observation':'10d0.0', 'height':'6', 'temperature':'72', 'pressure':'1010', 'horizon':'Artificial'}
        result = DP.dispatch(sighting)
        calculatedAltitude = '9d54.7'
        self.assertEquals(result['altitude'], calculatedAltitude)

#Sad Path
    def test600_910_AltitudeAlreadyPresent(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160', 'horizon':'Natural', 'altitude':'60d25.5'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

# -----------------------------------------------------------------------
# ---- Acceptance Tests
# 700 Body
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs:      star ->    string where string in star list
#                                mandatory, unvalidated
#        outputs:    none if valid, error if invalid
#    Happy path analysis:
#        star:      nominal value    stars = betelgeuse
#
#    Sad path analysis:
#        star:        invalid input: sydney
#                     invalid input: ""
#                     missing star
#
# Happy path
    def test700_010_ShouldAccept_Body(self):
        sighting = {'op':'predict', 'body':'betelgeuse'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test700_020_ShouldNTAccept_MissingStar(self):
        sighting = {'op':'predict'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test700_030_ShouldNTAccept_InvalidStar(self):
        sighting = {'op':'predict', 'body':'Sydney'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test700_040_CorrectDeclination(self):
        sighting = {'op':'predict', 'body':'betelgeuse'}
        starValues = SC.getStar(sighting['body'])
        starValues = starValues.split(',')
        sideRealAngle = starValues[0]
        declination = starValues[1]
        self.assertEquals('7d24.3', declination)

    def test700_050_CorrectSideReal(self):
        sighting = {'op':'predict', 'body':'betelgeuse'}
        starValues = SC.getStar(sighting['body'])
        starValues = starValues.split(',')
        sideRealAngle = starValues[0]
        declination = starValues[1]
        self.assertEquals('270d59.1', sideRealAngle)

    def test700_060_latitudeCorrect(self):
        sighting = {'op':'predict', 'body':'betelgeuse'}
        result = DP.dispatch(sighting)
        self.assertEquals('7d24.3', result['lat'])

    def test700_070_BodyIsNumeric(self):
        sighting = {'op':'predict', 'body':'123'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

# Sad path

# -----------------------------------------------------------------------
# ---- Acceptance Tests
# 800 Date
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs:      date ->    yyyy-mm-dd where yyyy .GE. 2001
#                                             mm is an integer 01 - 12
#                                             dd is an integer 01 - 31
#                                 optional, unvalidated
#        outputs:    none
#    Happy path analysis:
#        date:      nominal value    2016-01-17
#                   missing
#
#    Sad path analysis:
#        date:       2000-01-17
#                    2001-99-17
#                    2001-01-00
#                    2001-01-32
#                    01-01-17
#
# Happy path
    def test800_010_ShouldAccept_Date(self):
        sighting = {'op':'predict', 'date':'2016-01-17', 'body':'betelgeuse'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test800_020_ShouldAccept_MissingDate(self):
        sighting = {'op':'predict', 'body':'betelgeuse'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test800_030_ShouldAccept_lowYear(self):
        sighting = {'op':'predict', 'date':'2000-01-17'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_040_ShouldAddError_invalidMonth(self):
        sighting = {'op':'predict', 'date':'2001-99-17'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_050_ShouldAddError_invalidDayLow(self):
        sighting = {'op':'predict', 'date':'2001-01-00'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_060_ShouldAddError_invalidDayHigh(self):
        sighting = {'op':'predict', 'date':'2001-01-32'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_070_ShouldAddError_invalidDateFormat(self):
        sighting = {'op':'predict', 'date':'01-01-17'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_080_NumericDateOne(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'date':'42'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_090_NumericDateTwo(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'date':'2017-09'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_100_ShouldAddError_NonNumeric1(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'date':'aa-15-42'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_110_ShouldAddError_NonNumeric2(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'date':'22-aa-42'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_120_ShouldAddError_NonNumeric3(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'date':'22-15-aa'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_130_wrongthirtyonedays(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'date':'2017-04-31'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_150_ShouldAddError_wrongfebdays(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'date':'2017-02-30'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test800_160_ShouldAddError_wrongfebdaysleap(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'date':'2017-02-29'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)
# Sad path

# -----------------------------------------------------------------------
# ---- Acceptance Tests
# 900 Time
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs:      time ->    hh:mm:ss  where hh is 0 to 24
#                                               mm is 0 to 59
#                                               mm is 0 to 59
#                                 optional, unvalidated
#        outputs:    none
#    Happy path analysis:
#        time:      nominal: 03:15:42
#                   missing
#
#
#    Sad path analysis:
#        time:       03:15:99
#                   109:15:42
#
# Happy path
    def test900_010_ShouldAccept_Time(self):
        sighting = {'op':'predict', 'time':'03:15:42', 'body':'betelgeuse'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test900_020_ShouldAccept_TimeDate(self):
        sighting = {'op':'predict', 'body':'betelgeuse'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test900_030_ShouldAccept_lowYear(self):
        sighting = {'op':'predict', 'time':'03:15:99'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test900_040_ShouldAddError_invalidMonth(self):
        sighting = {'op':'predict', 'time':'25:15:42'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test900_050_ShouldAddError_invalidDayLow(self):
        sighting = {'op':'predict', 'time':'03:67:42'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test900_060_ShouldAddError_invalidDateFormat(self):
        sighting = {'op':'predict', 'time':'109:15:42'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test900_070_NumericTimeOne(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'time':'42'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test900_080_NumericTimeTwo(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'time':'42:11'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test900_090_ShouldAddError_NonNumeric1(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'time':'aa:15:42'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test900_100_ShouldAddError_NonNumeric2(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'time':'22:aa:42'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test900_110_ShouldAddError_NonNumeric3(self):
        sighting = {'op':'predict', 'body':'betelgeuse', 'time':'22:15:aa'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)



# Sad path

# -----------------------------------------------------------------------
# ---- Acceptance Tests
# 1000 predict
#    Desired level of confidence:    boundary value analysis
#    Input-output Analysis
#        inputs:      dict ->
#        outputs:    lat long
#    Happy path analysis:
#        angle:
#
#                    nominal value    y.y=45.0
#                    low bound        y.y=0.0
#                    high bound       y.y=59.9
#
#                    correct format    angle = 30d1.5
#
#    Sad path analysis:
#        angle:       lat long is already present
#
# Happy path

# Sad path
    def test1000_010_ShouldAddError_Lat(self):
        sighting = {'op':'predict', 'time':'03:15:42', 'body':'betelgeuse', 'lat':'7d24.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test1000_020_ShouldAddError_Long(self):
        sighting = {'op':'predict', 'time':'03:15:42', 'body':'betelgeuse', 'long':'7d24.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test1000_030_ShouldAddError_LatandLong(self):
        sighting = {'op':'predict', 'time':'03:15:42', 'body':'betelgeuse', 'lat':'7d24.3', 'long':'7d24.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test1000_040_CumProg(self):
        sighting = {'op':'predict', 'body':'Betelgeuse', 'date':'2016-01-17', 'time':'03:15:42'}
        CumProg = DP.CalculateCumulativeProgress(sighting['date'])
        self.assertEquals(round(CumProg, 2), 3.58)

    def test1000_050_leapProg(self):
        sighting = {'op':'predict', 'body':'Betelgeuse', 'date':'2016-01-17', 'time':'03:15:42'}
        leapProg = DP.CalculateLeapProg('2016')
        self.assertEquals(round(leapProg, 2), 2.95)

    def test1000_100_primeMeridian(self):
        sighting = {'op':'predict', 'body':'Betelgeuse', 'date':'2016-01-17', 'time':'03:15:42'}
        cumProg = DP.CalculateCumulativeProgress(sighting['date'])
        leapProg = DP.CalculateLeapProg(sighting['date'])
        pm = DP.PMRotation(cumProg, leapProg)
        self.assertEquals(round(pm, 2) , 100.08)

    def test1000_060_observationRot(self):
        sighting = {'op':'predict', 'body':'Betelgeuse', 'date':'2016-01-17', 'time':'03:15:42'}
        observedRot = DP.observationRotation(sighting['date'], sighting['time'])
        self.assertEquals(round(observedRot, 1) , 64.8)

    def test1000_070_total(self):
        sighting = {'op':'predict', 'body':'Betelgeuse', 'date':'2016-01-17', 'time':'03:15:42'}
        cumProg = DP.CalculateCumulativeProgress('2016')
        leapProg = DP.CalculateLeapProg('2016')
        pm = DP.PMRotation(cumProg, leapProg)
        obsRot = DP.observationRotation(sighting['date'], sighting['time'])
        tot = DP.total(pm, obsRot)
        self.assertTrue(round(tot, 3), 164.908)

    def test1000_110_calculateGHA(self):
        sighting = {'op':'predict', 'body':'Betelgeuse', 'date':'2016-01-17', 'time':'03:15:42'}
        cumProg = DP.CalculateCumulativeProgress('2016')
        leapProg = DP.CalculateLeapProg('2016')
        pm = DP.PMRotation(cumProg, leapProg)
        obsRot = DP.observationRotation(sighting['date'], sighting['time'])
        tot = DP.total(pm, obsRot)
        gha = DP.calculateGHA(tot, '270d59.1')
        self.assertEquals(round(gha, 1), 435.9)

    def test1000_080_format(self):
        sighting = {'op':'predict', 'body':'Betelgeuse', 'date':'2016-01-17', 'time':'03:15:42'}
        result = DP.dispatch(sighting)
        self.assertEquals(result['long'], '75d53.6')


