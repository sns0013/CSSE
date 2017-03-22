import unittest
import softwareprocess.dispatch as DP
import math

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
        self.assertEquals(result['temperature'], '72')
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
        self.assertEquals(result['pressure'], '1010')
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
        self.assertEquals(result['horizon'], 'natural')

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
        self.assertEquals(programDip, calculatedDip)

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
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160', 'horizon':'Natural'}
        result = DP.dispatch(sighting)
        calculatedAltitude = 0
        self.assertEquals(result['altitude'], calculatedAltitude)

#Sad Path
    def test600_910_AltitudeAlreadyPresent(self):
        sighting = {'op':'adjust', 'observation':'60d1.5', 'height':'60', 'temperature':'60', 'pressure':'160', 'horizon':'Natural', 'Altitude':'60d25.5'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)
