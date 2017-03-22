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
        sighting = {'op':'adjust', 'observation':'0d1.5', 'height':'0'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test200_030_ShouldAccept_MissingHeight(self):
        sighting = {'op':'adjust', 'observation':'60d20.5'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)
# Sad path
    def test200_910_ShouldAddError_NonIntHeight(self):
        sighting = {'op':'adjust', 'observation':'0d1.5', 'height':'a'}
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

# Sad path

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

# Sad path

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

# Sad path
