import unittest
import prod.dispatch as DP
import math
import prod.StarCatalog as SC

class CorrectedTest(unittest.TestCase):
    def test001_001_lat_LBX(self):
        sighting = {'op':'correct', 'lat':'-89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_002_lat_HBX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_003_lat_NX(self):
        sighting = {'op':'correct', 'lat':'65d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_004_lat_EX(self):
        sighting = {'op':'correct', 'lat':'100d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_005_lat_LBY(self):
        sighting = {'op':'correct', 'lat':'89d0.0', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_006_lat_HBY(self):
        sighting = {'op':'correct', 'lat':'89d59.9', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_007_lat_NY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_008_lat_EY(self):
        sighting = {'op':'correct', 'lat':'89d61.0', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_009_lat_Missing(self):
        sighting = {'op':'correct', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_010_lat_IncorrectFormat(self):
        sighting = {'op':'correct', 'lat':'8920.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)



    def test001_000_long_LBX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'0d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_long_HBX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'359d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_long_NX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_long_EX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'976d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_long_LBY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d0.0', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_long_HBY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d59.9', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_long_NY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_long_EY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d70.5', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_long_Missing(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_long_IncorrectFormat(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'1545.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)



    def test001_000_alt_LBX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'1d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_alt_HBX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'89d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_alt_NX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_alt_EX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'91d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_alt_LBY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d0.0', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_alt_HBY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d59.9', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_alt_NY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_alt_EY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d61.0', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_alt_Missing(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_alt_IncorrectFormat(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'3717.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)


    def test001_000_assLat_LBX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'-89d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLat_HBX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'89d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLat_NX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLat_EX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'105d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_assLat_LBY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d0.0', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLat_HBY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.9', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLat_NY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d40.1', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLat_EY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d356', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_assLat_Missing(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_assLat_IncorrectFormat(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'3559.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)


    def test001_000_assLong_LBX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'0d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLong_HBX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'359d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLong_NX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLong_EX(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'789d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_assLong_LBY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d0.0'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLong_HBY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d59.9'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLong_NY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        result = DP.dispatch(sighting)
        self.assertTrue(not 'error' in result)

    def test001_000_assLong_EY(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d61.0'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_assLong_Missing(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)

    def test001_000_assLong_IncorrectFormat(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'7435.3'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)


    def test001_000_correctedDistance_Present(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3', 'correctedDistance':'104'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)


    def test001_000_correctedAzi_Present(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3', 'correctedAzimuth':'od36.8'}
        result = DP.dispatch(sighting)
        self.assertTrue('error' in result)



    def test001_000_correctedDistance_Correct(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        results = DP.dispatch(sighting)
        self.assertEquals(results['correctedDistance'], '104')


    def test001_000_correctedAzi_Correct(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        results = DP.dispatch(sighting)
        self.assertEquals(results['correctedAzimuth'], '0d36.8')


    def test001_000_calculatedLHA_Correct(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        calculatedLHA = DP.calculatedLHA(sighting)
        self.assertEquals(calculatedLHA, '228d40.7')

    def test001_000_calculateCorrectAlt_Correct(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        calculatedLHA = DP.calculatedLHA(sighting)
        intermediate = DP.calcInter(sighting, calculatedLHA)
        correctAlt = DP.calculateCorrectAlt(intermediate)
        self.assertEquals(correctAlt, '35d33.3')

    def test001_000_calculateCorrectDist_Correct(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        calculatedLHA = DP.calculatedLHA(sighting)
        intermediate = DP.calcInter(sighting, calculatedLHA)
        correctAlt = DP.calculateCorrectAlt(intermediate)
        correctedDistance = DP.calculateCorrectedDistance(sighting, correctAlt)
        self.assertEquals(correctedDistance, 104)

    def test001_000_calculateCorrectAzi_Correct(self):
        sighting = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4', 'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        calculatedLHA = DP.calculatedLHA(sighting)
        intermediate = DP.calcInter(sighting, calculatedLHA)
        correctAlt = DP.calculateCorrectAlt(intermediate)
        correctedDistance = DP.calculateCorrectedDistance(sighting, correctAlt)
        correctedAzi = DP.calculateCorrectedAzi(sighting, intermediate)
        self.assertEquals(correctedAzi, '0d36.8')

    def test001_002_calculatedLHA_Correct(self):
        sighting = {'op':'correct', 'lat':'16d32.3', 'long':'95d41.6', 'altitude':'13d42.3', 'assumedLat':'-53d38.4', 'assumedLong':'74d35.3'}
        calculatedLHA = DP.calculatedLHA(sighting)
        self.assertEquals(calculatedLHA, '170d16.9')

    def test001_002_calculateCorrectAlt_Correct(self):
        sighting = {'op':'correct', 'lat':'16d32.3', 'long':'95d41.6', 'altitude':'13d42.3', 'assumedLat':'-53d38.4', 'assumedLong':'74d35.3'}
        calculatedLHA = DP.calculatedLHA(sighting)
        intermediate = DP.calcInter(sighting, calculatedLHA)
        correctAlt = DP.calculateCorrectAlt(intermediate)
        self.assertEquals(correctAlt, '-52d7.8')

    def test001_002_calculateCorrectDist_Correct(self):
        sighting = {'op':'correct', 'lat':'16d32.3', 'long':'95d41.6', 'altitude':'13d42.3', 'assumedLat':'-53d38.4', 'assumedLong':'74d35.3'}
        calculatedLHA = DP.calculatedLHA(sighting)
        intermediate = DP.calcInter(sighting, calculatedLHA)
        correctAlt = DP.calculateCorrectAlt(intermediate)
        correctedDistance = DP.calculateCorrectedDistance(sighting, correctAlt)
        self.assertEquals(correctedDistance, 3950)

    def test001_002_calculateCorrectAzi_Correct(self):
        sighting = {'op':'correct', 'lat':'16d32.3', 'long':'95d41.6', 'altitude':'13d42.3', 'assumedLat':'-53d38.4', 'assumedLong':'74d35.3'}
        calculatedLHA = DP.calculatedLHA(sighting)
        intermediate = DP.calcInter(sighting, calculatedLHA)
        correctAlt = DP.calculateCorrectAlt(intermediate)
        correctedDistance = DP.calculateCorrectedDistance(sighting, correctAlt)
        correctedAzi = DP.calculateCorrectedAzi(sighting, intermediate)
        self.assertEquals(correctedAzi, '164d42.9')






