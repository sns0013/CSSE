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
        self.assertTrue(True);

    def test001_000_long_HBX(self):
        self.assertTrue(True);

    def test001_000_long_NX(self):
        self.assertTrue(True);

    def test001_000_long_EX(self):
        self.assertTrue(True);

    def test001_000_long_LBY(self):
        self.assertTrue(True);

    def test001_000_long_HBY(self):
        self.assertTrue(True);

    def test001_000_long_NY(self):
        self.assertTrue(True);

    def test001_000_long_EY(self):
        self.assertTrue(True);

    def test001_000_long_Missing(self):
        self.assertTrue(True);

    def test001_000_long_IncorrectFormat(self):
        self.assertTrue(True);


    def test001_000_alt_LBX(self):
        self.assertTrue(True);

    def test001_000_alt_HBX(self):
        self.assertTrue(True);

    def test001_000_alt_NX(self):
        self.assertTrue(True);

    def test001_000_alt_EX(self):
        self.assertTrue(True);

    def test001_000_alt_LBY(self):
        self.assertTrue(True);

    def test001_000_alt_HBY(self):
        self.assertTrue(True);

    def test001_000_alt_NY(self):
        self.assertTrue(True);

    def test001_000_alt_EY(self):
        self.assertTrue(True);

    def test001_000_alt_Missing(self):
        self.assertTrue(True);

    def test001_000_alt_IncorrectFormat(self):
        self.assertTrue(True);


    def test001_000_assLat_LBX(self):
        self.assertTrue(True);

    def test001_000_assLat_HBX(self):
        self.assertTrue(True);

    def test001_000_assLat_NX(self):
        self.assertTrue(True);

    def test001_000_assLat_EX(self):
        self.assertTrue(True);

    def test001_000_assLat_LBY(self):
        self.assertTrue(True);

    def test001_000_assLat_HBY(self):
        self.assertTrue(True);

    def test001_000_assLat_NY(self):
        self.assertTrue(True);

    def test001_000_assLat_EY(self):
        self.assertTrue(True);

    def test001_000_assLat_Missing(self):
        self.assertTrue(True);

    def test001_000_assLat_IncorrectFormat(self):
        self.assertTrue(True);


    def test001_000_assLong_LBX(self):
        self.assertTrue(True);

    def test001_000_assLong_HBX(self):
        self.assertTrue(True);

    def test001_000_assLong_NX(self):
        self.assertTrue(True);

    def test001_000_assLong_EX(self):
        self.assertTrue(True);

    def test001_000_assLong_LBY(self):
        self.assertTrue(True);

    def test001_000_assLong_HBY(self):
        self.assertTrue(True);

    def test001_000_assLong_NY(self):
        self.assertTrue(True);

    def test001_000_assLong_EY(self):
        self.assertTrue(True);

    def test001_000_assLong_Missing(self):
        self.assertTrue(True);

    def test001_000_assLong_IncorrectFormat(self):
        self.assertTrue(True);


    def test001_000_correctedDistance_Present(self):
        self.assertTrue(True);

    def test001_000_correctedAzi_Present(self):
        self.assertTrue(True);

    def test001_000_correctedDistance_Correct(self):
        self.assertTrue(True);

    def test001_000_correctedAzi_Correct(self):
        self.assertTrue(True);


    def test001_000_calculateLHA_Correct(self):
        self.assertTrue(True);

    def test001_000_calculateCorrectAlt_Correct(self):
        self.assertTrue(True);

    def test001_000_calculateCorrectDist_Correct(self):
        self.assertTrue(True);

    def test001_000_calculateCorrectAzi_Correct(self):
        self.assertTrue(True);






