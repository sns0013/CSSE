StarCatalog = {
    'star':'alpheratz', 'sha':'357d41.7', 'dec':'29d10.9',
    'star':'ankaa', 'sha':'353d14.1', 'dec':'-42d13.4',
    'star':'schedar', 'sha':'349d38.4', 'dec':'56d37.7',
    'star':'diphda', 'sha':'348d54.1', 'dec':'-17d54.1',
    'star':'achernar', 'sha':'335d25.5', 'dec':'-57d09.7',
    'star':'hamal', 'sha':'327d58.7', 'dec':'23d32.3',
    'star':'polaris', 'sha':'316d41.3', 'dec':'89d20.1',
    'star':'akamar', 'sha':'315d16.8', 'dec':'-40d14.8',
    'star':'menkar', 'sha':'314d13.0', 'dec':'4d09.0',
    'star':'mirfak', 'sha':'308d37.4', 'dec':'49d55.1',
    'star':'aldebaran', 'sha':'290d47.1', 'dec':'16d32.3',
    'star':'rigel', 'sha':'281d10.1', 'dec':'-8d11.3',
    'star':'capella', 'sha':'280d31.4', 'dec':'46d00.7',
    'star':'bellatrix', 'sha':'278d29.8', 'dec':'6d21.6',
    'star':'elnath', 'sha':'278d10.1', 'dec':'28d37.1',
    'star':'alnilam', 'sha':'275d44.3', 'dec':'-1d11.8',
    'star':'betelgeuse', 'sha':'270d59', 'dec':'17d24.3',
    'star':'canopus', 'sha':'263d54.8', 'dec':'-52d42.5',
    'star':'sirius', 'sha':'258d31.7', 'dec':'-16d44.3',
    'star':'adara', 'sha':'255d10.8', 'dec':'-28d59.9',
    'star':'procyon', 'sha':'244d57.5', 'dec':'5d10.9',
    'star':'pollux', 'sha':'243d25.2', 'dec':'27d59.0',
    'star':'avior', 'sha':'234d16.6', 'dec':'-59d33.7',
    'star':'suhail', 'sha':'222d50.7', 'dec':'-43d29.8',
    'star':'miaplacidus', 'sha':'221d38.4', 'dec':'-69d46.9',
    'star':'alphard', 'sha':'217d54.1', 'dec':'-8d43.8',
    'star':'regulus', 'sha':'207d41.4', 'dec':'11d53.2',
    'star':'dubhe', 'sha':'193d49.4', 'dec':'61d39.5',
    'star':'denebola', 'sha':'182d31.8', 'dec':'14d28.9',
    'star':'gienah', 'sha':'175d50.4', 'dec':'-17d37.7',
    'star':'acrux', 'sha':'173d07.2', 'dec':'-63d10.9',
    'star':'gacrux', 'sha':'171d58.8', 'dec':'-57d11.9',
    'star':'alioth', 'sha':'166d19.4', 'dec':'55d52.1',
    'star':'spica', 'sha':'158d29.5', 'dec':'-11d14.5',
    'star':'alcaid', 'sha':'152d57.8', 'dec':'49d13.8',
    'star':'hadar', 'sha':'148d45.5', 'dec':'-60d26.6',
    'star':'menkent', 'sha':'148d05.6', 'dec':'-36d26.6',
    'star':'arcturus', 'sha':'145d54.2', 'dec':'19d06.2',
    'star':'rigil kent.', 'sha':'139d49.6', 'dec':'-60d53.6',
    'star':'zubenelg.', 'sha':'137d03.7', 'dec':'-16d06.3',
    'star':'kochab', 'sha':'137d21.0', 'dec':'74d05.2',
    'star':'alphecca', 'sha':'126d09.9', 'dec':'26d39.7',
    'star':'antares', 'sha':'112d24.4', 'dec':'-26d27.8',
    'star':'atria', 'sha':'107d25.2', 'dec':'-69d03.0',
    'star':'sabik', 'sha':'102d10.9', 'dec':'-15d44.4',
    'star':'shaula', 'sha':'96d20.0', 'dec':'-37d06.6',
    'star':'rasalhague', 'sha':'96d05.2', 'dec':'12d33.1',
    'star':'etamin', 'sha':'90d45.9', 'dec':'51d29.3',
    'star':'kaus aust.', 'sha':'83d41.9', 'dec':'-34d22.4',
    'star':'vega', 'sha':'80d38.2', 'dec':'38d48.1',
    'star':'nunki', 'sha':'75d56.6', 'dec':'-26d16.4',
    'star':'altair', 'sha':'62d06.9', 'dec':'8d54.8',
    'star':'peacock', 'sha':'53d17.2', 'dec':'-56d41.0',
    'star':'deneb', 'sha':'49d30.7', 'dec':'45d20.5',
    'star':'enif', 'sha':'33d45.7', 'dec':'9d57.0',
    'star':'alnair', 'sha':'27d42.0', 'dec':'-46d53.1',
    'star':'fomalhaut', 'sha':'15d22.4', 'dec':'-29d32.3',
    'star':'scheat', 'sha':'13d51.8', 'dec':'28d10.3',
    'star':'markab', 'sha':'13d36.7', 'dec':'15d17.6' }

def getSHA(starName):
    for starName in StarCatalog:
        return StarCatalog['sha']
    else:
        return 'error'

def getDEC(starName):
    if starName.lower() in StarCatalog:

    else:
        return 'error'
