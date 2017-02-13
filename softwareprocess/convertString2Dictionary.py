import urllib

def convertString2Dictionary(inputString = ""):

    readableInput = urllib.unquote(inputString)

    errorDict = {'error':'true'}

    keyDict = {}
    for pair in readableInput.split(','):



        #checks if value has a key AND if a key/value was input
        if '=' not in pair:
            return errorDict

        if pair.split('=').__len__() != 2:
            return errorDict

        key, value = pair.split('=')

        #checks that key starts with a alphabetic character
        if key[0].isdigit():
            return errorDict

        #checks for embedded spaces in key
        if ' ' in key.strip():
            return errorDict

        #checks if key is already in dictionary
        if key.strip() in keyDict:
            return errorDict

        #checks if key has a value
        if value.strip() == "":
            return errorDict

        keyDict[key.strip()] = value.strip()

    return keyDict
