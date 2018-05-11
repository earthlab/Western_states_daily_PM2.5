# function to generate .prj file information using spatialreference.org
def getWKT_PRJ (srid, code):
    import urllib
    # access projection information
    wkt = urllib.urlopen("http://spatialreference.org/ref/{0}/{1}/esriwkt/".format(srid, code))
    # remove spaces between characters
    remove_spaces = wkt.read().replace(" ","")
    # place all the text on one line
    output = remove_spaces.replace("\n", "")
    return output

