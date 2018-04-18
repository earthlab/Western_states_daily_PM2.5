subdirs = []

for x in range(2008, 2015):
    for y in range(1, 13):
        x = str(x)
        month = x + ("%02d" % (y,))
        subdirs.append(month)

print(subdirs)