import fileinput

cnt = 0
cntCmt = 0
cntEmpty = 0

for line in fileinput.input():
    if line[0] is not '%':
        cnt += 1
    else:
        cntCmt += 1

print 'lines:', cnt, 'Cmt lines:', cntCmt, 'total:', cnt + cntCmt
