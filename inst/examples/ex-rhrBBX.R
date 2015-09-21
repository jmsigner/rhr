data(trackS)

rhrBBX(trackS)

# Extends the rangy by 5% to each side
rhrBBX(trackS, 0.05)


# Check that is actually works
bbx <- rhrBBX(trackS)
ext <- apply(bbx, 1, diff)

ext * 1.1
apply(rhrBBX(trackS, 0.05), 1, diff)  
