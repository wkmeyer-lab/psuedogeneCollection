# psuedogeneCollection

Command line argument to download lossum data from  https://genome.senckenberg.de/download/TOGA/ : wget --recursive      --no-parent      --no-check-certificate      --accept "*.tsv.gz"    --execute robots=off      https://genome.senckenberg.de/download/TOGA/

Project contains a script used to access losssum data and from that create a .tsv suitable for input for baystraits containing GENE ID on COL 1, SPECIES on ROW 1 and GENE PRESENCE on all other cells. 
