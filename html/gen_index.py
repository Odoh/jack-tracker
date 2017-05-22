rootUrl = 'var rootUrl = "http://localhost:8080";'
jackLocs = 'jackLocsRequest(state.session, whiteNodes, state.prevJackLocs, state.jackLocs);'

with open('turn_index.html', 'w') as turnfile, open('tree_index.html', 'w') as treefile, open('index.html', 'r') as indexfile:
         for line in indexfile:
             if rootUrl in line:
                 turnfile.write(line.replace(rootUrl, 'var rootUrl = "http://whodag.us/jacktracker";'))
                 treefile.write(line.replace(rootUrl, 'var rootUrl = "http://whodag.us/jacktracker/tree";'))
             elif jackLocs in line:
                 turnfile.write(line);
                 treefile.write(line.replace(jackLocs, 'jackCurrLocsRequest(state.session, whiteNodes, state.jackLocs);'))
             else:
                 turnfile.write(line);
                 treefile.write(line);
