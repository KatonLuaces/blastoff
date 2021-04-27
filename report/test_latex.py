"""Generate LaTeX for all tests"""

import os

TEST_DIR = '../tests/'
output = ""

for t in sorted(os.listdir(TEST_DIR)):
    output += '\\verb='+t+'=:\n\lstinputlisting{../tests/'+t+'}\n'

# print(output)
with open('appendix_test.tex', 'w') as f:
    f.write(output)
