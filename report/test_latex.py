"""Generate LaTeX for all tests"""

import os

TEST_DIR = '../tests/'
output = ""

for t in sorted(os.listdir(TEST_DIR)):
    if not t.endswith('.bl'):
        continue
    output += '\subsubsection{' + t.replace('_', '\_') + '}\n'
    output += '\\begin{lstlisting}\n'
    with open(TEST_DIR + t) as f:
        output += f.read()
    output += '\\end{lstlisting}\n'

# print(output)
with open('appendix_test.tex', 'w') as f:
    f.write(output)
