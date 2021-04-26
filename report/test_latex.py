"""Generate LaTeX for all tests"""

import os

TEST_DIR = '../tests/'
tests = sorted(os.listdir(TEST_DIR))

for t in tests:
    if not t.endswith('.bl'):
        continue
    with open(TEST_DIR + t) as f:
        contents = f.read()
    print('\subsubsection{' + t + '}')
    print('\\begin{lstlisting}')
    print(contents)
    print('\\end{lstlisting}')
