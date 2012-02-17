import runtest
import os
import sys

def main(argv):
    if len(argv) != 2:
        sys.stderr.write('Usage: %s [ruffian path]' % argv[0])
        return 1

    ruffian_path = argv[1]
    tests = filter(lambda file: os.path.isdir(file), os.listdir('.'))

    success = True
    for test in tests:
        print 'Running test %s...' % test
        success&= runtest.runAndPrint(ruffian_path, test)
        print ''

    if success:
        print 'All tests succeeded.'
        return 0
    else:
        print 'Some tests failed.'
        return 1

if __name__ == '__main__':
    main(sys.argv)
