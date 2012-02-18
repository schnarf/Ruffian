import runtest
import os
import sys

def main(argv):
    if len(argv) != 3:
        sys.stderr.write('Usage: %s [include path] [ruffian path]' % argv[0])
        return 1

    include_path = argv[1]
    ruffian_path = argv[2]
    tests = filter(lambda file: os.path.isdir(file), os.listdir('.'))

    success = True
    for test in tests:
        print 'Running test %s...' % test
        success&= runtest.runAndPrint(include_path, ruffian_path, test)
        print ''

    if success:
        print 'All tests succeeded.'
        return 0
    else:
        print 'Some tests failed.'
        return 1

if __name__ == '__main__':
    main(sys.argv)
