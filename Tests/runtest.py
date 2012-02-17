import os.path
import subprocess
import sys

def runRuffian(ruffian_path, test_filename):
    """ Compiles and runs the test file. Returns (stdout, stderr) """
    process = subprocess.Popen([ruffian_path, test_filename],
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    return process.communicate()

def runTest(ruffian_path, test_name):
    """
    Runs the test and compares against the gold results. Returns a
    triplet of success (true/false), the actual results, and the gold results
    """
    
    test_filename = '%s/%s.rf' % (test_name, test_name)
    gold_filename = '%s/gold.txt' % test_name

    # Run the test and grab the output
    (results, stderr) = runRuffian(ruffian_path, test_filename)

    # Check for the gold file. If it does not exist, write the results
    # to it and succeed. If it does exist, compare the results and fail
    # if they don't match.
    if os.path.exists(gold_filename):
        f = open(gold_filename, 'r')
        gold = f.read()
        f.close()
        if results == gold:
            return (True, results, gold)
        else:
            return (False, results, gold)
    else:
        print "Results file doesn't exist, creating it."
        f = open(gold_filename, 'w')
        f.write(results)
        f.close()
        return (True, results, results)

def runAndPrint(ruffian_path, test_name):
    """
    Runs the specified tests. Prints out a message on success or failure.
    Returns True for success and False for failure.
    """
    (success, results, gold) = runTest(ruffian_path, test_name)

    if success:
        print 'Test succeeded.'
    else:
        print 'Test failed.'
        print '=====RESULTS===='
        print results
        print '======GOLD======'
        print gold
        print '================'

    return success

def main(argv):
    if len(argv) != 3:
        sys.stderr.write('Usage: %s [ruffian path] [name of test]\n' % argv[0])
        return 1

    ruffian_path = argv[1]
    test_name = argv[2]

    success = runAndPrint(ruffian_path, test_name)
    if success: return 0
    else: return 1

if __name__ == '__main__':
    main(sys.argv)
