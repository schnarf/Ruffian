import os.path
import subprocess
import sys

def runRuffian(include_path, ruffian_path, test_filename):
    """ Compiles and runs the test file. Returns (stdout, stderr) """
    process = subprocess.Popen([ruffian_path, include_path, test_filename],
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    (stdout, stderr) = process.communicate()
    return (stdout, stderr, process.returncode)

def runTest(include_path, ruffian_path, test_name):
    """
    Runs the test and compares against the gold results. Returns a
    triplet of success (true/false), the actual results, and the gold results
    """
    
    test_filename = '%s/%s.rf' % (test_name, test_name)
    gold_filename = '%s/gold.txt' % test_name

    # Run the test and grab the output
    (results, stderr, returncode) = runRuffian(include_path,
                                               ruffian_path,
                                               test_filename)

    # Format the results: The first line of the results file is whether
    # compilation succeeded or not. The rest is stdout.
    if returncode == 0:
        returncode_str = 'Success'
    elif returncode == 1:
        returncode_str = 'Failure'
    else:
        assert(False)
        returncode_str = 'Unknown return code'
    results = '%s\n%s' % (returncode_str, results)

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

def runAndPrint(include_path, ruffian_path, test_name):
    """
    Runs the specified tests. Prints out a message on success or failure.
    Returns True for success and False for failure.
    """
    (success, results, gold) = runTest(include_path, ruffian_path, test_name)

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
    if len(argv) != 4:
        sys.stderr.write('Usage: %s [include path] [ruffian path] [name of test]\n' % argv[0])
        return 1

    include_path = argv[1]
    ruffian_path = argv[2]
    test_name = argv[3]

    success = runAndPrint(include_path, ruffian_path, test_name)
    if success: return 0
    else: return 1

if __name__ == '__main__':
    main(sys.argv)
