import argparse
import os
import subprocess

def parse_args():
    parser = argparse.ArgumentParser(
        prog = 'algorithm_test',
        description = 'This is a test program for algorithm'
    )

    parser.add_argument(
        '--case-dir', 
        type = str,
        help = 'test case directory',
    )

    parser.add_argument(
        '--bara-interpreter',
        type = str,
        help = 'bara interpreter path',
    )

    parser.add_argument(
        '--bara-file',
        type = str,
        help = 'bara file path',
    )

    parser.add_argument(
        '--valgrind',
        type = str,
        help = 'valgrind path',
    )

    parser.add_argument(
        '--gc-thresholds',
        type = str,
        help = 'garbage collection thresholds',
    )

    return parser.parse_args()


def main():
    args = parse_args()

    case_dir = args.case_dir
    bara_interpreter = args.bara_interpreter
    bara_file = args.bara_file
    valgrind = args.valgrind
    gc_thresholds = map(int, args.gc_thresholds.split(' '))

    files = [int(f) \
        for f in os.listdir(case_dir) if os.path.isfile(os.path.join(case_dir, f)) and not f.endswith('.a')]
    files.sort()

    for gc_threshold in gc_thresholds:
        for input_number in files:
            print("====running valgrind test %d - threshold=%d====" % (input_number, gc_threshold))
            input_file = os.path.join(case_dir, str(input_number))
            log_file = os.path.join(case_dir, 'valgrind', f'{input_number}.log')
            command = [valgrind, '--leak-check=full', f'--log-file={log_file}', '--quiet', '--error-exitcode=1', bara_interpreter, bara_file, f'--gc-threshold={gc_threshold}']

            with open(input_file, 'r') as f:
                result = subprocess.run(command, stdin = f, stdout = subprocess.DEVNULL, stderr = subprocess.DEVNULL)
                if result.returncode != 0:
                    print(f'Failed to run valgrind test {input_file}')
                    return
                print(f'Valgrind test {input_number}-{gc_threshold} passed')



if __name__ == '__main__':
    main()
