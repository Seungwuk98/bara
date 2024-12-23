import argparse 
import subprocess
import os

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
        '--checker-file',
        type = str,
        help = 'checker file path',
    )

    return parser.parse_args()

def main():
    args = parse_args() 

    case_dir = args.case_dir
    bara_interpreter = args.bara_interpreter
    bara_file = args.bara_file
    checker_file = args.checker_file

    output_dir = os.path.join(case_dir, 'output')

    files = [int(f) \
        for f in os.listdir(case_dir) if os.path.isfile(os.path.join(case_dir, f)) and not f.endswith('.a')]
    files.sort()

    for input_file in files:
        print("running test case: ", input_file)
        input_file = os.path.join(case_dir, str(input_file))
        output_file = os.path.join(output_dir, os.path.basename(input_file))
        command = [bara_interpreter, bara_file]
        result = subprocess.run(command, stdin = open(input_file, 'r'), stdout = open(output_file, 'w'), stderr = subprocess.PIPE)
        if result.returncode != 0:
            print(f'Execution Error: {os.path.basename(input_file)} failed')
            print(result.stderr.decode('utf-8'))
            exit(result.returncode)

        answer_file = input_file + '.a'

        command = [checker_file, input_file, output_file, answer_file]
        result = subprocess.run(command, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
        if result.returncode != 0:
            print(f'Checker Error: {os.path.basename(input_file)} failed')
            exit(result.returncode)
        print(result.stderr.decode('utf-8'))

if __name__ == '__main__':
    main()
