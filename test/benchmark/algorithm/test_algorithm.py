import argparse 
import subprocess
import os
import resource
from enum import Enum, unique

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

class Time:
    @unique
    class Row(Enum):
        TIME = 0

    @staticmethod
    def run(command, *args, **kwargs):
        command = ['time', '-f', "%M", *command]
        before = resource.getrusage(resource.RUSAGE_CHILDREN)
        result = subprocess.run(command, *args, **kwargs)
        time = resource.getrusage(resource.RUSAGE_CHILDREN).ru_utime - before.ru_utime
        if (result.returncode != 0):
            timeParser = Time(time, None, result)
        else:
            timeParser = Time(time, result.stderr.decode('utf-8'),  result)
        return timeParser

    def __init__(self, time, time_str, result):
        self.time = time * 1000
        self.memory = int(time_str.strip()) if time_str else 0
        self.result = result

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
        print("====running test case: %d====" % input_file)
        input_file = os.path.join(case_dir, str(input_file))
        output_file = os.path.join(output_dir, os.path.basename(input_file))
        command = [bara_interpreter, bara_file]
        time = Time.run(command, stdin = open(input_file, 'r'), stdout = open(output_file, 'w'), stderr = subprocess.PIPE)
      
        if time.result.returncode != 0:
            print(f'Execution Error: {os.path.basename(input_file)} failed')
            print(time.result.stderr.decode('utf-8'))
            exit(time.result.returncode)

        answer_file = input_file + '.a'

        command = [checker_file, input_file, output_file, answer_file]
        result = subprocess.run(command, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
        if result.returncode != 0:
            print(f'Checker Error: {os.path.basename(input_file)} failed')
            exit(result.returncode)
        print(result.stderr.decode('utf-8').strip())
        print(f'Execution Time: {time.time:.0f}ms')
        print(f'Memory Usage: {int(time.memory)}KB')

if __name__ == '__main__':
    main()
