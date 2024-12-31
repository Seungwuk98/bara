import argparse 
import subprocess
import os
import time as timeModule

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

    parser.add_argument(
        '--iteration',
        type = int,
        default = 1,
        help = 'iteration count',
    )

    return parser.parse_args()

class Time:
    @staticmethod
    def run(command, *args, **kwargs):
        command = ['time', '-f', "%M", *command]
        before = timeModule.time()
        result = subprocess.run(command, *args, **kwargs)
        time = timeModule.time() - before  
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
    iteration = args.iteration

    output_dir = os.path.join(case_dir, 'output')

    files = [int(f) \
        for f in os.listdir(case_dir) if os.path.isfile(os.path.join(case_dir, f)) and not f.endswith('.a')]
    files.sort()

    for input_file in files:
        print("====running test case: %d====" % input_file)
        input_file = os.path.join(case_dir, str(input_file))
        output_file = os.path.join(output_dir, os.path.basename(input_file))
        solveCommand = [bara_interpreter, bara_file]

        totalExecTime = 0 
        totalMemory = 0
        with open(input_file, 'r') as inf, open(output_file, 'w') as outf:
            time = Time.run(solveCommand, stdin = inf, stdout = outf, stderr = subprocess.PIPE)
      
            if time.result.returncode != 0:
                print(f'Execution Error: {os.path.basename(input_file)} failed')
                print(time.result.stderr.decode('utf-8'))
                exit(time.result.returncode)

            answer_file = input_file + '.a'

            checkerCommand = [checker_file, input_file, output_file, answer_file]
            result = subprocess.run(checkerCommand, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
            if result.returncode != 0:
                print(f'Checker Error: {os.path.basename(input_file)} failed')
                exit(result.returncode)
            print(result.stderr.decode('utf-8').strip())

        totalExecTime += time.time 
        totalMemory += time.memory

        for _ in range(iteration - 1):
            with open(input_file, 'r') as inf, open(output_file, 'w') as outf:
                time = Time.run(solveCommand, stdin = inf, stdout = outf, stderr = subprocess.PIPE)
 
                if time.result.returncode != 0:
                    print(f'Execution Error: {os.path.basename(input_file)} failed')
                    print(time.result.stderr.decode('utf-8'))
                    exit(time.result.returncode)

                totalExecTime += time.time
                totalMemory += time.memory

        print(f'Average Execution Time: {totalExecTime / iteration:.0f}ms')
        print(f'Average Memory Usage: {totalMemory // iteration}KB')


if __name__ == '__main__':
    main()
