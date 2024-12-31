import argparse
import re
import subprocess

def parse_args():
    parser = argparse.ArgumentParser(
        prog = 'generate',
        description = 'This program generates test cases for algorithm'
    )

    parser.add_argument(
        '--arguments', 
        type = str,
    )

    parser.add_argument(
        '--generators',
        type = str,
        help = 'generator path',
    )

    parser.add_argument(
        '--answer',
        type = str,
        help = 'answer path',
    )

    parser.add_argument(
        '--input-gen-files',
        type = str,
        help = 'output file path',
    )

    parser.add_argument(
        '--output-gen-files',
        type = str,
        help = 'output file path',
    )

    return parser.parse_args()

def main():
    args = parse_args()

    arguments = [arg.split(' ') for arg in re.findall(r'\{([^\}]*)\}', args.arguments)]
    generators = args.generators.split(' ')
    answer = args.answer
    input_files = args.input_gen_files.split(' ')
    output_files = args.output_gen_files.split(' ')

    assert len(arguments) == len(generators) == len(input_files) == len(output_files)

    for argument, generator, input_file, output_file in zip(arguments, generators, input_files, output_files):
        command = [generator, *argument]
        with open(input_file, 'w') as f:
            result = subprocess.run(command, stdout = f)
            if result.returncode != 0:
                print(f'Failed to generate {input_file}')
                return
        
        with open(input_file, 'r') as inf, open(output_file, 'w') as outf:
            result = subprocess.run([answer], stdin = inf, stdout = outf)
            if result.returncode != 0:
                print(f'Failed to generate {output_file}')
                return
        

if __name__ == '__main__':
    main()


