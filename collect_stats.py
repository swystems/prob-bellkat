#!/usr/bin/env python 
import subprocess
import sys
import click
import json
from typing import NamedTuple, Optional



TESTS = {
    'Protocol (a)': 'Pa',
    'Protocol (a1)': 'Pa1',
    'Protocol (a2)': 'Pag',
    'Protocol 5.1.I (parallel)': 'P5_1_I_parallel',
    'Protocol 5.1.I (ordered)': 'P5_1_I_ordered',
    'Protocol 5.1.II (parallel, two iterations)': 'P5_1_II_parallel',
    'Protocol 5.1.II (parallel, three iterations)': 'P5_1_II_parallel_three',
    'Protocol 5.1.II (ordered)': 'P5_1_II_ordered',
    'Protocol 5.1.III (one iteration)': 'P5_1_III_one',
    'Protocol 5.1.III (two iterations)': 'P5_1_III_two',
    'Protocol 5.1.IV (three iterations)': 'P5_1_IV',
    'Protocol 5.3 (Pompili)': 'P5_3_pompili',
    'Protocol 5.3 (Coopmans, outer)': 'P5_3_coopmans_outer',
    'Protocol 5.3 (Coopmans, inner)': 'P5_3_coopmans_inner',
    'Protocol 5.3 (Coopmans, mixed)': 'P5_3_coopmans_mixed',
    }

class RunResult(NamedTuple):
    output: str
    probability: Optional[str]
    stats: dict[str, int]

def get_output_filename(example, machine_readable) -> str:
    suffix = 'json' if machine_readable else 'txt'
    return f'output/probabilistic-examples/{example}.{suffix}'

def get_probability_filename(example) -> str:
    return f'output/probabilistic-examples/{example}.prob'

def run_make(target):
    try:
        r = subprocess.run(['make', target], check=True, capture_output=True, encoding='utf-8')
    except subprocess.CalledProcessError as e:
        if e.stderr is not None:
            sys.stderr.write(e.stderr)
            sys.exit(1)
        raise

def run_probability(example):
    probability_file_name = get_probability_filename(example)
    run_make(probability_file_name)
    with open(probability_file_name, 'r') as probability_f:
        return probability_f.read().strip()

def run(example, machine_readable=False) -> dict[str, RunResult]:
    output_file_name = get_output_filename(example, machine_readable)
    run_make(output_file_name)
    with open(output_file_name, 'r', encoding='utf-8') as output_f:
        output = output_f.read().strip()
    with open(f'{output_file_name}.stderr', 'r') as stats_f:
        stats = dict(eval(stats_f.read().strip()))

    probability = run_probability(example) if machine_readable else None

    return RunResult(output=output, probability=probability, stats=stats)

def print_result(test_name, result):
    print('=' * 4, test_name, '=' * 4)
    print('-' * 4, 'Output', '-' * 4)
    print(result.output)
    print('-' * 4, 'Stats', '-' * 4)
    print('Memory (MiB):', result.stats['peak_megabytes_allocated'])
    print('Time (s):', result.stats['total_wall_seconds'])

def format_result_tex(cdbps):
    return '{\\small\\begin{gather*}\n' '\\llparenthesis\n' +\
        ',\\\\\n'.join(format_dist_tex(d) for d in cdbps) + \
        '\n\\rrparenthesis\n' + '\\end{gather*}}\n' 

def format_dist_tex(dbps):
    return '+'.join('\\{\\!\\{' + format_bps_tex(bps_prob['value']) + '\\}\\!\\}' + 
                    '\\times' + format_prob_tex(bps_prob['probability']) 
                    for bps_prob in dbps)

def format_bps_tex(bps): 
    return ','.join(format_bp_tex(bp) for bp in bps)

def format_prob_tex(p):
    if isinstance(p, float) or isinstance(p, int): 
        return f'{p:10.4f}'
    else:
        numerator = p["numerator"]
        denominator = p["denominator"]
        return '\\frac{' + str(numerator) + '}{' + str(denominator) + '}'

def format_bp_tex(bp):
    l1, l2 = bp
    return f'{l1}\\!\\sim\\!{l2}'

def print_result_tex(test_name, result):
    output = json.loads(result.output)
    probability = json.loads(result.probability)
    print()
    print(r'\subsection{', test_name, '}')
    print()
    print(r'\paragraph{Output}')
    print(format_result_tex(output))
    print(r'\paragraph{Stats}')
    print()
    print(r'\begin{center}\begin{tabular}{llll}')
    print(r'\toprule')
    print(r'\bf Num Generators & \bf Success probability & \bf Memory (MiB) & \bf Time(s)\\')
    print(r'\midrule')
    print(len(output), '&', format_probability_range_tex(probability), '&', 
          result.stats['peak_megabytes_allocated'], '&',
          '{:10.3f}'.format(float(result.stats['total_wall_seconds'])), r'\\')
    print(r'\bottomrule')
    print(r'\end{tabular}\end{center}')

def format_probability_range_tex(p):
    return '$[' +  format_prob_tex(p[0]) + ',' + format_prob_tex(p[1]) + ']$'

@click.command()
@click.option('--tex', is_flag=True)
def main(tex):
    for test_name, test_target in TESTS.items():
        if tex:
            print_result_tex(test_name, run(test_target, machine_readable=True))
        else:
            print_result(test_name, run(test_target))

if __name__ == '__main__':
    main()
