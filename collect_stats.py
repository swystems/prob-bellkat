#!/usr/bin/env python 
import subprocess
import sys
import json
import os
from os import path
from typing import NamedTuple, Optional

import click

MODE = 'cabal'

TESTS = {
    'Protocol (a)': 'Pa',
    'Protocol (a1)': 'Pa1',
    'Protocol (a2)': 'Pag',
    'Protocol 5.1.I (parallel)': 'P5_1_I_parallel',
    'Protocol 5.1.I (ordered)': 'P5_1_I_ordered',
    'Protocol 5.1.II (parallel, two iterations)': 'P5_1_II_parallel',
    'Protocol 5.1.II (parallel, three iterations)': 'P5_1_II_parallel_three',
    'Protocol 5.1.II (ordered, two iterations)': 'P5_1_II_ordered',
    'Protocol 5.1.II (ordered, three iterations)': 'P5_1_II_ordered_three',
    'Protocol 5.1.III (one iteration)': 'P5_1_III_one',
    'Protocol 5.1.III (two iterations)': 'P5_1_III_two',
    'Protocol 5.1.IV (three iterations)': 'P5_1_IV',
    'Protocol 5.3 (Pompili)': 'P5_3_pompili',
    'Protocol 5.3 (Coopmans, outer)': 'P5_3_coopmans_outer',
    'Protocol 5.3 (Coopmans, inner)': 'P5_3_coopmans_inner',
    'Protocol 5.3 (Coopmans, mixed)': 'P5_3_coopmans_mixed',
    }

SLOW_TESTS = {'P5_3_coopmans_mixed'}

class BellKATRunResult(NamedTuple):
    output: str
    stats: dict[str, int]


class RunResult(NamedTuple):
    output: str
    probability: Optional[str]
    stats: dict[str, int]
    bellkat: Optional[BellKATRunResult]

def get_output_filename(example, machine_readable) -> str:
    suffix = 'json' if machine_readable else 'txt'
    return f'output/probabilistic-examples/{example}.{suffix}'

def get_probability_filename(example) -> str:
    return f'output/probabilistic-examples/{example}.prob'

def run_make(target):
    try:
        subprocess.run(['make', f'MODE={MODE}', target], check=True, capture_output=True, encoding='utf-8')
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

def run(example, machine_readable=False) -> RunResult:
    output_file_name = get_output_filename(example, machine_readable)
    run_make(output_file_name)
    with open(output_file_name, 'r', encoding='utf-8') as output_f:
        output = output_f.read().strip()
    stats_file = f'{output_file_name}.stderr'
    with open(stats_file, 'r') as stats_f:
        try:
            stats = dict(eval(stats_f.read().strip()))
        except SyntaxError:
            print(f'{stats_file} is malformed (likely failed run)', file=sys.stderr)
            print(f'...removing output {output_file_name} and retrying...', file=sys.stderr)
            os.remove(output_file_name)
            return run(example, machine_readable)

    probability = run_probability(example) if machine_readable else None
    bellkat = run_bellkat(example) if path.exists(get_bellkat_source_filename(example)) else None

    return RunResult(output=output, probability=probability, stats=stats, bellkat=bellkat)

def run_bellkat(example) -> BellKATRunResult:
    output_file_name = get_bellkat_output_filename(example)
    run_make(output_file_name)

    with open(output_file_name, 'r', encoding='utf-8') as output_f:
        output = output_f.read().strip()
    with open(f'{output_file_name}.stderr', 'r') as stats_f:
        stats = dict(eval(stats_f.read().strip()))

    return BellKATRunResult(output=output, stats=stats)

def get_bellkat_source_filename(example) -> str:
    return f'examples/{example}.hs'

def get_bellkat_output_filename(example) -> str:
    return f'output/examples/{example}.json'

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

def format_bellkat_result_tex(sbps):
    return '{\\small\\begin{gather*}\n' '\\{\n' +\
        ',\\\\\n'.join(format_bps_tex(bps) for bps in sbps) + \
        '\n\\}\n' + '\\end{gather*}}\n' 

def format_dist_tex(dbps):
    return '+'.join(format_bps_tex(bps_prob['value']) + 
                    '\\times' + format_prob_tex(bps_prob['probability']) 
                    for bps_prob in dbps)

def format_bps_tex(bps): 
    return '\\{\\!\\{' + ','.join(format_bp_tex(bp) for bp in bps) + '\\}\\!\\}'

def format_prob_tex(p):
    if isinstance(p, float) or isinstance(p, int): 
        return f'{p:.10f}'
    else:
        numerator = p["numerator"]
        denominator = p["denominator"]
        return '\\frac{' + str(numerator) + '}{' + str(denominator) + '}'

def format_bp_tex(bp):
    l1, l2 = bp
    return f'{l1}\\!\\sim\\!{l2}'

def print_result_tex(test_name, result: RunResult):
    try:
        output = json.loads(result.output)
    except json.decoder.JSONDecodeError as e:
        print(f'Error decoding output for {test_name}: {e}', file=sys.stderr)
        sys.exit(1)
    try:
        probability = json.loads(result.probability)
    except json.decoder.JSONDecodeError as e:
        print(f'Error decoding probability for {test_name}: {e}', file=sys.stderr)
        sys.exit(1)

    print()
    print(r'\subsection{', test_name, '}')
    print()
    print(r'\paragraph{Output}')
    print(format_result_tex(output))
    print(r'\paragraph{Stats}')
    print(format_stats_tex(result.stats, output, probability))
    if result.bellkat is not None:
        bellkat_output = json.loads(result.bellkat.output)
        print(r'\paragraph{BellKAT Output}')
        print(format_bellkat_result_tex(bellkat_output))
        print(r'\paragraph{BellKAT Stats}')
        print(format_stats_tex(result.bellkat.stats, bellkat_output, probability=None))

def format_stats_tex(stats, output, probability=None):
    return '\n'.join([
        r'{\small\begin{center}\begin{tabular}{llll}',
        r'\toprule',
        r'\bf Num Generators & \bf Success probability & \bf Memory (MiB) & \bf Time(s)\\',
        r'\midrule',
        ' '.join([
            str(len(output)), '&', 
            format_probability_range_tex(probability) if probability is not None else '', '&', 
            stats['peak_megabytes_allocated'], '&',
            '{:10.3f}'.format(float(stats['total_wall_seconds'])), r'\\']),
        r'\bottomrule',
        r'\end{tabular}\end{center}}',
        ])

def format_probability_range_tex(p):
    return '$[' +  format_prob_tex(p[0]) + ',' + format_prob_tex(p[1]) + ']$'

def print_tex_header():
    print(r'\documentclass{article}')
    print(r'\usepackage{amsmath}')
    print(r'\usepackage{stmaryrd}')
    print(r'\usepackage{booktabs}')
    print(r'\begin{document}')

def print_tex_footer():
    print(r'\end{document}')

@click.command()
@click.option('--tex', is_flag=True, help='Generate output in TeX format (text, otherwise)')
@click.option('--standalone', is_flag=True, help='Generate standalone TeX output')
@click.option('--fast', is_flag=True, help='Ignore protocols that take long time to analyze')
@click.option('--mode', type=click.Choice(['direct', 'docker', 'cabal']), default='cabal',
              help='Which execution mode to use')
def main(tex, standalone, mode, fast): # pylint: disable=missing-function-docstring
    global MODE # pylint: disable=global-statement

    MODE = mode

    if standalone and not tex:
        print('--standalone can only be used together with --tex', file=sys.stderr)

    if standalone:
        print_tex_header()
    for test_name, test_target in TESTS.items():
        if fast and test_target in SLOW_TESTS:
            continue
        if tex:
            print_result_tex(test_name, run(test_target, machine_readable=True))
        else:
            print_result(test_name, run(test_target))
    if standalone:
        print_tex_footer()

if __name__ == '__main__':
    main()  # pylint: disable=no-value-for-parameter
