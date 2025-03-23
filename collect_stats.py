#!/usr/bin/env python 
import subprocess
import click
import json
from typing import NamedTuple



TESTS = {
    'Protocol (a)': 'probPa',
    'Protocol (a1)': 'probPa1',
    'Protocol (ag)': 'probPag',
    'Protocol 5.1.I (parallel)': 'probP5_1_I_parallel',
    'Protocol 5.1.I (ordered)': 'probP5_1_I_ordered',
    'Protocol 5.1.II (parallel)': 'probP5_1_II_parallel',
    'Protocol 5.1.II (ordered)': 'probP5_1_II_ordered',
    'Protocol 5.1.III (one iteration)': 'probP5_1_III_one',
    'Protocol 5.1.III (two iterations)': 'probP5_1_III_two',
    'Protocol 5.1.IV (three iterations)': 'probP5_1_IV',
    'Protocol 5.3 (Pompili)': 'probP5_3_pompili',
    'Protocol 5.3 (Coopmans)': 'probP5_3_coopmans',
    }

class RunResult(NamedTuple):
    output: str
    stats: dict[str, int]

def build(test_target):
    subprocess.run(['cabal', '-O2', 'build', test_target], check=True, capture_output=True)

def run(test_target, machine_readable=False) -> dict[str, RunResult]:
    build(test_target)
    args = ['cabal', '-O2', 'run', test_target, '--', 
            '+RTS', '--machine-readable', '-t', '-RTS']
    
    if machine_readable:
        args.append('--json')

    r = subprocess.run(args, capture_output=True, text=True, encoding='utf-8', check=True)
    return RunResult(output=r.stdout.strip(), stats=dict(eval(r.stderr.strip())))

def print_result(test_name, result):
    print('=' * 4, test_name, '=' * 4)
    print('-' * 4, 'Output', '-' * 4)
    print(result.output)
    print('-' * 4, 'Stats', '-' * 4)
    print('Memory (MiB):', result.stats['peak_megabytes_allocated'])
    print('Time (s):', result.stats['total_wall_seconds'])

def format_result_tex(cdbps):
    return '\\begin{gather*}\n' '\\llparenthesis\n' +\
        ',\\\\\n'.join(format_dist_tex(d) for d in cdbps) + \
        '\n\\rrparenthesis\n' + '\\end{gather*}\n' 

def format_dist_tex(dbps):
    return '+'.join('\\{\\!\\{' + format_bps_tex(bps_prob['value']) + '\\}\\!\\}' + 
                    '\\times' + format_prob_tex(bps_prob['probability']) 
                    for bps_prob in dbps)

def format_bps_tex(bps): 
    return ','.join(format_bp_tex(bp) for bp in bps)

def format_prob_tex(p):
    if isinstance(p, float) or isinstance(p, int): 
        return str(p)
    else:
        numerator = p["numerator"]
        denominator = p["denominator"]
        return '\\frac{' + str(numerator) + '}{' + str(denominator) + '}'

def format_bp_tex(bp):
    l1, l2 = bp
    return f'{l1}\\!\\sim\\!{l2}'

def print_result_tex(test_name, result):
    parsed_result = json.loads(result.output.strip())
    print()
    print(r'\subsection{', test_name, '}')
    print()
    print(r'\paragraph{Output}')
    print('Num Generators:', len(parsed_result['output']))
    print()
    print('Success Probability Range:', format_probability_range_tex(parsed_result["probability"]))
    print(format_result_tex(parsed_result['output']))
    print()
    print(r'\paragraph{Stats}')
    print()
    print(r'\begin{tabular}{ll}')
    print(r'\bf Memory (MiB) & \bf Time(s)\\')
    print(result.stats['peak_megabytes_allocated'], '&', result.stats['total_wall_seconds'])
    print(r'\end{tabular}')

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
