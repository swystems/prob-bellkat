import json
import subprocess
import os

def run_pa_example(mode, output_file):
    """Run the Pa example in the specified mode and save the output to a file."""
    cmd = f"./new-probabilistic-examples/Pa_run.sh --json {mode} > {output_file}"
    subprocess.run(cmd, shell=True, check=True)

def compare_outputs(file1, file2):
    """Compare the JSON outputs of two files."""
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        data1 = json.load(f1)
        data2 = json.load(f2)
        return data1 == data2

def main():
    # Define the modes and corresponding output files
    modes = ['run', 'execution-trace', 'automaton', 'probability']
    output_files = [f'output_{mode}.json' for mode in modes]

    # Run the Pa example in all modes
    for mode, output_file in zip(modes, output_files):
        run_pa_example(mode, output_file)

    # Compare the outputs
    for i in range(len(modes)):
        for j in range(i + 1, len(modes)):
            if not compare_outputs(output_files[i], output_files[j]):
                print(f"Outputs for {modes[i]} and {modes[j]} do not coincide.")
                return

    print("All outputs coincide.")

    # Clean up the output files
    for output_file in output_files:
        os.remove(output_file)

if __name__ == "__main__":
    main()
