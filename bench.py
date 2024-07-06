# This test script compiles the benchmarking suites under three schemes:
# 1. gcc -O1
# 2. bin/c0c -O1 -ex86_64
# 3. bin/c0c -ewasm
# And compare the runtime between them.
#
# Usage: python3 bench.py <test_name>
# test_name: the test file of the test suite to be benchmarked.

import subprocess
import os
import numpy as np
import time
import matplotlib.pyplot as plt
from wasmtime import Linker, Module, Store

is_linux = True

temp_dir = 'bench'

# tmp dir to run test artifacts
os.makedirs('bench', exist_ok=True)

# build c0c by make
subprocess.run(['make'])

def run_gcc_O0(test_name):
    gcc_cmd = f'gcc -Wno-everything -O0 -o {temp_dir}/{test_name}_gcc ../tests/bench/unsafe/{test_name}.c'
    subprocess.run(gcc_cmd.split())

    # run the compiled binary
    clock_start = time.perf_counter_ns()
    subprocess.run([f'{temp_dir}/{test_name}_gcc'])
    elapsed_time = time.perf_counter_ns() - clock_start

    return elapsed_time

def run_gcc(test_name):
    gcc_cmd = f'gcc -Wno-everything -O1 -o {temp_dir}/{test_name}_gcc ../tests/bench/unsafe/{test_name}.c'
    subprocess.run(gcc_cmd.split())

    # run the compiled binary
    clock_start = time.perf_counter_ns()
    subprocess.run([f'{temp_dir}/{test_name}_gcc'])
    elapsed_time = time.perf_counter_ns() - clock_start

    return elapsed_time
    
def run_c0c_O0(test_name):
    # run c0c -O1 -ex86_64
    c0c_cmd = f'../bin/c0c -O0 --unsafe -ex86-64 ../tests/bench/{test_name}.l4'
    env = os.environ.copy()
    env.update({'NORENAMEMAIN': '1'})
    subprocess.run(c0c_cmd.split(), env=env)

    # link the generated assembly
    asm_file = f'../tests/bench/{test_name}.l4.s'
    if is_linux:
        link_cmd = f'gcc -Wno-everything -m64 -no-pie -o {temp_dir}/{test_name}_c0c {asm_file}'
    else:
        link_cmd = f'arch x86_64 gcc -Wno-everything -m64 -no-pie -o {temp_dir}/{test_name}_c0c {asm_file}'
    subprocess.run(link_cmd.split())

    start_time = time.perf_counter_ns()
    subprocess.run([f'{temp_dir}/{test_name}_c0c'])
    elapsed_time = time.perf_counter_ns() - start_time

    return elapsed_time



def run_c0c(test_name):
    # run c0c -O1 -ex86_64
    c0c_cmd = f'../bin/c0c -O1 --unsafe -ex86-64 ../tests/bench/{test_name}.l4'
    env = os.environ.copy()
    env.update({'NORENAMEMAIN': '1'})
    subprocess.run(c0c_cmd.split(), env=env)

    # link the generated assembly
    asm_file = f'../tests/bench/{test_name}.l4.s'
    if is_linux:
        link_cmd = f'gcc -Wno-everything -m64 -no-pie -o {temp_dir}/{test_name}_c0c {asm_file}'
    else:
        link_cmd = f'arch x86_64 gcc -Wno-everything -m64 -no-pie -o {temp_dir}/{test_name}_c0c {asm_file}'
    subprocess.run(link_cmd.split())

    start_time = time.perf_counter_ns()
    subprocess.run([f'{temp_dir}/{test_name}_c0c'])
    elapsed_time = time.perf_counter_ns() - start_time

    return elapsed_time

def run_wasm(test_name):
    alloc_wat_path = "../alloc.wat"
    c0c_cmd = f'../bin/c0c -ewasm ../tests/bench/{test_name}.l4'
    subprocess.run(c0c_cmd.split())

    src_wat = f'../tests/bench/{test_name}.l4.wat'

    store = Store()
    linker = Linker(store.engine)

    # Load and instantiate the allocator module
    with open(alloc_wat_path, "rb") as file:
        allocator_module = Module(store.engine, file.read())

    allocator_instance = linker.instantiate(store, allocator_module)
    linker.define(store, "alloc", "memory", allocator_instance.exports(store)["memory"])
    linker.define(store, "alloc", "allocate", allocator_instance.exports(store)["allocate"])

    with open(src_wat, "rb") as file:
        consumer_module = Module(store.engine, file.read())
    consumer_instance = linker.instantiate(store, consumer_module)
    run = consumer_instance.exports(store)["main"]

    start_time = time.perf_counter_ns()
    run(store)
    elapsed_time = time.perf_counter_ns() - start_time

    return elapsed_time

def cleanup():
    # remove the binary
    import glob
    subprocess.run(['rm', '-rf', f'{temp_dir}'])
    # remove the asm
    d = '../tests/bench/'
    wcs = ['*.s', '*.abs', '*.wat']
    for wc in wcs:
        g = glob.glob(os.path.join(d, wc))
        for f in g:
            os.remove(f)

def bench_one(test_name):
    print(f'Benchmarking {test_name}...')
    t_gcc = run_gcc(test_name)
    t_gcc_O0 = run_gcc_O0(test_name)
    t_c0c = run_c0c(test_name)
    t_c0c_O0 = run_c0c_O0(test_name)
    t_wasm = run_wasm(test_name)

    return t_gcc, t_gcc_O0, t_c0c, t_c0c_O0, t_wasm

# get_all_tests under ../tests/bench
def get_all_tests():
    tests = []
    for file in os.listdir('../tests/bench'):
        if file.endswith('.l4'):
            tests.append(file[:-3])
    return tests

alltests = get_all_tests()

def run_all():
    # run all the tests and draw a table
    result = {}
    buf = []
    buf.append('Test\t\tgcc -O1\tgcc -O0\tc0c -u1 (x86_64)\tc0c -u0 (x86_64)\twasm')
    for test in alltests:
        t_gcc, t_gcc_O0, t_c0c, t_c0c_O0, t_wasm = bench_one(test)
        buf.append(f'{test}\t\t{t_gcc}\t{t_gcc_O0}\t{t_c0c}\t{t_c0c_O0}\t{t_wasm}')
        result[test] = (t_gcc, t_gcc_O0,t_c0c, t_c0c_O0, t_wasm)

    tests = list(result.keys())
    gcc_times = [result[test][0] for test in tests]
    gcc_O0_times = [result[test][1] for test in tests]
    c0c_times = [result[test][2] for test in tests]
    c0c_O0_times = [result[test][3] for test in tests]
    wasm_times = [result[test][4] for test in tests]   

    gcc_avg = sum(gcc_times) / len(gcc_times)
    gcc_O0_avg = sum(gcc_O0_times) / len(gcc_O0_times)
    c0c_avg = sum(c0c_times) / len(c0c_times)
    c0c_O0_avg = sum(c0c_O0_times) / len(c0c_O0_times)
    wasm_avg = sum(wasm_times) / len(wasm_times)

    buf.append(f"avg: gcc(-O1)={gcc_avg}, gcc(-O0)={gcc_O0_avg}, c0c(-O1)={c0c_avg}, c0c(-O0)={c0c_O0_avg}, wasm={wasm_avg}")
 
    return buf, result

def draw_plot(result):
    # Extracting test names and corresponding times for gcc, c0c, wasm
    tests = list(result.keys())
    gcc_times = [result[test][0] for test in tests]
    gcc_O0_times = [result[test][1] for test in tests]
    c0c_times = [result[test][2] for test in tests]
    c0c_O0_times = [result[test][3] for test in tests]
    wasm_times = [result[test][4] for test in tests]
    
    # Set up the figure and axes
    fig, ax = plt.subplots(figsize=(15, 7))
    
    # Defining the x locations for the groups
    x = np.arange(len(tests))  # label locations
    width = 0.16  # width of the bars

    # Plotting data
    ax.bar(x - 2 * width, gcc_times, width, label='gcc -O1')
    ax.bar(x - width, gcc_O0_times, width, label='gcc -O0')
    ax.bar(x, c0c_O0_times, width, label='c0c -u0(x86_64)')
    ax.bar(x + width, c0c_times, width, label='c0c -u1(x86_64)')
    ax.bar(x + 2 * width, wasm_times, width, label='c0c(wasm)')

    # Add some text for labels, title, and custom x-axis tick labels, etc.
    ax.set_ylabel('Times')
    ax.set_title('Test results for gcc, c0c, and wasm')
    ax.set_xticks(x)
    ax.set_xticklabels(tests, rotation=60, ha='right')  # Adjust rotation and alignment
    ax.legend()

    plt.tight_layout()

    # Show the plot
    plt.savefig('eachtest.png')

def draw_plot_2(result):
    # clear the previous plot
    plt.clf()

    # draw a plot for average runtime
    # Extracting test names and corresponding times for gcc, c0c, wasmtime
    tests = list(result.keys())
    gcc_times = [result[test][0] for test in tests]
    gcc_O0_times = [result[test][1] for test in tests]
    c0c_times = [result[test][2] for test in tests]
    c0c_O0_times = [result[test][3] for test in tests]
    wasm_times = [result[test][4] for test in tests]   

    gcc_avg = sum(gcc_times) / len(gcc_times)
    gcc_O0_avg = sum(gcc_O0_times) / len(gcc_O0_times)
    c0c_avg = sum(c0c_times) / len(c0c_times)
    c0c_O0_avg = sum(c0c_O0_times) / len(c0c_O0_times)
    wasm_avg = sum(wasm_times) / len(wasm_times)

    # draw the average bars
    compilers = ['gcc -O1', 'gcc -O0', 'c0c -u0(x86_64)', 'c0c -u1(x86_64)', 'c0c(wasm)']
    averages = [gcc_avg, gcc_O0_avg, c0c_O0_avg, c0c_avg, wasm_avg]

    # Plotting the averages
    plt.figure(figsize=(10, 6))

    plt.bar(compilers, averages)
    plt.title('Average Runtime Comparison')
    plt.xlabel('Cases')
    plt.ylabel('Average Runtime (nanoseconds)')

    plt.savefig('average.png')



buf, result = run_all()
for line in buf:
    print(line)

draw_plot(result)
draw_plot_2(result)

cleanup()

