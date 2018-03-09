const _Benchmark = (function() {
    const iterations = 5; // How many times to repeat a benchmark

    function generate_report(mode, program_name, result) {
        return {'iteration': result.iteration,
                'elapsed': _Performance.elapsed(result.start_time, result.end_time),
                'verified': result.verified,
                'expected_result': result.expected_result,
                'actual_result': result.actual_result,
                'program': program_name,
                'mode': mode};
    }

    function print_report(report, string_of_value) {
        var string =
            "\n" +
            "               Mode:  " + report.mode + "\n" +
            "            Program:  " + string_of_value(report.program) + "\n" +
            "          Iteration:  " + report.iteration + "\n" +
            "       Verification:  " + (report.verified ? "SUCCESSFUL" : "FAILED") + "\n" +
            "      Actual result:  " + string_of_value(report.actual_result)  + "\n" +
            "    Expected result:  " + string_of_value(report.expected_result) + "\n" +
            "Execution time (ms):  " + report.elapsed + "\n";
        _print(string);
        return;
    }

    function generate_result(iteration, expected, actual, start, end) {
        return {'iteration': iteration, 'expected_result': expected, 'actual_result': actual, 'start_time': start, 'end_time': end};
    }

    function run(mode, program_name, program, comparer, expected, string_of_value) {
        // Generate results
        const results = [];
        for (let i = 0; i < iterations; i++) {

            const start = _Performance.now();
            const actual = program();
            const end = _Performance.now();

            results[i] = generate_result(i, expected, actual, start, end);
        }

        // Verify and print results
        for (let i = 0; i < iterations; i++) {
            results[i]['verified'] = comparer(results[i]['expected_result'], results[i]['actual_result']);
            print_report(generate_report(mode, program_name, results[i]), string_of_value);
        }

        return;
    }

    return {
        'cps': function(program_name, compare, expected, program, kappa) {
            // Result comparison
            const comparer = function(expected, actual) {
                return compare(expected, actual, _K.identity);
            };
            // Thunked program
            const thunk = function() {
                return program(_K.identity);
            };

            // Run benchmark
            run("CPS", program_name, thunk, comparer, expected, function(v) { return "" + v; });

            // Return unit
            return _K.apply(kappa, {});
        },
        'geniter': function*(program_name, compare, expected, program) {
            // Result comparison
            const comparer = function(expected, actual) {
                return _Toplevel.run(function*() { return yield* compare(expected, actual); });
            };
            // Thunked program
            const thunk = function() {
                return _Toplevel.run(program);
            };

            // Run benchmark
            run("GENITER", program_name, thunk, comparer, expected, function(v) { return "" + v; });

            // Return unit
            return {};
        },
        'cek': function(program_name, compare, expected, program, env) {
            // Result comparison
            const comparer = function(expected, actual) {
                const result = CEK.run(funs, env, IR.Make.computation([], IR.Make.apply(IR.Make.variable(compare.name), [expected, actual])));
                return result.value;
            };

            // Thunked program
            const prog = IR.Make.computation([], IR.Make.apply(IR.Make.variable(program.name), []));
            const thunk = function() {
                return CEK.run(funs, env, prog);
            };

            // Run benchmark
            run("CEK", program_name, thunk, comparer, expected, Value.toString);

            // Return unit
            return IR.Make.unit;
        }
    }
}());



function benchmark() {
    const mode = _mode; // global variable
    const program_name = arguments[0];
    const compare = arguments[1];
    const expected_output = arguments[2];
    const program = arguments[3];

    switch (mode) {
    case "CPS":
        const kappa = arguments[4];
        return _Benchmark.cps(program_name, compare, expected_output, program, kappa);
    case "CEK":
        const env = arguments[4];
        return _Benchmark.cek(program_name, compare, expected_output, program, env);
    case "GENITER":
        return _Benchmark.geniter(program_name, compare, expected_output, program);
    default:
        throw ("Unknown mode " + mode);
    }
}
