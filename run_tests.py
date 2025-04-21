#!/usr/bin/env python3
import os
import subprocess


COMPILER_PATH = "./target/debug/amulet"  # Replace with the actual path to your compiler
CLANG_PATH = "clang"  # Replace with the actual path to your clang installation
TESTS_DIR = "unittests"  # Directory containing test files


def run_test(test_file: str) -> bool:
    """
    Run a test file and return True if it passes, False otherwise.
    """

    # Run the command and capture the output
    compiler_result = subprocess.run(
        [COMPILER_PATH, test_file, "--nostdlib"], capture_output=True, text=True
    )
    if compiler_result.returncode != 0:
        print(f"\033[31;1merror: \033[0mfailed to compile {test_file}")
        print(compiler_result.stderr)
        return False

    clang_result = subprocess.run(
        [
            CLANG_PATH,
            test_file.replace(".am", ".o"),
            "-o",
            test_file.replace(".am", ""),
        ],
        capture_output=True,
        text=True,
    )
    if clang_result.returncode != 0:
        print(f"\033[31;1merror: \033[0mfailed to {test_file.replace('.am', '')}")
        print(clang_result.stderr)
        return False

    executable_result = subprocess.run(
        [f"./{test_file.replace('.am', '')}"], capture_output=True, text=True
    )

    with open(test_file, "r") as f:
        contents = f.read().splitlines()

    expected_type_line = contents[0]
    expected_type_index = expected_type_line.find("/// type=")
    if expected_type_index == -1:
        print(f"\033[31;1merror: \033[0mno expected type found in {test_file}")
        return False

    expected_type = expected_type_line[expected_type_index + len("/// type=") :].strip()
    if expected_type not in ["pass", "fail"]:
        print(
            f"\033[31;1merror: \033[0minvalid expected type `{expected_type}` in {test_file}"
        )
        return False

    expected_exit_code_line = contents[1]
    expected_exit_code_index = expected_exit_code_line.find("/// exitcode=")
    if expected_exit_code_index == -1:
        print(f"\033[31;1merror: \033[0mno expected exit code found in {test_file}")
        return False

    expected_exit_code = expected_exit_code_line[
        expected_exit_code_index + len("/// exitcode=") :
    ].strip()
    # if not expected_exit_code.isdigit():
    #     print(f"\033[31;1merror: \033[0minvalid expected exit code `{expected_exit_code}` in {test_file}")
    #     return False
    if int(expected_exit_code) != executable_result.returncode:
        print(
            f"\033[31;1merror: \033[0mexpected exit code {expected_exit_code} but got {executable_result.returncode} in {test_file}"
        )
        print(executable_result.stderr)
        return False

    expected_output_line = contents[2]
    expected_output_index = expected_output_line.find("/// output=")
    if expected_output_index == -1:
        print(f"\033[31;1merror: \033[0mno expected output found in {test_file}")
        return False
    expected_output = expected_output_line[
        expected_output_index + len("/// output=") :
    ].strip()
    if expected_type == "pass":
        if expected_output != executable_result.stdout.replace("\n", "\\n"):
            print(
                f"\033[31;1merror: \033[0mexpected output `{expected_output}` but got `{executable_result.stdout}` in {test_file}"
            )
            return False
    elif expected_type == "fail":
        if expected_output != executable_result.stderr.replace("\n", "\\n"):
            print(
                f"\033[31;1merror: \033[0mexpected output `{expected_output}` but got `{executable_result.stderr}` in {test_file}"
            )
            return False

    os.remove(test_file.replace(".am", ""))
    os.remove(test_file.replace(".am", ".o"))

    return True


def main():
    """
    Main function to run all tests in the specified directory.
    """

    # Get a list of all test files in the directory
    test_files = [
        os.path.join(root, file)
        for root, _, files in os.walk(TESTS_DIR)
        for file in files
        if file.endswith(".am")
    ]

    passing_tests = []
    failing_tests = []

    # Run each test file and print the result
    for test_file in test_files:
        test_path = test_file
        passed = run_test(test_path)
        if passed:
            print(f"\033[32;1mTest {test_file} passed.\033[0m")
            passing_tests.append(test_file)
        else:
            print(f"\033[31;1mTest {test_file} failed.\033[0m")
            failing_tests.append(test_file)

    # Print a summary of the results
    print("\nSummary:")
    print(f"\033[32;1m{len(passing_tests)} tests passed.\033[0m")
    print(f"\033[31;1m{len(failing_tests)} tests failed.\033[0m")


# This script is designed to run tests for the Amulet compiler.
if __name__ == "__main__":
    main()
