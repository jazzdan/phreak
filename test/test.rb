#!/usr/bin/env ruby

require 'diffy'
require 'highline'

def run_test(test_file)
  output = `cat #{@test_path}/#{test_file} | ./#{@binary_path}`
  expected = File.read("#{@expected_path}/#{test_file}.out")

  diff = Diffy::Diff.new(expected, output)

  if (diff.to_s != '')
    puts 'TEST FAILURE:'
    puts diff
  else
    puts 'Test Passed!'
  end
end

@binary_path = '../phreak.native'
@test_path = 'input'
@expected_path = 'expected_output'
cli = HighLine.new
arg = ARGV[0].to_s

if !File.file? @binary_path
  puts 'No binary found, did you forget to run `make`?'
  exit 1
end

if arg == 'ENABLE_WRITE_MODE'
  answer = cli.ask("Are you sure you be wanting to be overwriting all the tests with the current output of phreak matey? Y/N")
  if answer == 'Y'
    Dir.foreach(@test_path) do |test_file|
      next if test_file.to_s.start_with?('.')

      puts "writing #{@test_path}/#{test_file}"
      `cat #{@test_path}/#{test_file} | ./#{@binary_path} > #{@expected_path}/#{test_file}.out`
    end
    exit
  else
    exit 1
  end
end

if File.file? arg
  # assume we're testing just one testcase
  run_test(arg)
else
  # run all tests
  Dir.foreach(@test_path) do |test_file|
    next if test_file.to_s.start_with?('.')

    run_test(test_file)
  end
end
