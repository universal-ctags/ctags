# lines are taken from https://ruby.github.io/rake/doc/rakefile_rdoc.html


task :name0

task name1: [:prereq1, :prereq2]

task 'name2' => %w[prereq1 prereq2]

task name3: %w[prereq1 prereq2]

task name4: [:prereq1, :prereq2] do |t|
  # actions (may reference t)
end

file "prog" => ["a.o", "b.o"] do |t|
  sh "cc -o #{t.name} #{t.prerequisites.join(' ')}"
end

directory "testdata/examples/doc"

multitask copy_files: %w[copy_src copy_doc copy_bin] do
  puts "All Copies Complete"
end

# TODO
rule '.o' => ['.c'] do |t|
  sh "cc #{t.source} -c -o #{t.name}"
end

# TODO
rule( /\.o$/ => [
  proc {|task_name| task_name.sub(/\.[^.]+$/, '.c') }
]) do |t|
  sh "cc #{t.source} -c -o #{t.name}"
end

# TODO
import ".depends.mf"

namespace "main" do
  task :build do
    # Build the main program
  end
end

namespace "samples" do
  task :build do
    # Build the sample programs
  end
end

task build: %w[main:build samples:build]

task(:warnings) do
  # Do something
end
