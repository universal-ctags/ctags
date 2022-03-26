# Based on from puppet/tasks/benchmark.rake
namespace :benchmark do
  def generate_scenario_tasks(location, name)
    desc File.read(File.join(location, 'description'))
    task name => "#{name}:run"
    # Load a BenchmarkerTask to handle config of the benchmark
    task_handler_file = File.expand_path(File.join(location, 'benchmarker_task.rb'))
    if File.exist?(task_handler_file)
      require task_handler_file
      run_args = BenchmarkerTask.run_args
    else
      run_args = []
    end

    namespace name do
      task :setup do
        ENV['ITERATIONS'] ||= '10'
        ENV['SIZE'] ||= '100'
        ENV['TARGET'] ||= Dir.mktmpdir(name)
        ENV['TARGET'] = File.expand_path(ENV['TARGET'])

        mkdir_p(ENV['TARGET'])

        require File.expand_path(File.join(location, 'benchmarker.rb'))

        @benchmark = Benchmarker.new(ENV['TARGET'], ENV['SIZE'].to_i)
      end

      task :generate => :setup do
        @benchmark.generate
        @benchmark.setup
      end
    end
  end
end
