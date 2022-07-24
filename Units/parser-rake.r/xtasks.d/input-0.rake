# Taken from https://github.com/tmm1/ripper-tags/blob/master/Rakefile
task :default => :test

require 'rake/testtask'
Rake::TestTask.new 'test' do |t|
  t.options = '--use-color' if ENV['GITHUB_ACTIONS']
  t.test_files = FileList['test/test_*.rb']
end
