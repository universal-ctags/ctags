# https://raw.githubusercontent.com/apache/thrift/master/lib/rb/Rakefile
RSpec::Core::RakeTask.new(:'spec:rcovSingle') do |t|
  t.rspec_opts = ['--color', '--format d']
  t.rcov = true
  t.rcov_opts = ['--exclude', '^spec,/gems/']
end

RSpec::Core::RakeTask.new(:"spec:rcovDouble") do |t|
  t.rspec_opts = ['--color', '--format d']
  t.rcov = true
  t.rcov_opts = ['--exclude', '^spec,/gems/']
end

# The next one may be extracted.
RSpec::Core::RakeTask.new(:) do |t|
  t.rspec_opts = ['--color', '--format d']
  t.rcov = true
  t.rcov_opts = ['--exclude', '^spec,/gems/']
end
