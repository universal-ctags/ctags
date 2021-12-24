# Taken from rspec-core/rspec-core.gemspec

# -*- encoding: utf-8 -*-
$LOAD_PATH.unshift File.expand_path("../lib", __FILE__)
require "rspec/core/version"

Gem::Specification.new do |s|
  s.name        = "rspec-core"
  s.version     = RSpec::Core::Version::STRING
  s.platform    = Gem::Platform::RUBY
  s.license     = "MIT"
  s.authors     = ["Steven Baker", "David Chelimsky", "Chad Humphries", "Myron Marston"]
  s.email       = "rspec@googlegroups.com"
  s.homepage    = "https://github.com/rspec/rspec-core"
  s.summary     = "rspec-core-#{RSpec::Core::Version::STRING}"
  s.description = "BDD for Ruby. RSpec runner and example groups."

  s.metadata = {
    'bug_tracker_uri'   => 'https://github.com/rspec/rspec-core/issues',
    'changelog_uri'     => "https://github.com/rspec/rspec-core/blob/v#{s.version}/Changelog.md",
    'documentation_uri' => 'https://rspec.info/documentation/',
    'mailing_list_uri'  => 'https://groups.google.com/forum/#!forum/rspec',
    'source_code_uri'   => 'https://github.com/rspec/rspec-core',
  }

  s.files            = `git ls-files -- lib/*`.split("\n")
  s.files           += %w[README.md LICENSE.md Changelog.md .yardopts .document]
  s.test_files       = []
  s.bindir           = 'exe'
  s.executables      = `git ls-files -- exe/*`.split("\n").map{ |f| File.basename(f) }
  s.rdoc_options     = ["--charset=UTF-8"]
  s.require_path     = "lib"

  s.required_ruby_version = '>= 1.8.7'

  private_key = File.expand_path('~/.gem/rspec-gem-private_key.pem')
  if File.exist?(private_key)
    s.signing_key = private_key
    s.cert_chain = [File.expand_path('~/.gem/rspec-gem-public_cert.pem')]
  end

  if RSpec::Core::Version::STRING =~ /[a-zA-Z]+/
    # rspec-support is locked to our version when running pre,rc etc
    s.add_runtime_dependency "rspec-support", "= #{RSpec::Core::Version::STRING}"
  else
    # rspec-support must otherwise match our major/minor version
    s.add_runtime_dependency "rspec-support", "~> #{RSpec::Core::Version::STRING.split('.')[0..1].concat(['0']).join('.')}"
  end

  s.add_development_dependency "cucumber", "~> 1.3"
  s.add_development_dependency "minitest", "~> 5.3"
  s.add_development_dependency "aruba",    "~> 0.14.9"

  s.add_development_dependency "coderay",  "~> 1.1.1"

  s.add_development_dependency "mocha",        "~> 0.13.0"
  s.add_development_dependency "rr",           "~> 1.0.4"
  s.add_development_dependency "flexmock",     "~> 0.9.0"
  s.add_development_dependency "thread_order", "~> 1.1.0"
end
