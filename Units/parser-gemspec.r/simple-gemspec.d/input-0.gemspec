# Derrived from katello/katello.gemspec
$LOAD_PATH.push File.expand_path("../lib", __FILE__)

# Maintain your gem's version:
require "katello/version"

# Describe your gem and declare its dependencies:
Gem::Specification.new do |gem|
  gem.name        = "katello"
  gem.version     = Katello::VERSION
  gem.authors     = ["N/A"]
  gem.email       = ["katello-devel@redhat.com"]
  gem.homepage    = "http://www.katello.org"
  gem.summary     = ""
  gem.description = "Content and Subscription Management plugin for Foreman"

  gem.files = Dir["{app,vendor,lib,db,ca,config,locale}/**/*"] + ["LICENSE.txt", "README.md"]
  gem.files += Dir["engines/bastion_katello/{app,vendor,lib,config}/**/*"]
  gem.files += Dir["engines/bastion_katello/{README.md,bastion_katello.gemspec}"]

  gem.require_paths = ["lib"]

  # Core Dependencies
  gem.add_dependency "rails"

  # ...
end
