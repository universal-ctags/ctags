#
# Randomly taken from https://github.com/rspec/rspec-core
# https://github.com/rspec/rspec-core/blob/master/LICENSE.md
#

RSpec.describe Order do
  context "with no items" do
    it "behaves one way" do
      # ...
    end
  end

  context "with one item" do
    it "behaves another way" do
      # ...
    end
  end
end

RSpec.describe Calculator do
  describe '#add' do
    it 'returns the sum of its arguments' do
      expect(Calculator.new.add(1, 2)).to eq(3)
    end
  end
end

# Taken from rspec-core/spec/rspec/core/example_group_spec.rb
module RSpec::Core
  RSpec.describe ExampleGroup do
    %w[ describe context let before it it_behaves_like ].each do |method|
      context "when calling `#{method}`, an example group API, from within an example" do
        it "tells the user they are in the wrong scope for that API" do
          ex = nil

          RSpec.describe do
            ex = example { __send__(method, "foo") }
          end.run

          expect(ex).to fail_with(ExampleGroup::WrongScopeError)
        end
      end
    end

    describe Object, "describing nested example_groups", :little_less_nested => 'yep' do
    end
  end
end
