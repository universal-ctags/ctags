puppetManifest parser is a test bench parser for evaluating
the quality and usefulness mtable regex metaparser.

Input for test cases started from puppet- under this directory are
taken from puppet-3.8.7/spec/fixtures/unit/parser/lexer/*.pp.

## Test Input Verification

### Requirements

Install puppet-agent to your platform:
[Linux](https://puppet.com/docs/puppet/5.3/install_linux.html),
[Windows](https://puppet.com/docs/puppet/5.3/install_windows.html) or
[macOS](https://puppet.com/docs/puppet/5.3/install_osx.html)

### Execute Verification

From the `<repo_root>/Units/parser-puppetManifest.r` directory execute the
following:

```
find . -name "*.pp" | xargs -n 1 -I@ bash -c " echo @ &&  /opt/puppetlabs/bin/puppet apply --noop @"
```

In that command we find all puppet files in the unit test directory. Then we
run `puppet apply --noop` on each of them, essentially checking the input
file.  The return value of the above command will be non-zero if any puppet
run fails.


