# Taken from https://puppet.com/docs/puppet/5.5/lang_classes.html

# /etc/puppetlabs/code/modules/webserver/manifests/params.pp

class webserver::params {
  $packages = $operatingsystem ? {
    /(?i-mx:ubuntu|debian)/        => 'apache2',
    /(?i-mx:centos|fedora|redhat)/ => 'httpd',
  }
  $vhost_dir = $operatingsystem ? {
    /(?i-mx:ubuntu|debian)/        => '/etc/apache2/sites-enabled',
    /(?i-mx:centos|fedora|redhat)/ => '/etc/httpd/conf.d',
  }
}

# /etc/puppetlabs/code/modules/webserver/manifests/init.pp

class webserver(
  String $packages  = $webserver::params::packages,
  String $vhost_dir = $webserver::params::vhost_dir
) inherits webserver::params {

 package { $packages: ensure => present }

 file { 'vhost_dir':
   path   => $vhost_dir,
   ensure => directory,
   mode   => '0750',
   owner  => 'www-data',
   group  => 'root',
 }
}
