/* Taken from https://docs.puppet.com/puppet/5.1/lang_node_definitions.html */
node 'default' {}
node 'www1.example.com' {
  include common
  include apache
  include squid
}

node /^www\d+$/ {
  include common
}

node 'www2.example.com', 'www3.example.com' {
  include common
  include apache, squid
}
