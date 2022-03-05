<html><head><title>X</title>
<?php // This test input is derived from #2256 submitted by @StephenWall.
  function draw($x) {
      echo "$x";
  }
?>
    <meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">
    <title>My Home Page</title>
    <link rel="stylesheet" href="css/general.css" type="text/css">
    <style type="text/css"></stylE>
  </head>
  <body>
    <h1>stuff</h1>
    <a href="/blah">nowhere</a>
    <?=draw('nowhere')?>
    <a href="/blech">somewhere</a>
    <?=draw('somewhere')?>
  </body>
</html>
<script>
    var f = function (n) {
	return n + 1;
    }
</script>
<style>
.blarg {
    position: relative;
}
</style>
<?php
  function nothing($x) {
      ;
  }
?>