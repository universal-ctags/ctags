<!DOCTYPE html>
<head>
<title><?= htmlspecialchars($title) ?></title>
<script type="text/javascript">
document.onload = function() {
  window.title += ' (fully loaded!)';
}
</script>
</head>
<body>

<h1>Home page</h1>

<?php
for ($entries as $entry) {
  ?><h2><?= htmlspecialchars($entry['title']) ?></h2>
  <p><?= htmlspecialchars($entry['body']) ?></p>
  <div class="author">By <?= htmlspecialchars($entry['author']) ?></div>
<?php
}

function paginate() {
  return $some_magic;
}

paginate($entries);
?>
</body>
