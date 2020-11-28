<?xml version="1.0" encoding="utf-8"?>
<root>
<?php foreach ([1, 2, 3] as $id): ?>
  <group id="<?=$id?>">
  <?php foreach (array('a', 'b', 'c') as $type): ?>
    <node type="<?=$type?>">A node of type <?=$type?></node>
  <?php endforeach ?>
  </group>
<?php endforeach ?>
</root>
