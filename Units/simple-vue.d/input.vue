<!-- Taken from https://vuejs.org/v2/guide/single-file-components.html -->

<template>
  <p id="slot">{{ greeting }} World!</p>
</template>

<script>
function makeWord ()
{
	return 'Hello';
}

module.exports = {
  data: function () {
    return {
      greeting: makeWord ()
    }
  }
}
</script>

<style scoped>
p {
  font-size: 2em;
  text-align: center;
}
</style>
