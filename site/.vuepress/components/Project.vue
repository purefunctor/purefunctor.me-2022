<template>
<div class="project-container">
  <img
    class="project-logo"
    alt="PureScript Logo"
    src="https://raw.githubusercontent.com/purescript/logo/master/youtube/channel_icon-800.png"
    height=256
    width=256 />
  <div class="project-description">
    <a v-bind:href="link">
      {{ title }}
    </a>
    <p>
      {{ description }}
    </p>
  </div>
</div>
</template>

<script>
export default {
  props: [
    "title",
    "description",
    "link",
  ],
}
</script>

<style scoped lang="scss">
$project-margin: 16px;
$project-size: 166px - $project-margin;

a {
    font-size: 1.25em;
    margin: $project-margin;
}

p {
    margin: 0 16px 0 16px;
}

.project-container {
    border: 2px solid;
    border-radius: 16px;
    overflow: hidden;
    display: flex;
    height: $project-size;
    width: 100%;
}
.project-logo {
    height: 100%;
    width: $project-size;
}
.project-description {
    flex-grow: 1;
    display: flex;
    flex-direction: column;
}
</style>
