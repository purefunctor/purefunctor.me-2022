<template>
<div class="project-container">
  <img
    class="project-logo"
    alt="PureScript Logo"
    src="https://raw.githubusercontent.com/purescript/logo/master/youtube/channel_icon-800.png"
    height=256
    width=256 />
  <div class="project-description">
    <article>
      <h3>
        {{ title }}
      </h3>
      <p>
        {{ description }}
      </p>
    </article>
  </div>
</div>
</template>

<script>
export default {
  props: [
    "title",
    "description",
  ],
}
</script>

<style scoped lang="scss">
$project-margin: 16px;
$project-size: 166px - $project-margin;

p, h3 {
    margin: $project-margin;
}

.project-container {
    border: 2px solid;
    border-radius: 16px;
    overflow: hidden;
    display: flex;
    height: $project-size;
    /* margin: $project-margin; */
    width: 100%;
}
.project-logo {
    height: 100%;
    width: $project-size;
}
.project-description {
    flex-grow: 1;
}
</style>
