const path = require("path");

module.exports = {
  title: "PureFunctor",
  description: "Justin's Home Page and Blog",
  head: [
    ['link', { rel: "icon", type: "image/png", sizes: "32x32", href: "/favicon-32.png"}],
    ['link', { rel: "icon", type: "image/png", sizes: "16x16", href: "/favicon-16.png"}],
  ],
  themeConfig: {
    base: "/site/",
    contributors: false,
    darkMode: true,
    navbar: [
      {
        text: "Blog",
        link: "/blog/",
      },
      {
        text: "Projects",
        link: "/projects/",
      }
    ],
    sidebar: false,
  },
  theme: "@vuepress/theme-default",
}
