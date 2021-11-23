const path = require("path");

module.exports = {
  title: "PureFunctor",
  description: "Justin's Home Page and Blog",
  head: [
    ['link', { rel: "icon", type: "image/png", sizes: "32x32", href: "/favicon-32.png"}],
    ['link', { rel: "icon", type: "image/png", sizes: "16x16", href: "/favicon-16.png"}],
    ['meta', { prefix: 'og: http://ogp.me/ns#', property: 'og:title', content: "PureFunctor" }],
    ['meta', { prefix: 'og: http://ogp.me/ns#', property: 'twitter:title', content: "PureFunctor" }],
    ['meta', { prefix: 'og: http://ogp.me/ns#', property: 'og:type', content: 'website' }],
    ['meta', { prefix: 'og: http://ogp.me/ns#', property: 'og:url', content: 'https://purefunctor.me' }],
    ['meta', { prefix: 'og: http://ogp.me/ns#', property: 'og:description', content: "PureFunctor's Home Page" }],
    ['meta', { prefix: 'og: http://ogp.me/ns#', property: 'og:image', content: 'https://raw.githubusercontent.com/vtrl/.github/main/profile/banner.png' }],
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
