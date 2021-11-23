import { defineClientAppEnhance } from '@vuepress/client'
import Project from './components/Project.vue'

export default defineClientAppEnhance(({ app, router, siteData }) => {
  app.component('Project', Project)
})
