module.exports = {
  purge: [
    "./output/**/*.js",           // Inline classes
    "./pf_app/css/tailwind.css",  // Composed classes
  ],
  darkMode: false, // or 'media' or 'class'
  theme: {
    fontFamily: {
      'sans': ['Montserrat']
    },
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
