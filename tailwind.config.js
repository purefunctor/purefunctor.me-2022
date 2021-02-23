const plugin = require("tailwindcss/plugin");

module.exports = {
  purge: [
    "pf_app/**/*.purs",           // Inline classes
    "pf_app/css/tailwind.css",  // Composed classes
  ],
  darkMode: false, // or 'media' or 'class'
  theme: {
    fontFamily: {
      'sans': ['Montserrat']
    },
    extend: {
      colors: {
	      "faint": {
	      DEFAULT: "#fffdf2",
	      100: "#e5e2ce"
	      },
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [
    plugin(function({ addUtilities }) {
      const scrollSnap = {
        ".scroll-snap-y-proximity": {
          "scroll-snap-type": "y proximity",
        },
	".scroll-snap-align-start": {
          "scroll-snap-align": "start",
	},
	".no-scroll-snap-type": {
	  "scroll-snap-type": "none",
	},
	".no-scroll-snap-align": {
	  "scroll-snap-align": "none",
	},
      }

      addUtilities(scrollSnap, {
        variants: ["responsive"]
      })
    })
  ],
}
